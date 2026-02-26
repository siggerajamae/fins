;;; fins.el --- Fins Interactively Narrows Searches -*- lexical-binding: t; -*-

;; Author: Sigge Rajamäe
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: matching, tools
;; URL: https://github.com/siggerajamae/fins

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Iterative, interactive narrowing of file and grep matches.

;;; Code:

(require 'cl-lib)

(defgroup fins nil
  "Fins Greps And Interactively Narrows."
  :group 'matching)

(defcustom fins-grep-files-command "rg --files-with-matches --smart-case --color=never"
  "Command used for finding files whose contents match a term."
  :type 'string
  :group 'fins)

(defcustom fins-grep-lines-command "rg --json --smart-case"
  "Command used for grepping lines from files."
  :type 'string
  :group 'fins)

(defcustom fins-highlight-faces '(consult-highlight-match match)
  "Faces recognized as match highlights when parsing candidates."
  :type '(repeat face)
  :group 'fins)

(defcustom fins-use-consult-preview t
  "Whether to use consult preview for the entry at point."
  :type 'boolean
  :group 'fins)

(defface fins-file
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight files in Fins buffers.")

(defface fins-line-number
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight line numbers in Fins buffers.")

(defface fins-mark
  '((t :inherit warning))
  "Face used to highlight marked entries in Fins buffers.")

(defvar-keymap fins-mode-map
  :doc "Keymap for Fins mode."
  :parent special-mode-map
  "* n" #'fins-mark-by-name
  "* c" #'fins-mark-by-content
  "* l" #'fins-mark-by-lines
  "t"   #'fins-toggle-marks
  "n"   #'next-line
  "p"   #'previous-line
  "m"   #'fins-mark
  "u"   #'fins-unmark
  "U"   #'fins-unmark-all
  "e"   #'fins-expand
  "w"   #'fins-collapse
  "d"   #'fins-delete-marked
  "RET" #'fins-visit)

(define-derived-mode fins-mode special-mode "Fins"
  "Major mode for iterative narrowing of file and grep matches."
  (setq-local fins-entries nil))

(cl-defstruct fins-entry
  file
  line
  column
  content
  marked)

(defmacro fins--with-entry (entry &rest body)
  "Bind the slots of ENTRY to variables and execute BODY.
Binds `file', `line', `column', `content', and `marked'.
Supports `setf' on the bindings."
  (declare (indent 1))
  (let ((sym (gensym "entry")))
    `(let ((,sym ,entry))
       (cl-symbol-macrolet ((file (fins-entry-file ,sym))
                            (line (fins-entry-line ,sym))
                            (column (fins-entry-column ,sym))
                            (content (fins-entry-content ,sym))
                            (marked (fins-entry-marked ,sym)))
         ,@body))))

(defvar-local fins-entries nil
  "List of `fins-entry' entries in the current buffer.")

(defconst fins--grep-column-regexp
  (concat "\\`"          ; beginning of string
          "\\([^:]+\\)"  ; capture file
          ":"            ; separator
          "\\([0-9]+\\)" ; capture line number
          ":"            ; separator
          "\\([0-9]+\\)" ; capture column number
          ":"            ; separator
          "\\(.*\\)"     ; capture content
          "\\'")         ; end of string
  "Regexp matching grep output with column numbers.")

(defconst fins--grep-regexp
  (concat "\\`"          ; beginning of string
          "\\([^:]+\\)"  ; capture file
          ":"            ; separator
          "\\([0-9]+\\)" ; capture line number
          ":"            ; separator
          "\\(.*\\)"     ; capture content
          "\\'")         ; end of string
  "Regexp matching standard grep output format.")

(defconst fins--file-regexp
  (concat "\\`"          ; beginning of string
          "[^\0\n]+"     ; one or more chars, no null bytes or newlines
          "\\'")         ; end of string
  "Regexp matching a valid file name.")

(defun fins--parse-json (obj &optional term)
  "Parse a JSON match OBJ into a `fins-entry'.
When TERM is non-nil, highlight all matches of TERM in content."
  (let* ((data (gethash "data" obj))
         (submatches (gethash "submatches" data))
         (path (gethash "text" (gethash "path" data)))
         (file (file-relative-name path))
         (line (gethash "line_number" data))
         (column (gethash "start" (aref submatches 0)))
         (content (string-trim-right (gethash "text" (gethash "lines" data)) "\n")))
    (when term (fins--highlight content term))
    (make-fins-entry
     :file file
     :line line
     :column column
     :content content)))

(defun fins--parse-jsons (lines &optional term)
  "Parse JSON LINES into a list of `fins-entry' structs.
When TERM is non-nil, highlight all matches of TERM in content."
  (delq nil (mapcar (lambda (line)
                      (let ((obj (json-parse-string line)))
                        (when (equal (gethash "type" obj) "match")
                          (fins--parse-json obj term))))
                    lines)))

(defun fins--grep-entry-p (entry)
  "Return non-nil if ENTRY is a grep entry with line, column, and content."
  (fins--with-entry entry
                     (and line column content)))

(defun fins--format-mark (marked)
  "Format the mark indicator for MARKED."
  (if marked
      (propertize "*" 'face 'fins-mark)
    " "))

(defun fins--format-file (file)
  "Format FILE name with face."
  (propertize file 'face 'fins-file))

(defun fins--format-line (line)
  "Format LINE number with face."
  (propertize (number-to-string line) 'face 'fins-line-number))

(defun fins--format-content (content)
  "Format CONTENT string."
  content)

(defun fins--format-entry (entry)
  "Format ENTRY as a display string."
  (fins--with-entry entry
                     (let ((text (if (fins--grep-entry-p entry)
                                     (format "%s %s:%s:%s"
                                             (fins--format-mark marked)
                                             (fins--format-file file)
                                             (fins--format-line line)
                                             (fins--format-content content))
                                   (format "%s %s"
                                           (fins--format-mark marked)
                                           (fins--format-file file)))))
                       (propertize text 'fins-entry entry))))

(defun fins--entry-at-point ()
  "Return the entry at point."
  (get-text-property (line-beginning-position) 'fins-entry))

(defun fins--find-highlight (content)
  "Return position of first match highlight in CONTENT, or nil."
  (cl-dolist (face fins-highlight-faces)
    (when-let ((pos (text-property-any 0 (length content) 'face face content)))
      (cl-return pos))))

(defun fins--highlight (content term)
  "Highlight all matches of TERM in CONTENT."
  (let ((start 0))
    (while (string-match (regexp-quote term) content start)
      (add-face-text-property (match-beginning 0) (match-end 0)
                              'match nil content)
      (setq start (match-end 0)))))

(defun fins--parse-candidate (candidate &optional term)
  "Parse CANDIDATE string into an `fins-entry'.
When TERM is non-nil, highlight all matches of TERM in content."
  (cond
   ;; Match grep entries with column
   ((string-match fins--grep-column-regexp candidate)
    (let* ((file (file-relative-name (match-string 1 candidate)))
           (line (string-to-number (match-string 2 candidate)))
           (column (1- (string-to-number (match-string 3 candidate))))
           (content (match-string 4 candidate)))
      (when term (fins--highlight content term))
      (make-fins-entry
       :file file
       :line line
       :column column
       :content content)))
   ;; Match grep entries without column
   ((string-match fins--grep-regexp candidate)
    (let* ((file (file-relative-name (match-string 1 candidate)))
           (line (string-to-number (match-string 2 candidate)))
           (content (match-string 3 candidate))
           (column (fins--find-highlight content)))
      (make-fins-entry
       :file file
       :line line
       :column column
       :content content)))
   ;; Treat as plain file
   ((string-match-p fins--file-regexp candidate)
    (make-fins-entry :file (file-relative-name candidate)))))

(defun fins--parse-candidates (candidates &optional term)
  "Parse CANDIDATES into a list of `fins-entry' structs.
When TERM is non-nil, highlight all matches of TERM in content.
Malformed candidates are silently skipped."
  (delq nil (mapcar (lambda (candidate) (fins--parse-candidate candidate term)) candidates)))

(defun fins--run-grep (command term files)
  "Run COMMAND with TERM on FILES, return output lines."
  (let* ((args (split-string-and-unquote command))
         (program (car args))
         (base-args (cdr args))
         (expanded-files (mapcar #'expand-file-name files)))
    (with-temp-buffer
      (apply #'call-process program nil t nil
             (append base-args (list term) expanded-files))
      (split-string (buffer-string) "\n" t))))

(defun fins--files ()
  "Return deduplicated list of files from `fins-entries'."
  (delete-dups (mapcar #'fins-entry-file fins-entries)))

(defun fins--redisplay ()
  "Render `fins-entries' into the current buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        ;; Record line position and top of viewport
        (line (line-number-at-pos))
        (wstart (window-start)))
    (combine-change-calls (point-min) (point-max)
      (erase-buffer)
      (dolist (entry fins-entries)
        (insert (fins--format-entry entry) "\n")))
    (goto-char (point-min))
    ;; Restore line position and top of viewport
    ;; TODO: Does there exist a builtin with-command that is not
    ;; invalidated by erase-buffer?
    (forward-line (1- line))
    (set-window-start nil wstart)))

(defun fins--redisplay-current ()
  "Redisplay the current line from its entry."
  (when-let ((entry (fins--entry-at-point)))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (insert (fins--format-entry entry) "\n")
      (forward-line -1))))

(defun fins--toggle-mark-in-region (beg end)
  "Toggle marks on all entries between BEG and END, redisplaying each line."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (when-let ((entry (fins--entry-at-point)))
        (fins--with-entry entry
                           (setf marked (not marked)))
        (fins--redisplay-current))
      (forward-line 1))))

(defun fins--set-mark-in-region (beg end value)
  "Set MARKED on all entries between BEG and END, redisplaying each line."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (when-let ((entry (fins--entry-at-point)))
        (fins--with-entry entry
                           (setf marked value))
        (fins--redisplay-current))
      (forward-line 1))))

(defun fins-mark ()
  "Mark the entry at point, or all entries in the active region."
  (interactive)
  (if (use-region-p)
      (fins--set-mark-in-region (region-beginning) (region-end) t)
    (when-let ((entry (fins--entry-at-point)))
      (fins--with-entry entry
                         (setf marked t))
      (fins--redisplay-current)
      (forward-line 1))))

(defun fins-unmark ()
  "Unmark the entry at point, or all entries in the active region."
  (interactive)
  (if (use-region-p)
      (fins--set-mark-in-region (region-beginning) (region-end) nil)
    (when-let ((entry (fins--entry-at-point)))
      (fins--with-entry entry
                         (setf marked nil))
      (fins--redisplay-current)
      (forward-line 1))))

(defun fins-unmark-all ()
  "Unmark all entries."
  (interactive)
  (dolist (entry fins-entries)
    (fins--with-entry entry
                       (setf marked nil)))
  (fins--redisplay))

(defun fins--buffer-name ()
  "Return a descriptive name for an Fins buffer."
  ;; TODO: Display the original command here if available
  (format "*Fins: %s*"
          (file-name-nondirectory
           (directory-file-name default-directory))))

(defun fins-import (candidates)
  "Import CANDIDATES into an Fins buffer."
  (let ((buf (generate-new-buffer (fins--buffer-name))))
    (with-current-buffer buf
      (fins-mode)
      (setq-local fins-entries (fins--parse-candidates candidates))
      (fins--redisplay))
    (pop-to-buffer buf)))

(defun fins-expand (term)
  "Expand entries into grep matches for TERM."
  (interactive "sGrep: ")
  (setq-local fins-entries
              (delete-dups
               (fins--parse-jsons
                (fins--run-grep fins-grep-lines-command term (fins--files))
                term)))
  (fins--redisplay))

(defun fins-mark-by-name (term)
  "Mark entries whose file matches TERM."
  (interactive "sMark by name: ")
  (dolist (entry fins-entries)
    (fins--with-entry entry
                       (when (string-match-p term file)
                         (setf marked t))))
  (fins--redisplay))

(defun fins-mark-by-content (term)
  "Mark entries whose file contains TERM."
  (interactive "sMark by content: ")
  (let ((matching (make-hash-table :test #'equal)))
    (dolist (file (fins--run-grep fins-grep-files-command term (fins--files)))
      (puthash (file-relative-name file) t matching))
    (dolist (entry fins-entries)
      (fins--with-entry entry
                         (when (gethash file matching)
                           (setf marked t))))
    (fins--redisplay)))

(defun fins-mark-by-lines (term)
  "Mark grep entries whose content matches TERM."
  (interactive "sMark by lines: ")
  (dolist (entry fins-entries)
    (fins--with-entry entry
                       (when (and content (string-match-p term content))
                         (setf marked t))))
  (fins--redisplay))

(defun fins-toggle-marks ()
  "Toggle marks on all entries, or entries in the active region."
  (interactive)
  (if (use-region-p)
      (fins--toggle-mark-in-region (region-beginning) (region-end))
    (dolist (entry fins-entries)
      (fins--with-entry entry
                         (setf marked (not marked))))
    (fins--redisplay)))

(defun fins-visit ()
  "Visit the file at point, jumping to the line if available."
  (interactive)
  (when-let ((entry (fins--entry-at-point)))
    (fins--with-entry entry
                       (find-file file)
                       (when line
                         (goto-char (point-min))
                         (forward-line (1- line))
                         (when column
                           (forward-char column))))))

(defun fins--entries-by-mark (value)
  "Return entries whose marked slot matches VALUE."
  (cl-remove-if-not (lambda (entry) (eq (fins-entry-marked entry) value))
                    fins-entries))

(defun fins-delete-marked ()
  "Delete all marked entries."
  (interactive)
  (setq-local fins-entries (fins--entries-by-mark nil))
  (fins--redisplay))

(defun fins-collapse ()
  "Collapse grep entries into file entries."
  (interactive)
  (setq-local fins-entries
              (mapcar (lambda (file) (make-fins-entry :file file)) (fins--files)))
  (fins--redisplay))

(defun fins--entry-to-file-candidate (entry)
  "Convert ENTRY to a file candidate string."
  (fins--with-entry entry file))

(defun fins--entry-to-grep-candidate (entry)
  "Convert ENTRY to a consult-grep candidate string."
  (fins--with-entry entry
                     (let* ((file-str (copy-sequence file))
                            (line-str (number-to-string line))
                            (str (concat file-str ":" line-str ":" content)))
                       (put-text-property 0 (length file-str) 'face 'consult-file str)
                       (put-text-property (1+ (length file-str))
                                          (+ 1 (length file-str) (length line-str))
                                          'face 'consult-line-number str)
                       str)))

(defun fins--entry-to-candidate (entry type)
  "Convert ENTRY to an embark candidate string of TYPE."
  (cond
   ((eq type 'consult-grep) (fins--entry-to-grep-candidate entry))
   ((eq type 'file) (fins--entry-to-file-candidate entry))))

(defun fins--candidate-type ()
  "Return the embark candidate type for the current Fins buffer."
  ;; All entries are valid grep candidates, and embark-consult is
  ;; available: consult-grep candidate
  (if (and (featurep 'embark-consult)
           (cl-every #'fins--grep-entry-p fins-entries))
      'consult-grep
    ;; otherwise: file candidate
    'file))

(defun fins-target-finder ()
  "Return the embark target for the entry at point."
  (when (derived-mode-p 'fins-mode)
    (when-let ((entry (fins--entry-at-point)))
      (let ((type (fins--candidate-type)))
        `(,type
          ,(fins--entry-to-candidate entry type)
          ,(line-beginning-position) . ,(line-end-position))))))

(defun fins-candidate-collector ()
  "Return marked or all entries as embark candidates."
  (when (derived-mode-p 'fins-mode)
    (let* ((entries (or (fins--entries-by-mark t) fins-entries))
           (type (fins--candidate-type)))
      (cons type (mapcar (lambda (entry)
                           (fins--entry-to-candidate entry type))
                         entries)))))

(with-eval-after-load 'embark
  (add-to-list 'embark-multitarget-actions #'fins-import)
  (add-to-list 'embark-target-finders #'fins-target-finder)
  (add-to-list 'embark-candidate-collectors #'fins-candidate-collector)
  (define-key embark-general-map (kbd "N") #'fins-import))

(provide 'fins)
;;; fins.el ends here

;;; fins.el --- Fins Interactively Narrows Searches  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sigge Rajamäe

;; Author: Sigge Rajamäe <sigge@rajamae.se>
;; Version: 0-git
;; Package-Requires: ((emacs "31.0"))
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
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

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

(defcustom fins-string-match-p-function #'fins--smart-string-match-p
  "Function used for matching a term when marking entries.
It is called with a regexp and a string, and returns non-nil if
the regexp matches the string."
  :type 'function
  :group 'fins)

(defcustom fins-highlight-faces '(consult-highlight-match xref-match match)
  "Faces recognized as match highlights when parsing candidates."
  :type '(repeat face)
  :group 'fins)

(defcustom fins-use-consult-preview t
  "Whether to use consult preview for the entry at point."
  :type 'boolean
  :group 'fins)

(defcustom fins-parser-alist
  '((consult-location . fins-parse-consult-location-candidate)
    (consult-grep     . fins-parse-grep-candidate)
    (consult-xref     . fins-parse-consult-xref-candidate)
    (file             . fins-parse-file-candidate)
    (project-file     . fins-parse-file-candidate))
  "Alist mapping embark target types to candidate parsers.
Each parser is called with a candidate string, and returns a
`fins-entry', or nil if the candidate cannot be parsed."
  :type '(alist :key-type symbol :value-type function)
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
  "N" #'fins-mark-by-name
  "c" #'fins-mark-by-content
  "l" #'fins-mark-by-lines
  "t"   #'fins-toggle-marks
  "n"   #'next-line
  "p"   #'previous-line
  "m"   #'fins-mark
  "u"   #'fins-unmark
  "U"   #'fins-unmark-all
  "e"   #'fins-expand
  "w"   #'fins-collapse
  "d"   #'fins-delete-marked
  "k"   #'fins-keep-marked
  "RET" #'fins-visit
  "<remap> <undo>" #'fins-undo)

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

(defvar-local fins-history nil
  "Stack of previous `fins-entries' states, most recent first.")

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

(defun fins--byte-to-char-offset (string offset)
  "Return the character offset of byte OFFSET in UTF-8 encoded STRING."
  (length (decode-coding-string (substring string 0 offset) 'utf-8-unix)))

;; @Todo: Breaks when `fins-grep-lines-command' is not a ripgrep command.
(defun fins--parse-json (obj)
  "Parse a JSON match OBJ into a `fins-entry'."
  (let* ((data (gethash "data" obj))
         (path (gethash "text" (gethash "path" data)))
         (file (file-relative-name path))
         (line (gethash "line_number" data))
         (lines (gethash "lines" data))
         ;; Man 1 rg: Since file paths or the contents of files are
         ;; not guaranteed to be valid UTF-8 and JSON itself must be
         ;; representable by a Unicode encoding, ripgrep will emit all
         ;; data elements as objects with one of two keys: text or
         ;; bytes.  text is a normal JSON string when the data is
         ;; valid UTF-8 while bytes is the base64 encoded contents of
         ;; the data.
         (bytes
          (if-let* ((text (gethash "text" lines)))
              (encode-coding-string text 'utf-8-unix)
            (base64-decode-string (gethash "bytes" lines))))
         (content (string-trim-right (decode-coding-string bytes 'utf-8-unix) "\n"))
         (ranges (mapcar (lambda (submatch)
                           (cons (fins--byte-to-char-offset bytes (gethash "start" submatch))
                                 (fins--byte-to-char-offset bytes (gethash "end" submatch))))
                         (gethash "submatches" data))))
    (fins--highlight content ranges)
    (make-fins-entry
     :file file
     :line line
     :column (caar ranges)
     :content content)))

(defun fins--parse-jsons (lines)
  "Parse JSON LINES into a list of `fins-entry' structs."
  (delq nil (mapcar (lambda (line)
                      (let ((obj (json-parse-string line)))
                        (when (equal (gethash "type" obj) "match")
                          (fins--parse-json obj))))
                    lines)))

(defun fins--grep-entry-p (entry)
  "Return non-nil if ENTRY is a grep entry with line and content."
  (fins--with-entry entry
    (and line content)))

(defun fins--smart-string-match-p (regexp string)
  "Return index of start of first match for REGEXP in STRING, or nil.
Matching respects case if REGEXP contains any upper-case characters."
  (let* ((has-upper (let ((case-fold-search nil))
                      (string-match-p "[[:upper:]]" regexp)))
         (case-fold-search (not has-upper)))
    (string-match-p regexp string)))

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

(defun fins--find-highlights (content)
  "Return list of (START . END) ranges of match highlights in CONTENT."
  (cl-loop for (start end props) in (object-intervals content)
           when (cl-intersection (ensure-list (plist-get props 'face))
                                 fins-highlight-faces)
           collect (cons start end)))

(defun fins--highlight (content ranges)
  "Strip faces from CONTENT, then highlight RANGES in it.
RANGES is a list of (START . END) ranges."
  (remove-text-properties 0 (length content) '(face nil) content)
  (dolist (range ranges)
    (put-text-property (car range) (cdr range) 'face 'match content)))

(defun fins-parse-grep-candidate (candidate)
  "Parse a grep CANDIDATE string into an `fins-entry'."
  (cond
   ;; Match grep entries with column
   ((string-match fins--grep-column-regexp candidate)
    (let* ((file (match-string 1 candidate))
           (line (string-to-number (match-string 2 candidate)))
           (content (match-string 4 candidate))
           (ranges (fins--find-highlights content))
           (column (caar ranges)))
      (fins--highlight content ranges)
      (make-fins-entry
       :file file
       :line line
       :column column
       :content content)))
   ;; Match grep entries without column
   ((string-match fins--grep-regexp candidate)
    (let* ((file (match-string 1 candidate))
           (line (string-to-number (match-string 2 candidate)))
           (content (match-string 3 candidate))
           (ranges (fins--find-highlights content))
           (column (caar ranges)))
      (fins--highlight content ranges)
      (make-fins-entry
       :file file
       :line line
       :column column
       :content content)))))

(defun fins-parse-file-candidate (candidate)
  "Parse a plain file CANDIDATE string into an `fins-entry'."
  (when (string-match-p fins--file-regexp candidate)
    (make-fins-entry :file candidate)))

(defun fins-parse-consult-location-candidate (candidate)
  "Parse a `consult-location' CANDIDATE into an `fins-entry'.
Candidates from buffers not visiting a file are skipped."
  (when-let* ((loc (consult--get-location candidate))
              (buf (marker-buffer (car loc)))
              (file (buffer-file-name buf)))
    (make-fins-entry
     :file file
     :line (cdr loc)
     :content candidate)))

(defun fins-parse-consult-xref-candidate (candidate)
  "Parse a `consult-xref' CANDIDATE into an `fins-entry'.
Candidates whose location is not an `xref-file-location' are skipped."
  (when-let* ((xref (get-text-property 0 'consult-xref candidate))
              (loc (xref-item-location xref))
              ((xref-file-location-p loc))
              (content (copy-sequence (xref-item-summary xref))))
    (fins--highlight content (fins--find-highlights content))
    (make-fins-entry
     :file (xref-file-location-file loc)
     :line (xref-file-location-line loc)
     :column (xref-file-location-column loc)
     :content content)))

(defvar fins--import-type nil
  "Embark target type of the candidates currently being imported.
Bound by `fins--capture-import-type' while `fins-import' runs.")

(defun fins--capture-import-type (&rest args)
  "Bind `fins--import-type' to the target type in ARGS, then run the action.
Meant to be used as an embark around action hook, which supplies the
action and its arguments under the `:run' key of ARGS."
  (let ((fins--import-type (plist-get args :type)))
    (apply (plist-get args :run) args)))

(defun fins--parse-candidates (candidates)
  "Parse CANDIDATES into a list of `fins-entry' structs.
The parser is chosen from `fins-parser-alist' according to
`fins--import-type'."
  (let ((parse (alist-get fins--import-type fins-parser-alist)))
    (unless parse
      (user-error "Unrecognized import type `%s'" fins--import-type))
    (let ((entries (delq nil (mapcar (lambda (candidate)
                                       (funcall parse (copy-sequence candidate)))
                                     candidates))))
      (unless entries
        (user-error "No `%s' candidates could be parsed" fins--import-type))
      entries)))

(defun fins--run-grep (command term files)
  "Run COMMAND with TERM on FILES, return output lines."
  (let* ((args (split-string-and-unquote command))
         (program (car args))
         (base-args (cdr args))
         (expanded-files (mapcar #'expand-file-name files)))
    (with-temp-buffer
      (apply #'call-process program nil (list t nil) nil
             (append base-args (list term) expanded-files))
      (split-string (buffer-string) "\n" t))))

(defun fins--files ()
  "Return deduplicated list of files from `fins-entries'."
  (delete-dups (mapcar #'fins-entry-file fins-entries)))

(defun fins--push-history ()
  "Push a copy of `fins-entries' onto `fins-history'."
  (push (mapcar #'copy-fins-entry fins-entries) fins-history))

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
    (forward-line (1- line))
    (set-window-start nil wstart)))

(defun fins--redisplay-current ()
  "Redisplay the current line from its entry."
  (when-let* ((entry (fins--entry-at-point)))
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
      (when-let* ((entry (fins--entry-at-point)))
        (fins--with-entry entry
          (setf marked (not marked)))
        (fins--redisplay-current))
      (forward-line 1))))

(defun fins--set-mark-in-region (beg end value)
  "Set MARKED on all entries between BEG and END, redisplaying each line."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (when-let* ((entry (fins--entry-at-point)))
        (fins--with-entry entry
          (setf marked value))
        (fins--redisplay-current))
      (forward-line 1))))

(defun fins-mark ()
  "Mark the entry at point, or all entries in the active region."
  (interactive)
  (fins--push-history)
  (if (use-region-p)
      (fins--set-mark-in-region (region-beginning) (region-end) t)
    (when-let* ((entry (fins--entry-at-point)))
      (fins--with-entry entry
        (setf marked t))
      (fins--redisplay-current)
      (forward-line 1))))

(defun fins-unmark ()
  "Unmark the entry at point, or all entries in the active region."
  (interactive)
  (fins--push-history)
  (if (use-region-p)
      (fins--set-mark-in-region (region-beginning) (region-end) nil)
    (when-let* ((entry (fins--entry-at-point)))
      (fins--with-entry entry
        (setf marked nil))
      (fins--redisplay-current)
      (forward-line 1))))

(defun fins-unmark-all ()
  "Unmark all entries."
  (interactive)
  (fins--push-history)
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
    ;; Deferred so the selection survives Embark restoring the window
    (run-at-time 0 nil #'pop-to-buffer buf)))

(defun fins-expand (term)
  "Expand entries into grep matches for TERM."
  (interactive "sGrep: ")
  (fins--push-history)
  (setq-local fins-entries
              (delete-dups
               (fins--parse-jsons
                (fins--run-grep fins-grep-lines-command term (fins--files)))))
  (fins--redisplay))

(defun fins-mark-by-name (term)
  "Mark entries whose file matches TERM."
  (interactive "sMark by name: ")
  (fins--push-history)
  (dolist (entry fins-entries)
    (fins--with-entry entry
      (when (funcall fins-string-match-p-function term file)
        (setf marked t))))
  (fins--redisplay))

(defun fins-mark-by-content (term)
  "Mark entries whose file contains TERM."
  (interactive "sMark by content: ")
  (fins--push-history)
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
  (fins--push-history)
  (dolist (entry fins-entries)
    (fins--with-entry entry
      (when (and content (funcall fins-string-match-p-function term content))
        (setf marked t))))
  (fins--redisplay))

(defun fins-toggle-marks ()
  "Toggle marks on all entries, or entries in the active region."
  (interactive)
  (fins--push-history)
  (if (use-region-p)
      (fins--toggle-mark-in-region (region-beginning) (region-end))
    (dolist (entry fins-entries)
      (fins--with-entry entry
        (setf marked (not marked))))
    (fins--redisplay)))

(defun fins-visit ()
  "Visit the file at point, jumping to the line if available."
  (interactive)
  (when-let* ((entry (fins--entry-at-point)))
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
  (fins--push-history)
  (setq-local fins-entries (fins--entries-by-mark nil))
  (fins--redisplay))

(defun fins-keep-marked ()
  "Keep only the marked entries."
  (interactive)
  (fins--push-history)
  (setq-local fins-entries (fins--entries-by-mark t))
  (fins--redisplay))

(defun fins-collapse ()
  "Collapse grep entries into file entries."
  (interactive)
  (fins--push-history)
  (setq-local fins-entries
              (mapcar (lambda (file) (make-fins-entry :file file)) (fins--files)))
  (fins--redisplay))

(defun fins-undo ()
  "Restore the previous state of entries."
  (interactive)
  ;; Pop all no-operations.
  (while (and fins-history (equal (car fins-history) fins-entries))
    (pop fins-history))
  (unless fins-history
    (user-error "No further undo information"))
  (setq-local fins-entries (pop fins-history))
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
    (when-let* ((entry (fins--entry-at-point)))
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
  (add-to-list 'embark-around-action-hooks '(fins-import fins--capture-import-type))
  (define-key embark-general-map (kbd "N") #'fins-import))

(provide 'fins)
;;; fins.el ends here

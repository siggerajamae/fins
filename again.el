;;; again.el --- Again Greps And Interactively Narrows -*- lexical-binding: t; -*-

;; Author: Sigge Rajamäe
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: matching, tools
;; URL: https://github.com/siggerajamae/again

;;; Commentary:

;; Iterative, interactive narrowing of file and grep matches.

;;; Code:

(require 'cl-lib)

(defgroup again nil
  "Again Greps And Interactively Narrows."
  :group 'matching)

(defcustom again-find-command "fd --color=never"
  "Command used for finding files by name."
  :type 'string
  :group 'again)

(defcustom again-grep-files-command "rg --files-with-matches --color=never"
  "Command used for finding files whose contents match a term."
  :type 'string
  :group 'again)

(defcustom again-grep-lines-command "rg --no-heading --line-number --column --color=never"
  "Command used for grepping lines from files."
  :type 'string
  :group 'again)

(defcustom again-highlight-faces '(consult-highlight-match match)
  "Faces recognized as match highlights when parsing candidates."
  :type '(repeat face)
  :group 'again)

(defcustom again-use-consult-preview t
  "Whether to use consult preview for the entry at point."
  :type 'boolean
  :group 'again)

(defface again-file
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight files in again buffers.")

(defface again-line-number
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight line numbers in again buffers.")

(defface again-mark
  '((t :inherit warning))
  "Face used to highlight marked entries in again buffers.")

(defvar-keymap again-mode-map
  :doc "Keymap for Again mode."
  :parent special-mode-map
  "* n" #'again-mark-by-name
  "* c" #'again-mark-by-content
  "* l" #'again-mark-by-lines
  "t"   #'again-toggle-marks
  "n"   #'next-line
  "p"   #'previous-line
  "m"   #'again-mark
  "u"   #'again-unmark
  "U"   #'again-unmark-all
  "e"   #'again-expand
  "w"   #'again-collapse
  "d"   #'again-delete-marked
  "RET" #'again-visit)

(define-derived-mode again-mode special-mode "Again"
  "Major mode for iterative narrowing of file and grep matches."
  (setq-local again-entries nil))

(cl-defstruct again-entry
  file
  line
  column
  content
  marked)

(defvar-local again-entries nil
  "List of `again-entry' entries in the current buffer.")

(defconst again--grep-column-regexp
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

(defconst again--grep-regexp
  (concat "\\`"          ; beginning of string
          "\\([^:]+\\)"  ; capture file
          ":"            ; separator
          "\\([0-9]+\\)" ; capture line number
          ":"            ; separator
          "\\(.*\\)"     ; capture content
          "\\'")         ; end of string
  "Regexp matching standard grep output format.")

(defconst again--file-regexp
  (concat "\\`"          ; beginning of string
          "[^\0\n]+"     ; one or more chars, no null bytes or newlines
          "\\'")         ; end of string
  "Regexp matching a valid file name.")

(defun again--format-entry (entry)
  "Format ENTRY as a display string."
  (let* ((mark (if (again-entry-marked entry)
                   (propertize "*" 'face 'again-mark)
                 " "))
         (file (propertize (again-entry-file entry) 'face 'again-file))
         (line (when (again-entry-line entry)
                 (propertize (number-to-string (again-entry-line entry))
                             'face 'again-line-number)))
         (content (again-entry-content entry))
         (text (if line
                   (format "%s %s:%s:%s" mark file line content)
                 (format "%s %s" mark file))))
    (propertize text 'again-entry entry)))

(defun again--entry-at-point ()
  "Return the entry at point."
  (get-text-property (line-beginning-position) 'again-entry))

(defun again--find-highlight (content)
  "Return position of first match highlight in CONTENT, or nil."
  (cl-dolist (face again-highlight-faces)
    (when-let ((pos (text-property-any 0 (length content) 'face face content)))
      (cl-return pos))))

(defun again--highlight (content term)
  "Highlight all matches of TERM in CONTENT."
  (let ((start 0))
    (while (string-match (regexp-quote term) content start)
      (add-face-text-property (match-beginning 0) (match-end 0)
                              'match nil content)
      (setq start (match-end 0)))))

(defun again--parse-candidate (candidate &optional term)
  "Parse CANDIDATE string into an `again-entry'.
When TERM is non-nil, highlight all matches of TERM in content."
  (cond
   ;; Match grep entries with column
   ((string-match again--grep-column-regexp candidate)
    (let* ((file (match-string 1 candidate))
           (line (string-to-number (match-string 2 candidate)))
           (column (1- (string-to-number (match-string 3 candidate))))
           (content (match-string 4 candidate)))
      (when term (again--highlight content term))
      (make-again-entry
       :file file
       :line line
       :column column
       :content content)))
   ;; Match grep entries without column
   ((string-match again--grep-regexp candidate)
    (let* ((file (match-string 1 candidate))
           (line (string-to-number (match-string 2 candidate)))
           (content (match-string 3 candidate))
           (column (again--find-highlight content)))
      (make-again-entry
       :file file
       :line line
       :column column
       :content content)))
   ;; Treat as plain file
   ((string-match-p again--file-regexp candidate)
    (make-again-entry :file candidate))))

(defun again--parse-candidates (candidates &optional term)
  "Parse CANDIDATES into a list of `again-entry' structs.
When TERM is non-nil, highlight all matches of TERM in content.
Malformed candidates are silently skipped."
  (delq nil (mapcar (lambda (candidate) (again--parse-candidate candidate term)) candidates)))

(defun again--run-grep (command term files)
  "Run COMMAND with TERM on FILES, return output lines."
  (let* ((args (split-string-and-unquote command))
         (program (car args))
         (base-args (cdr args))
         (expanded-files (mapcar #'expand-file-name files)))
    (with-temp-buffer
      (apply #'call-process program nil t nil
             (append base-args (list term) expanded-files))
      (split-string (buffer-string) "\n" t))))

(defun again--files ()
  "Return deduplicated list of files from `again-entries'."
  (delete-dups (mapcar #'again-entry-file again-entries)))

(defun again--redisplay ()
  "Render `again-entries' into the current buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (line (line-number-at-pos)))
    (combine-change-calls (point-min) (point-max)
      (erase-buffer)
      (dolist (entry again-entries)
        (insert (again--format-entry entry) "\n")))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun again--redisplay-current ()
  "Redisplay the current line from its entry."
  (when-let ((entry (again--entry-at-point)))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (insert (again--format-entry entry) "\n")
      (forward-line -1))))

(defun again--toggle-mark-in-region (beg end)
  "Toggle marks on all entries between BEG and END, redisplaying each line."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (when-let ((entry (again--entry-at-point)))
        (setf (again-entry-marked entry) (not (again-entry-marked entry)))
        (again--redisplay-current))
      (forward-line 1))))

(defun again--set-mark-in-region (beg end marked)
  "Set MARKED on all entries between BEG and END, redisplaying each line."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (when-let ((entry (again--entry-at-point)))
        (setf (again-entry-marked entry) marked)
        (again--redisplay-current))
      (forward-line 1))))

(defun again-mark ()
  "Mark the entry at point, or all entries in the active region."
  (interactive)
  (if (use-region-p)
      (again--set-mark-in-region (region-beginning) (region-end) t)
    (when-let ((entry (again--entry-at-point)))
      (setf (again-entry-marked entry) t)
      (again--redisplay-current)
      (forward-line 1))))

(defun again-unmark ()
  "Unmark the entry at point, or all entries in the active region."
  (interactive)
  (if (use-region-p)
      (again--set-mark-in-region (region-beginning) (region-end) nil)
    (when-let ((entry (again--entry-at-point)))
      (setf (again-entry-marked entry) nil)
      (again--redisplay-current)
      (forward-line 1))))

(defun again-unmark-all ()
  "Unmark all entries."
  (interactive)
  (dolist (entry again-entries)
    (setf (again-entry-marked entry) nil))
  (again--redisplay))

(defun again-import (candidates)
  "Import CANDIDATES into an again buffer."
  (let ((buf (get-buffer-create "*again*")))
    (with-current-buffer buf
      (again-mode)
      (setq-local again-entries (again--parse-candidates candidates))
      (again--redisplay))
    (pop-to-buffer buf)))

(defun again-expand (term)
  "Expand entries into grep matches for TERM."
  (interactive "sGrep: ")
  (let* ((lines (again--run-grep again-grep-lines-command term (again--files)))
         (new-entries (again--parse-candidates lines term)))
    (setq-local again-entries new-entries)
    (again--redisplay)))

(defun again-mark-by-name (term)
  "Mark entries whose file matches TERM."
  (interactive "sMark by name: ")
  (dolist (entry again-entries)
    (when (string-match-p term (again-entry-file entry))
      (setf (again-entry-marked entry) t)))
  (again--redisplay))

(defun again-mark-by-content (term)
  "Mark entries whose file contains TERM."
  (interactive "sMark by content: ")
  (let ((matching (make-hash-table :test #'equal)))
    (dolist (file (again--run-grep again-grep-files-command term (again--files)))
      (puthash file t matching))
    (dolist (entry again-entries)
      (when (gethash (again-entry-file entry) matching)
        (setf (again-entry-marked entry) t)))
    (again--redisplay)))

(defun again-mark-by-lines (term)
  "Mark grep entries whose content matches TERM."
  (interactive "sMark by lines: ")
  (dolist (entry again-entries)
    (when (and (again-entry-content entry)
               (string-match-p term (again-entry-content entry)))
      (setf (again-entry-marked entry) t)))
  (again--redisplay))

(defun again-toggle-marks ()
  "Toggle marks on all entries, or entries in the active region."
  (interactive)
  (if (use-region-p)
      (again--toggle-mark-in-region (region-beginning) (region-end))
    (dolist (entry again-entries)
      (setf (again-entry-marked entry) (not (again-entry-marked entry))))
    (again--redisplay)))

(defun again-visit ()
  "Visit the file at point, jumping to the line if available."
  (interactive)
  (when-let ((entry (again--entry-at-point)))
    (find-file (again-entry-file entry))
    (when-let ((line (again-entry-line entry)))
      (goto-char (point-min))
      (forward-line (1- line))
      (when-let ((col (again-entry-column entry)))
        (forward-char col)))))

(defun again-delete-marked ()
  "Delete all marked entries."
  (interactive)
  (setq-local again-entries
              (cl-remove-if #'again-entry-marked again-entries))
  (again--redisplay))

(defun again-collapse ()
  "Collapse grep entries into file entries."
  (interactive)
  (setq-local again-entries
              (mapcar (lambda (file) (make-again-entry :file file)) (again--files)))
  (again--redisplay))

(with-eval-after-load 'embark
  (add-to-list 'embark-multitarget-actions #'again-import)
  (define-key embark-general-map (kbd "N") #'again-import))

(provide 'again)
;;; again.el ends here

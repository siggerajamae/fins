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

(defcustom again-grep-lines-command "rg --no-heading --with-filename --line-number --column --color=never"
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
  "Face used to highlight files in Again buffers.")

(defface again-line-number
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight line numbers in Again buffers.")

(defface again-mark
  '((t :inherit warning))
  "Face used to highlight marked entries in Again buffers.")

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

(defmacro again--with-entry (entry &rest body)
  "Bind the slots of ENTRY to variables and execute BODY.
Binds `file', `line', `column', `content', and `marked'.
Supports `setf' on the bindings."
  (declare (indent 1))
  (let ((sym (gensym "entry")))
    `(let ((,sym ,entry))
       (cl-symbol-macrolet ((file (again-entry-file ,sym))
                            (line (again-entry-line ,sym))
                            (column (again-entry-column ,sym))
                            (content (again-entry-content ,sym))
                            (marked (again-entry-marked ,sym)))
         ,@body))))

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

(defun again--grep-entry-p (entry)
  "Return non-nil if ENTRY is a grep entry with line, column, and content."
  (again--with-entry entry
                     (and line column content)))

(defun again--format-mark (marked)
  "Format the mark indicator for MARKED."
  (if marked
      (propertize "*" 'face 'again-mark)
    " "))

(defun again--format-file (file)
  "Format FILE name with face."
  (propertize file 'face 'again-file))

(defun again--format-line (line)
  "Format LINE number with face."
  (propertize (number-to-string line) 'face 'again-line-number))

(defun again--format-content (content)
  "Format CONTENT string."
  content)

(defun again--format-entry (entry)
  "Format ENTRY as a display string."
  (again--with-entry entry
                     (let ((text (if (again--grep-entry-p entry)
                                     (format "%s %s:%s:%s"
                                             (again--format-mark marked)
                                             (again--format-file file)
                                             (again--format-line line)
                                             (again--format-content content))
                                   (format "%s %s"
                                           (again--format-mark marked)
                                           (again--format-file file)))))
                       (propertize text 'again-entry entry))))

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
    (let* ((file (file-relative-name (match-string 1 candidate)))
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
    (let* ((file (file-relative-name (match-string 1 candidate)))
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
    (make-again-entry :file (file-relative-name candidate)))))

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
        ;; Record line position and top of viewport
        (line (line-number-at-pos))
        (wstart (window-start)))
    (combine-change-calls (point-min) (point-max)
      (erase-buffer)
      (dolist (entry again-entries)
        (insert (again--format-entry entry) "\n")))
    (goto-char (point-min))
    ;; Restore line position and top of viewport
    ;; TODO: Does there exist a builtin with-command that is not
    ;; invalidated by erase-buffer?
    (forward-line (1- line))
    (set-window-start nil wstart)))

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
        (again--with-entry entry
                           (setf marked (not marked)))
        (again--redisplay-current))
      (forward-line 1))))

(defun again--set-mark-in-region (beg end value)
  "Set MARKED on all entries between BEG and END, redisplaying each line."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (when-let ((entry (again--entry-at-point)))
        (again--with-entry entry
                           (setf marked value))
        (again--redisplay-current))
      (forward-line 1))))

(defun again-mark ()
  "Mark the entry at point, or all entries in the active region."
  (interactive)
  (if (use-region-p)
      (again--set-mark-in-region (region-beginning) (region-end) t)
    (when-let ((entry (again--entry-at-point)))
      (again--with-entry entry
                         (setf marked t))
      (again--redisplay-current)
      (forward-line 1))))

(defun again-unmark ()
  "Unmark the entry at point, or all entries in the active region."
  (interactive)
  (if (use-region-p)
      (again--set-mark-in-region (region-beginning) (region-end) nil)
    (when-let ((entry (again--entry-at-point)))
      (again--with-entry entry
                         (setf marked nil))
      (again--redisplay-current)
      (forward-line 1))))

(defun again-unmark-all ()
  "Unmark all entries."
  (interactive)
  (dolist (entry again-entries)
    (again--with-entry entry
                       (setf marked nil)))
  (again--redisplay))

(defun again--buffer-name ()
  "Return a descriptive name for an Again buffer."
  ;; TODO: Display the original command here if available
  (format "*Again: %s*"
          (file-name-nondirectory
           (directory-file-name default-directory))))

(defun again-import (candidates)
  "Import CANDIDATES into an Again buffer."
  (let ((buf (generate-new-buffer (again--buffer-name))))
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
    (again--with-entry entry
                       (when (string-match-p term file)
                         (setf marked t))))
  (again--redisplay))

(defun again-mark-by-content (term)
  "Mark entries whose file contains TERM."
  (interactive "sMark by content: ")
  (let ((matching (make-hash-table :test #'equal)))
    (dolist (file (again--run-grep again-grep-files-command term (again--files)))
      (puthash file t matching))
    (dolist (entry again-entries)
      (again--with-entry entry
                         (when (gethash file matching)
                           (setf marked t))))
    (again--redisplay)))

(defun again-mark-by-lines (term)
  "Mark grep entries whose content matches TERM."
  (interactive "sMark by lines: ")
  (dolist (entry again-entries)
    (again--with-entry entry
                       (when (and content (string-match-p term content))
                         (setf marked t))))
  (again--redisplay))

(defun again-toggle-marks ()
  "Toggle marks on all entries, or entries in the active region."
  (interactive)
  (if (use-region-p)
      (again--toggle-mark-in-region (region-beginning) (region-end))
    (dolist (entry again-entries)
      (again--with-entry entry
                         (setf marked (not marked))))
    (again--redisplay)))

(defun again-visit ()
  "Visit the file at point, jumping to the line if available."
  (interactive)
  (when-let ((entry (again--entry-at-point)))
    (again--with-entry entry
                       (find-file file)
                       (when line
                         (goto-char (point-min))
                         (forward-line (1- line))
                         (when column
                           (forward-char column))))))

(defun again--entries-by-mark (value)
  "Return entries whose marked slot matches VALUE."
  (cl-remove-if-not (lambda (entry) (eq (again-entry-marked entry) value))
                    again-entries))

(defun again-delete-marked ()
  "Delete all marked entries."
  (interactive)
  (setq-local again-entries (again--entries-by-mark nil))
  (again--redisplay))

(defun again-collapse ()
  "Collapse grep entries into file entries."
  (interactive)
  (setq-local again-entries
              (mapcar (lambda (file) (make-again-entry :file file)) (again--files)))
  (again--redisplay))

(defun again--entry-to-file-candidate (entry)
  "Convert ENTRY to a file candidate string."
  (again--with-entry entry file))

(defun again--entry-to-grep-candidate (entry)
  "Convert ENTRY to a consult-grep candidate string."
  (again--with-entry entry
                     (let* ((file-str (copy-sequence file))
                            (line-str (number-to-string line))
                            (str (concat file-str ":" line-str ":" content)))
                       (put-text-property 0 (length file-str) 'face 'consult-file str)
                       (put-text-property (1+ (length file-str))
                                          (+ 1 (length file-str) (length line-str))
                                          'face 'consult-line-number str)
                       str)))

(defun again--entry-to-candidate (entry type)
  "Convert ENTRY to an embark candidate string of TYPE."
  (cond
   ((eq type 'consult-grep) (again--entry-to-grep-candidate entry))
   ((eq type 'file) (again--entry-to-file-candidate entry))))

(defun again--candidate-type ()
  "Return the embark candidate type for the current Again buffer."
  ;; All entries are valid grep candidates, and embark-consult is
  ;; available: consult-grep candidate
  (if (and (featurep 'embark-consult)
           (cl-every #'again--grep-entry-p again-entries))
      'consult-grep
    ;; otherwise: file candidate
    'file))

(defun again-target-finder ()
  "Return the embark target for the entry at point."
  (when (derived-mode-p 'again-mode)
    (when-let ((entry (again--entry-at-point)))
      (let ((type (again--candidate-type)))
        `(,type
          ,(again--entry-to-candidate entry type)
          ,(line-beginning-position) . ,(line-end-position))))))

(defun again-candidate-collector ()
  "Return marked or all entries as embark candidates."
  (when (derived-mode-p 'again-mode)
    (let* ((entries (or (again--entries-by-mark t) again-entries))
           (type (again--candidate-type)))
      (cons type (mapcar (lambda (entry)
                           (again--entry-to-candidate entry type))
                         entries)))))

(with-eval-after-load 'embark
  (add-to-list 'embark-multitarget-actions #'again-import)
  (add-to-list 'embark-target-finders #'again-target-finder)
  (add-to-list 'embark-candidate-collectors #'again-candidate-collector)
  (define-key embark-general-map (kbd "N") #'again-import))

(provide 'again)
;;; again.el ends here

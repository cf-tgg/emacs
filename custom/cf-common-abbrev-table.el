;;; cf-common-abbrev-table.el --- cf. abbrevs -*- lexical-binding: t; -*-

;;; Commentary:
;;      Simple abbrevs I often use.

;;; Code:

(abbrev-mode)

(defun abrv/current-time ()
  "Current time abbrev."
  (insert  (format-time-string "%T")))
(define-abbrev global-abbrev-table "mtt" "" 'abrv/current-time)

(defun abrv/current-date-time ()
  "Current date time abbrev."
  (insert  (format-time-string "%Y-%m-%d %T")))
(define-abbrev global-abbrev-table "mdt" "" 'abrv/current-date-time)

(defun abrv/hls-playlist-header ()
  "HLS playlist header abbrev."
  (insert  "#EXTM3U\n"))
(define-abbrev global-abbrev-table "m3u" "" 'abrv/hls-playlist-header)

(defun abrv/lexical-binding ()
  "Lexical-binding abbrev."
  (insert ";; -*- lexical-binding: t; -*-\n"))
(define-abbrev global-abbrev-table "lxb" "" 'abrv/lexical-binding)

(defvar abrv/file-local-variables-comment-alist
  '((emacs-lisp-mode   . ";; ")
    (lua-mode          . "--[ ")
    (c-mode            . "/* ")
    (c++-mode          . "/* ")
    (java-mode         . "// ")
    (cs-mode           . "// ")
    (js-mode           . "/* ")
    (ts-mode           . "/* ")
    (simpc-mode        . "/* ")
    (python-mode       . "# ")
    (hls-playlist-mode . "# ")
    (sh-mode           . "# ")
    (conf-mode         . "# ")
    (xconf-mode        . "# ")
    (org-mode          . nil)
    (json-mode         . nil)) ;; No comments allowed
  "User preferred comment strings per major mode.")

(defun abrv/insert-elisp-header ()
  "Insert a standard Emacs Lisp header if not present."
  (interactive)
  (unless (save-excursion
            (goto-char (point-min))
            (re-search-forward "^-\\*-.*-\\*-" (line-end-position 2) t))
    (goto-char (point-min))
    (insert ";;; " (file-name-nondirectory (buffer-file-name)) " ---\n\n")
    (insert ";;; Commentary:\n\n")
    (insert ";;; Code:\n\n")
    (insert ";;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")
    (save-buffer)))

(defun abrv/insert-org-header ()
  "Insert standard Org mode header if not already present."
  (interactive)
  (unless (save-excursion
            (goto-char (point-min))
            (re-search-forward "^#\\+title:" nil t))
    (goto-char (point-min))
    (insert (format "#+title: %s\n" (file-name-base (or (buffer-file-name) (buffer-name)))))
    (insert (format "#+author: %s\n" (or (getenv "USER") "Author")))
    (insert (format "#+date: %s\n" (format-time-string "<%Y-%m-%d %T>")))
    (insert "\n")
    (save-buffer)))
(define-abbrev global-abbrev-table "orghd" "" #'abrv/insert-org-header)

(defun abrv/major-mode-prop-line ()
  "Insert an Emacs file local variable prop line or mode-specific header.
Handles:
- Emacs Lisp files via `abrv/insert-elisp-header`.
- Org mode files via `abrv/insert-org-header`.
- JSON or unsupported modes are error-checked.
- Comment-style property lines otherwise."
  (interactive)
  (let* ((mode (if (or (not major-mode) (eq major-mode 'text-mode))
                   (intern (completing-read "Major mode: " obarray
                                            (lambda (sym)
                                              (and (commandp sym)
                                                   (string-suffix-p "-mode" (symbol-name sym))))
                                            t))
                 major-mode))
         (mode-name (symbol-name mode))
         (comment-string (cdr (assoc mode abrv/file-local-variables-comment-alist))))
    (cond
     ((eq mode 'emacs-lisp-mode)
      (abrv/insert-elisp-header))
     ((eq mode 'org-mode)
      (abrv/insert-org-header))
     ((null comment-string)
      (user-error "No comment syntax defined for %s (e.g., JSON files don't support comments)" mode-name))
     (t
      (let* ((cs-left (string-trim-right comment-string))
             (cs-right (if (string-match "\\`\\s-*\\([^[:space:]]+\\)\\s-*\\'" comment-string)
                           (match-string 1 comment-string)
                         comment-string))
             (prop-line (format "%s -*- mode: %s; -*- %s" cs-left mode-name cs-right)))
        (save-excursion
          (goto-char (point-min))
          (if (looking-at-p (or "^#!" "^#EXTM3U"))
              (progn
                (forward-line 1)
                (insert prop-line "\n"))
            (insert prop-line "\n")))
        (save-buffer))))))
(define-abbrev global-abbrev-table "MM" "" #'abrv/major-mode-prop-line)

(defun abrv/insert-elisp-header ()
  "Insert a standard Emacs Lisp file header."
  (interactive)
  (let ((file (file-name-base (or (buffer-file-name) (buffer-name))))
        (desc (read-string "Short description: "))
        (comm (read-string "Commentary: "))
        (lib  (read-string "Main library to require (optional): ")))
    (insert (format
             ";;; %s.el --- %s -*- lexical-binding: t; -*-\n\n"
             file desc))
    (insert ";;; Commentary:\n"
            ";; " comm "\n\n")
    (insert ";;; Code:\n\n")
    (when (not (string-empty-p lib))
      (insert (format "(require '%s)\n\n" lib)))
    (insert (format "(provide '%s)\n" file))
    (insert (format ";;; %s.el ends here\n" file))))
(define-abbrev emacs-lisp-mode-abbrev-table "elhd" "" 'abrv/insert-elisp-header)

(defun abrv/c-file-local-variables ()
  "Insert a standard file local variables block.
Prefer user-specified comment strings per major mode."
  (interactive)
  (let* ((cs (or (cdr (assoc major-mode abrv/file-local-variables-comment-alist))
                 (and comment-start (string-trim comment-start))
                 (read-string "Comment string: " "/* ")))
         (cs-left (string-trim-right cs))
         (cs-right (if (string-match "\\`\\s-*\\([^[:space:]]+\\)\\s-*\\'" cs)
                       (match-string 1 cs)
                     cs))
         (field-width 26)
         (local-vars '("Local Variables:"
                       "compile-command: \"make\""
                       "coding: utf-8"
                       "fill-column: 90"
                       "indent-tabs-mode: nil"
                       "require-final-newline: t"
                       "End:")))
    ;; Insert comment block for modes that use block comments (e.g. simpc-mode)
    (if (eq major-mode (or (simpc-mode c-ts-mode js-mode ts-mode)))
        (insert (format "%s\n * %s\n * %s\n * %s\n * %s\n * %s\n%s"
                        cs-left
                        (mapconcat (lambda (line) (format "%-26s: %s" line "")) local-vars "\n * ")
                        cs-right))
      (insert (mapconcat
               (lambda (line)
                 (format "%s %s %s"
                         cs-left
                         (string-pad line field-width)
                         cs-right))
               local-vars "\n")))))
(define-abbrev global-abbrev-table "cfv" "" #'abrv/c-file-local-variables)

(defun abrv/file-local-variables ()
  "Insert a standard file local variables block.
Prefer user-specified comment strings per major mode."
  (interactive)
  (let* ((cs (or (cdr (assoc major-mode abrv/file-local-variables-comment-alist))
                 (and comment-start (string-trim comment-start))
                 (read-string "Comment string: " ";; ")))
         (cs-left (string-trim-right cs))
         (cs-right (if (string-match "\\`\\s-*\\([^[:space:]]+\\)\\s-*\\'" cs)
                       (match-string 1 cs)
                     cs))
         (field-width 26))
    (insert
     (mapconcat
      (lambda (line)
        (format "%s %s %s"
                cs-left
                (string-pad line field-width)
                cs-right))
      '("Local Variables:"
        "coding: utf-8"
        "fill-column: 90"
        "indent-tabs-mode: nil"
        "require-final-newline: t"
        "End:")
      "\n")
 "")))
(define-abbrev global-abbrev-table "flv" "" #'abrv/file-local-variables)

(defun abrv/c-compile-command-var ()
  "Prompt for and insert a `compile-command` as a local variable inside a properly formatted Local Variables block.
Respects user-defined comment strings from `abrv/file-local-variables-comment-alist`.
Pads the comment sides uniformly.
Also sets `compile-command` buffer-locally and compiles the file immediately."
  (interactive)
  (let* ((existing-block-start (save-excursion
                                  (goto-char (point-min))
                                  (re-search-forward "Local Variables:" nil t)))
         (compile-cmd (read-string (format "Compile command (%s): "
                                           (or (car compile-history) "make"))
                                   nil 'compile-history))
         (cs (or (cdr (assoc major-mode abrv/file-local-variables-comment-alist))
                 (and comment-start (string-trim comment-start))
                 (read-string "Comment string: " "/* ")))
         (cs-left (string-trim-right cs))
         (cs-right (if (string-match "\\`\\s-*\\([^[:space:]]+\\)\\s-*\\'" cs)
                       (match-string 1 cs)
                     cs))
         (field-width 26))
    (if existing-block-start
        (save-excursion
          (goto-char existing-block-start)
          (forward-line 1)
          (insert (format "%s %s %s\n"
                          cs-left
                          (string-pad (concat "compile-command: \"" compile-cmd "\"") field-width)
                          cs-right)))
      (save-excursion
        (goto-char (point-max))
        (insert (mapconcat
                 (lambda (line)
                   (format "%s %s %s"
                           cs-left
                           (string-pad line field-width)
                           cs-right))
                 (list "Local Variables:"
                       (format "compile-command: \"%s\"" compile-cmd)
                       "coding: utf-8"
                       "fill-column: 90"
                       "indent-tabs-mode: nil"
                       "require-final-newline: t"
                       "End:")
                 "\n"))
        ""))
    (setq-local compile-command compile-cmd)
    (save-buffer)
    (compile compile-cmd)))
(define-abbrev global-abbrev-table "CC" "" #'abrv/c-compile-command-var)

(defun abrv/local-variables ()
  "Insert a standard file local variables block with aligned columns."
  (interactive)
  (let* ((cs (or (cdr (assoc major-mode abrv/file-local-variables-comment-alist))
                 (and comment-start (string-trim comment-start))
                 (read-string "Comment string: " "/* ")))
         (cs-left (string-trim-right cs))
         (cs-right (if (string-match "\\`\\s-*\\([^[:space:]]+\\)\\s-*\\'" cs)
                       (match-string 1 cs)
                     cs))
         (local-vars `(("Local Variables:" . "")
                       ("compile-command" . "\"make\"")
                       ("coding" . "utf-8")
                       ("fill-column" . "90")
                       ("indent-tabs-mode" . "nil")
                       ("require-final-newline" . "t")
                       ("End:" . ""))))
    (insert (format "%s\n" cs-left))
    (insert (abrv/align-local-variable-fields local-vars))
    (insert (format "\n%s" cs-right))))
(define-abbrev global-abbrev-table "LV" "" #'abrv/local-variables)

;; (defun cf/indent-for-tab-function ()
;;   "Smart TAB that prioritizes major mode indentation and tight delimiter alignment."
;;   (interactive)
;;   (let ((indent-tabs-mode nil)) ;; Always spaces
;;     (cond
;;      ((and transient-mark-mode mark-active)
;;       (cf/align-region-minimal (region-beginning) (region-end)))
;;      (t
;;       (let ((orig-point (point)))
;;         (indent-according-to-mode)
;;         (when (= (point) orig-point)
;;           (cf/align-line-by-previous-minimal)))))))

;; (defun cf/align-region-minimal (beg end)
;;   "Align region, minimal padding only at matching delimiters."
;;   (save-excursion
;;     (let* ((lines (cf/get-lines beg end))
;;            (baselines (mapcar #'cf/line-baseline-indentation lines))
;;            (delim-cols (cf/compute-dominant-delimiter-columns lines)))
;;       (cf/replace-region-with (mapcar* #'cf/realign-single-line-minimal
;;                                        lines baselines (make-list (length lines) delim-cols))
;;                                beg end))))

;; (defun cf/get-lines (beg end)
;;   "Return list of lines between BEG and END."
;;   (split-string (buffer-substring-no-properties beg end) "\n"))

;; (defun cf/replace-region-with (lines beg end)
;;   "Replace region from BEG to END with LINES."
;;   (goto-char beg)
;;   (delete-region beg end)
;;   (insert (mapconcat #'identity lines "\n")))

;; (defun cf/line-baseline-indentation (line)
;;   "Count leading spaces of LINE."
;;   (if (string-match "^\\s-*" line)
;;       (length (match-string 0 line))
;;     0))

;; (defun cf/compute-dominant-delimiter-columns (lines)
;;   "Compute dominant columns per delimiter."
;;   (let ((delims '("," "="))
;;         (positions (make-hash-table :test 'equal)))
;;     (dolist (line lines)
;;       (let ((pos 0))
;;         (dolist (delim delims)
;;           (while (string-match (regexp-quote delim) line pos)
;;             (push (match-beginning 0) (gethash delim positions nil))
;;             (setq pos (1+ (match-beginning 0))))
;;           (setq pos 0))))
;;     (let (result)
;;       (maphash
;;        (lambda (delim poslist)
;;          (when poslist
;;            (let ((most (cf/most-common poslist)))
;;              (push (cons delim most) result))))
;;        positions)
;;       result)))

;; (defun cf/most-common (lst)
;;   "Return most common element in LST."
;;   (car (car (sort (cl-group lst :test #'=) (lambda (a b) (> (length a) (length b)))))))

;; (defun cf/realign-single-line-minimal (line baseline-cols dominant-cols)
;;   "Realign delimiters inside LINE with minimal spacing, preserve BASELINE-COLS."
;;   (let ((content (string-trim-left line)))
;;     (setq content (cf/minimal-align-content content dominant-cols))
;;     (concat (make-string baseline-cols ?\s) content)))

;; (defun cf/minimal-align-content (content dominant-cols)
;;   "Realign CONTENT delimiters minimally to dominant-cols."
;;   (dolist (pair dominant-cols content)
;;     (let* ((delim (car pair))
;;            (target (cdr pair))
;;            (regex (regexp-quote delim)))
;;       (while (string-match regex content)
;;         (let* ((pos (match-beginning 0))
;;                (needed (- target pos)))
;;           (when (> needed 0)
;;             (setq content (concat (substring content 0 pos)
;;                                   (make-string needed ?\s)
;;                                   (substring content pos))))
;;           (setq pos (+ (match-end 0) needed))
;;           (setq content (concat (substring content 0 pos)
;;                                 (cf/minimal-align-content (substring content pos) dominant-cols))))
;;         (cl-return-from cf/minimal-align-content content)))
;;     content))

;; (defun cf/align-line-by-previous-minimal ()
;;   "Align current line delimiters relative to previous line, minimal padding."
;;   (save-excursion
;;     (let ((cur-indent (current-indentation)))
;;       (forward-line -1)
;;       (let ((prev-indent (current-indentation)))
;;         (when (> prev-indent cur-indent)
;;           (indent-line-to prev-indent))))))

;; (keymap-global-set "TAB" #'cf/indent-for-tab-function)



;;; _
(provide 'cf-common-abbrev-table)
;; Local Variables:
;; coding: utf-8
;; fill-column: 90
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:
;;; cf-common-abbrev-table.el ends here

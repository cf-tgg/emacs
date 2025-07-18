;;; cf-sdcv-definition.el --- SDCV Word lookups -*- lexical-binding: t; -*-

;;; Commentary:
;;   Provides interactive `sdcv' dictionary lookup with ANSI decoding,
;;   Eldoc integration for hover minibuffer definitions.

;;  Author: cf.
;;  Repos:
;;  + https://github.com/cf-tgg/emacs.git
;;  + https://github.com/cf-tgg/sdcv-dicts.git

;;; Code:

(require 'ansi-color)
(require 'thingatpt)
(require 'eldoc)

(defgroup sdcv-definition nil
  "Lookup words using `sdcv` with ANSI decoding and Eldoc support."
  :group 'applications)

(defcustom sdcv-default-dictionaries nil
  "List of available dictionaries, initialized on demand."
  :type '(repeat string)
  :group 'sdcv-definition)

(defcustom sdcv-default-dictionary nil
  "Default dictionary for exact lookups."
  :type 'string
  :group 'sdcv-definition)

(defcustom sdcv-command
  "sdcv -n --utf8-output --color %s 2>/dev/null"
  "Command template for fuzzy lookup."
  :type 'string
  :group 'sdcv-definition)

(defcustom sdcv-exact-command
  "sdcv -n --utf8-output --color -e %s 2>/dev/null"
  "Command template for exact lookup."
  :type 'string
  :group 'sdcv-definition)

(defcustom sdcv-ansi-cleanup-regexp
  (concat
   "\\(?:\x1B\\[[0-9;]*[A-Za-z]\\)"
   "\\|\\(?:\x1B[][()#;?]*[0-9]*[A-Za-z]\\)"
   "\\|\\(?:\x1B.\\)"
   "\\|\\(?:[\x00-\x08\x0B\x0C\x0E-\x1F]\\)"
   "\\|\\(?:[\x7F-\x9F]\\)")
  "Regexp matching unwanted ANSI sequences."
  :type 'regexp
  :group 'sdcv-definition)

(defcustom sdcv-delimiter "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"
  "Delimiter used between dictionary sections."
  :type 'string
  :group 'sdcv-definition)

(defcustom sdcv-buffer-name "*sdcv-definition*"
  "Buffer name for SDCV output."
  :type 'string
  :group 'sdcv-definition)

(defcustom sdcv-default-face 'default
  "Face applied to SDCV output."
  :type 'face
  :group 'sdcv-definition)


;;; Utilities

(defun cf--string-blank-p (str)
  "Return non-nil if STR is nil, empty, or only whitespace."
  (or (null str) (string-match-p "\\`[ \t\n\r]*\\'" str)))

(defun cf--clean-ansi-artifacts (beg end)
  "Remove ANSI artifacts between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward sdcv-ansi-cleanup-regexp end t)
      (replace-match ""))))

(defun cf--sdcv-run-command (command word)
  "Run SDCV COMMAND for WORD, clean output and return it."
  (let ((output (shell-command-to-string
                 (format command (shell-quote-argument word)))))
    (unless (cf--string-blank-p output)
      (with-temp-buffer
        (insert output)
        (ansi-color-apply-overlay-face (point-min) (point-max) 'sdcv-default-face)
        (ansi-color-apply-on-region (point-min) (point-max))
        (cf--clean-ansi-artifacts (point-min) (point-max))
        (buffer-string)))))

(defun cf--sdcv-installed-dictionaries ()
  "Return a list of installed SDCV dictionaries."
  (let ((output (shell-command-to-string "sdcv -l 2>/dev/null | awk -F'    ' '{print $1}'")))
    (split-string output "\n" t "\\s-+")))

(defun cf--sdcv-init-default-dictionaries ()
  "Initialize `sdcv-default-dictionaries' from installed SDCV dicts if not set."
  (unless sdcv-default-dictionaries
    (setq sdcv-default-dictionaries (cf--sdcv-installed-dictionaries))))

(defun cf--sdcv-display-buffer ()
  "Display the `sdcv-buffer-name' buffer on right side."
  (display-buffer
   (get-buffer-create sdcv-buffer-name)
   '((display-buffer-reuse-window display-buffer-in-side-window)
     (side . right)
     (slot . 1)
     (window-width . 0.4)
     (window-parameters . ((mode-line-format . none))))))


;;; Major Mode

(defvar sdcv-definition-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "]") #'sdcv-definition-section-next)
    (define-key map (kbd "[") #'sdcv-definition-section-previous)
    (define-key map (kbd "}") #'forward-paragraph)
    (define-key map (kbd "}") #'backward-paragraph)
    (define-key map (kbd "C-M-]") #'kill-paragraph)
    (define-key map (kbd "C-M-[") #'backward-kill-paragraph)
    (define-key map (kbd "k") #'kill-sentence)
    (define-key map (kbd "K") #'backward-kill-sentence)
    (define-key map (kbd "n") #'sdcv-next-definition)
    (define-key map (kbd "p") #'sdcv-previous-definition)
    (define-key map (kbd "u") #'scroll-down-command)
    (define-key map (kbd "v") #'scroll-up-command)
    (define-key map (kbd "h") #'hl-line-mode)
    (define-key map (kbd "f") #'forward-word)
    (define-key map (kbd "b") #'backward-word)
    (define-key map (kbd "m") #'cf/mark-construct-dwim)
    (define-key map (kbd "w") #'forward-word)
    (define-key map (kbd "i") #'back-to-indentation)
    (define-key map (kbd "a") #'backward-sentence)
    (define-key map (kbd "e") #'forward-sentence)
    (define-key map (kbd "o") #'end-of-line)
    (define-key map (kbd "x") #'cf/lookup-word)
    (define-key map (kbd ",") #'cf/lookup-word)
    (define-key map (kbd ".") #'isearch-forward-thing-at-point)
    (define-key map (kbd ";") #'set-mark-command)
    (define-key map (kbd "'") #'mark-word)
    (define-key map (kbd "X") #'cf/exact-lookup-word)
    (define-key map (kbd "l") #'recenter-top-bottom)
    (define-key map (kbd "c") #'cf/ansi-color-current-buffer)
    (define-key map (kbd "D") #'cf/sdcv-set-default-dictionary)
    (define-key map (kbd "S") #'cf/sdcv-set-default-dictionaries)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `sdcv-definition-mode'.")

(define-derived-mode sdcv-definition-mode special-mode "SDCV-Definition"
  "Major mode for SDCV output buffers."
  (when (fboundp 'olivetti-mode) (olivetti-mode 1))
  (when (fboundp 'font-lock-mode) (font-lock-mode 1))
  (when (fboundp 'highlight-numbers-mode) (highlight-numbers-mode 1))
  (when (fboundp 'rainbow-delimiters-mode) (rainbow-delimiters-mode 1))
  (when (fboundp 'highlight-quoted-mode) (highlight-quoted-mode 1))
  (when (fboundp 'hl-todo-mode) (hl-todo-mode 1))
  (when (fboundp 'treesit-auto-mode) (treesit-auto-mode 1))
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil))



;;; Interactive Lookup

;;;###autoload
(defun cf/sdcv-set-default-dictionaries ()
  "Interactively set `sdcv-default-dictionaries' for the current session."
  (interactive)
  (let* ((dict-list (cf--sdcv-installed-dictionaries))
         (selected-dicts (completing-read-multiple "Set `sdcv-default-dictionaries': " dict-list nil t)))
    (if selected-dicts
        (setq-local sdcv-default-dictionaries selected-dicts)
      (cf--sdcv-init-default-dictionaries))))

;;;###autoload
(defun cf/sdcv-set-default-dictionary ()
  "Interactively set `sdcv-default-dictionary' for the current session."
  (interactive)
  (let* ((dict-list (cf--sdcv-installed-dictionaries))
         (selected-dict (completing-read "Set `sdcv-default-dictionary': " dict-list nil t)))
    (if selected-dict
        (setq sdcv-default-dictionary selected-dict)
      (message "No dictionary selected."))
    (message (format "sdcv-default-dictionary set to: %s"  sdcv-default-dictionary))))

;;;###autoload
(defun cf/lookup-word (&optional word)
  "Lookup WORD using fuzzy search.
If WORD is nil, use word at point."
  (interactive)
  (let ((word (or word (thing-at-point 'word t))))
    (when word
      (let ((result (cf--sdcv-run-command sdcv-command word)))
        (when result
          (with-current-buffer (get-buffer-create sdcv-buffer-name)
            (let ((inhibit-read-only t))
              (sdcv-definition-mode)
              (goto-char (point-max))
              (let ((start (point)))
                (insert sdcv-delimiter)
                (insert result)
                (goto-char start)
                (search-forward sdcv-delimiter nil t)
                (forward-line 1)))
            (cf--sdcv-display-buffer)))))))


;;;###autoload
(defun cf/exact-lookup-word (&optional word)
  "Lookup WORD using --exact-search.
If WORD is nil, use word at point."
  (interactive)
  (let ((word (or word (thing-at-point 'word t))))
    (when word
      (let ((result (cf--sdcv-run-command sdcv-exact-command word)))
        (when result
          (with-current-buffer (get-buffer-create sdcv-buffer-name)
            (let ((inhibit-read-only t))
              (sdcv-definition-mode)
              (goto-char (point-max))
              (let ((start (point)))
                (insert sdcv-delimiter)
                (insert result)
                (goto-char start)
                (search-forward sdcv-delimiter nil t)
                (forward-line 1)))
            (cf--sdcv-display-buffer)))))))

;;;###autoload
(defun cf/ansi-color-current-buffer ()
  "Reapply ANSI color parsing in current buffer."
  (interactive)
  (when (derived-mode-p 'sdcv-definition-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))
      (cf--clean-ansi-artifacts (point-min) (point-max)))))


;;; Navigation

(defun sdcv-definition-section-next ()
  "Move to next dictionary section."
  (interactive)
  (search-forward sdcv-delimiter nil t 1))

(defun sdcv-definition-section-previous ()
  "Move to previous dictionary section."
  (interactive)
  (search-backward sdcv-delimiter nil t 1))

(defun sdcv-next-definition ()
  "Move to next dictionary definition."
  (interactive)
  (search-forward "-->" nil t 2))

(defun sdcv-previous-definition ()
  "Move to previous dictionary definition."
  (interactive)
  (search-backward "-->" nil t 2))


;;; Eldoc integration

;;;###autoload
(defun sdcv-eldoc-documentation-function (callback)
  "Provide async SDCV dictionary definition at point via CALLBACK.
Returns non-nil if a definition retrieval was started."
  (and-let* ((word (thing-at-point 'word t)))
    (unless sdcv-default-dictionaries
      (cf--sdcv-init-default-dictionaries))
    (let* ((dictionary (or (and (member sdcv-default-dictionary
                                        sdcv-default-dictionaries)
                                sdcv-default-dictionary)
                           (car sdcv-default-dictionaries)))
           (command (list "sdcv" "-n" "--utf8-output" "--color"
                          "-e" "-u" dictionary word)))
      (make-process
       :name "sdcv-eldoc-process"
       :buffer (generate-new-buffer " *sdcv-eldoc*")
       :command command
       :noquery t
       :sentinel
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (and-let* ((buf (process-buffer proc)))
             (unwind-protect
                 (with-current-buffer buf
                   (let ((raw (buffer-string)))
                     (unless (string-blank-p raw)
                       (with-temp-buffer
                         (insert raw)
                         (ansi-color-apply-on-region (point-min) (point-max))
                         (cf--clean-ansi-artifacts (point-min) (point-max))
                         (goto-char (point-min))
                         (while (and (not (eobp))
                                     (looking-at
                                      "\\(?:No magic header\\|Found \\|-->\\|$\\)"))
                           (forward-line 1))
                         (when (re-search-forward "^\\([0-9]+\\)\\s-+\\(.*\\)$" nil t)
                           (let ((lines '()))
                             (push (string-trim (match-string 2)) lines)
                             (while (and (forward-line 1)
                                         (looking-at "^\\s-\\{2,\\}\\S-"))
                               (push (string-trim
                                      (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position)))
                                     lines))
                             (let ((short-def (string-join (nreverse lines) " ")))
                               (funcall callback short-def))))))))
               (kill-buffer buf))))))
      t)))


;;;###autoload
(define-minor-mode sdcv-eldoc-mode
  "Minor mode to show SDCV dictionary definition in Eldoc."
  :lighter " SDCV"
  (if sdcv-eldoc-mode
      (progn
        (add-hook 'eldoc-documentation-functions #'sdcv-eldoc-documentation-function nil t)
        (setq-local eldoc-annotate-documentation-function #'sdcv-eldoc-annotate))
    (remove-hook 'eldoc-documentation-functions #'sdcv-eldoc-documentation-function t)
    (kill-local-variable 'eldoc-annotate-documentation-function)))



;;; _
(provide 'cf-sdcv-definition)
;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:
;;; cf-sdcv-definition.el ends here

;;; init.el --- Emacs init. file -*- lexical-binding:t -*-

;;; Commentary:
;;     Emacs from scratch. [ art ]
;;     LastEdited:   [ 2025-05-08 05:19 ]
;;     LastCompiled: [ 2025-05-08 05:33 ]

;;; Code:

;; Local Custom Package Directory
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom/"))

;; Dedicated Minibuffer Frame
(require 'cf-minibuffer-frame)
(setq default-minibuffer-frame (cf/minibuffer-frame))

(setq default-frame-alist
      (append
       '((minibuffer . nil))
       default-frame-alist))

(setq minibuffer-frame-alist
      '((name . "minibuffer")
        (minibuffer . only)
        (undecorated . t)
        (visibility . nil)
        (skip-taskbar . t)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (internal-border-width . 10)
        (width . 140)
        (height . 1)
        (left . 600)
        (top . 100)))

;; Package Setup
;; (require 'elpaca-setup)
(require 'package)
(package-initialize)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu-devel" . "https://elpa.gnu.org/devel/")
        ("ORG ELPA" . "https://orgmode.org/elpa/")
        ("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/")))

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t
      use-package-compute-statistics t
      use-package-always-ensure nil)

(require 'buffer-move)
(require 'app-launchers)

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start))

  (defun signal-restart-server ()
    "Handler for SIGUSR1 signal, to (re)start an Emacs server.

Can be tested from within Emacs with:
  (signal-process (emacs-pid) \\='sigusr1)

or from the command line with:
  $ kill -USR1 <emacs-pid>
  $ emacsclient -c"
    (interactive)
    (server-force-delete)
    (server-start))
  (define-key special-event-map [sigusr1] 'signal-restart-server)
  (defun cf/restart-server ()
    "Interactively restart the server with \\[signal-restart-server]"
    (interactive)
    (signal-process (emacs-pid) 'sigusr1))
  :bind ("C-c M-k" . cf/restart-server))

;; Garbage C. Magick
(use-package gcmh
  :ensure nil
  :diminish
  :hook after-init
  :init (require 'gcmh)
  :custom (gcmh-verbose nil)
  :config (gcmh-mode 1))

;; gc-buffers
(use-package gc-buffers
  :ensure nil
  :diminish
  :hook gcmh-mode)

(setq initial-scratch-message ";; -*- lexical-binding:t -*-\n\n")


;;; Global Options

;; Display Buffer
(setq display-buffer-alist
      `(("\\*\\(Compile-Log\\|[Hh]elp\\|tldr\\|Messages\\)\\*"
         (display-buffer-reuse-mode-window
          display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.5)
         (window-parameters . ((mode-line-format . none))))

        ("\\*\\(scratch\\|.* scratch\\|vterm\\)\\*"
         (display-buffer-reuse-window
          display-buffer-below-selected
          display-buffer-in-direction)
         (body-function . select-window)
         (direction . right)
         (slot . 1)
         (window-height . 0.3)
         (window-width . 0.5)
         (window-parameters . ((mode-line-format . none))))

        ("\\*eww\\*"
         (display-buffer-reuse-window
          display-buffer-in-direction)
         (body-function . select-window)
         (window-width . 0.5)
         (direction . right)
         (slot . 0)
         (window-height . 0.5)
         (direction . bottom)
         (slot . -1)
         (window-parameters . ((header-line-format . none)
                               (mode-line-format . none))))

        ("\\*\\(Ibuffer\\|Colors\\)*"
         (display-buffer-reuse-mode-window
          display-buffer-in-direction
          display-buffer-in-side-window)
         (body-function . select-window)
         (direction . right)
         (slot . 1)
         (window-height . 0.3)
         (window-width . 0.5)
         (window-parameters . ((mode-line-format .  none))))

        ("\\*Occur\\*"
         (display-buffer-reuse-mode-window
          display-buffer-in-side-window)
         (side . bottom)
         (slot . 1)
         (window-height . 0.3)
         (window-parameters . ((mode-line-format . (" " "%b")))))

        ("\\*Completions\\*"
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . bottom)
         (slot . 1)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((mode-line-format . none))))

        ("\\*ielm\\*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t)
         (reusable-frames . visible)
         (window-height . 10)
         (window-parameters . ((mode-line-format . none))))

        ("^\\*\\(\\(\\(Summary\\|Group\\) \\|\\)\\|^\\*\\)\\*"
         (display-buffer-reuse-window display-buffer-at-bottom display-buffer-in-previous-window))

        (".*nn\\(rss\\|ml\\|ml\\.+[a-z0-9-]+\\)\\.org/.*\\.article\\*.*"
         (display-buffer-reuse-window display-buffer-below-selected))

        ("\\*\\(Backtrace\\|Warnings\\|Disabled Command\\|mu4e-last-update\\|Async-native-compile-log\\)\\*"
         (display-buffer-no-window)
         (inhibit-same-window . t)
         (allow-no-window . t))))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(global-font-lock-mode 1)

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'gruber-darker t)

(set-face-attribute 'mode-line nil :family "Iosevka Nerd Font Mono" :foreground "#696969" :background "#282828")
(set-face-attribute 'mode-line-inactive nil :family "Iosevka Nerd Font Mono" :foreground "#484848" :background "#282828")

(setq-default blink-cursor-blinks 2
              window-divider-default-places 'bottom-only
              window-divider-default-bottom-width 1)

(defun cf-theme-settings ()
  "(re-)Sets default theming settings."
  (interactive)
  (setq blink-cursor-blinks 2
        window-divider-default-places 'bottom-only
        window-divider-default-bottom-width 1)
  (window-divider-mode 1)
  (let* ((sep1 (propertize "" 'face '(:foreground "#282828" :background "#282828" :box (:line-width (0 . -1) :color "#181818" :style none))))
         (sep2 (propertize "" 'face '(:foreground "#282828" :background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))))
         (sep3 (propertize "" 'face '(:foreground "#181818" :background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))))
         (sep4 (propertize "" 'face '(:foreground "#282828" :background "#181818" :box (:line-width (0 . -1) :color "#181818"))))
         (inner (propertize "   %b  "
                            'face '(:background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))
                            'local-map mode-line-buffer-identification-keymap)))
    (setq-default mode-line-buffer-identification (list sep1 sep2 inner sep3 sep4)))

  (setq-default mode-line-format
                `("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  mode-line-modes
                  mode-line-input-method-map
                  mode-line-format-right-align 'window
                  mode-line-bury-buffer
                  mode-line-misc-info
                  (:eval (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line mouse-1]
                                       (lambda ()
                                         (interactive)
                                         (when (use-region-p)
                                           (copy-region-as-kill (region-beginning) (region-end))
                                           (message "Region copied to the kill ring."))))
                           (propertize (format " [%d:%d] " (line-number-at-pos) (current-column))
                                       'face '(:foreground "#594949")
                                       'mouse-face 'mode-line-highlight
                                       'help-echo "mouse-1: Copy region"
                                       'local-map map)))
                  (:eval (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line mouse-1]
                                       (lambda () (interactive)
                                         (view-mode 'toggle)))
                           (define-key map [mode-line mouse-2]
                                       (lambda () (interactive)
                                         (eval-buffer)))
                           (define-key map [mode-line mouse-3]
                                       (lambda () (interactive)
                                         (save-buffer)))
                           (propertize
                            (cond
                             (buffer-read-only
                              " [RO] ")
                             ((buffer-modified-p)
                              " [+] ")
                             (t
                              " [-] "))
                            'face (cond
                                   (buffer-read-only '(:foreground "#ac2c1d"))
                                   ((buffer-modified-p) '(:foreground "#cb9c3c"))
                                   (t '(:foreground "#484848")))
                            'mouse-face 'mode-line-highlight
                            'help-echo "mouse-1: Toggle view-mode\nmouse-2: Eval buffer\nmouse-3: Save buffer"
                            'local-map map)))
                  (:eval (propertize (format-time-string "[%H:%M] ")
                                     'face '(:foreground "#696969")
                                     'help-echo (format-time-string "%c")))
                  mode-line-end-spaces)))
(add-hook 'after-init-hook #'cf-theme-settings)

(defvar-local cf--saved-header-line-format nil)
(defvar-local cf--saved-mode-line-format nil)

(define-minor-mode cf/mode-line-hide-mode
  "Minor mode to toggle visibility of mode-line and header-line in the current buffer."
  :init-value nil
  :lighter ""
  :global nil
  (if cf/mode-line-hide-mode
      (progn
        (unless cf--saved-header-line-format
          (setq cf--saved-header-line-format header-line-format))
        (unless cf--saved-mode-line-format
          (setq cf--saved-mode-line-format mode-line-format))
        (setq mode-line-format nil
              header-line-format nil))
    (setq mode-line-format cf--saved-mode-line-format
          header-line-format cf--saved-header-line-format))
  (force-mode-line-update)
  (redraw-display))

(defun cf/toggle-mode-line ()
  "Toggle `cf/mode-line-hide-mode' minor mode."
  (interactive)
  (if cf/mode-line-hide-mode
      (cf/mode-line-hide-mode -1)
    (cf/mode-line-hide-mode 1)))

(add-to-list 'save-some-buffers-action-alist
             (list "d" (lambda (buffer)
                         (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

(setq revert-without-query
      '(".*")
      backup-directory-alist
      '((".*" . "~/.emacs.d/backups/")))
(global-auto-revert-mode 1)



;;; Theme

;; Nerd Icons
(use-package nerd-icons
  :ensure t
  :config
  (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
    (nerd-icons-install-fonts t)))

(use-package nerd-icons-dired
  :ensure t
  :diminish
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-ibuffer
  :ensure t)

(use-package nerd-icons-ivy-rich
  :ensure t
  :after ivy)

(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy-rich
  :diminish
  :init (all-the-icons-ivy-rich-mode 1))

;; Dash
(use-package dash
  :ensure t
  :config
  ;; (eww "https://github.com/magnars/dash.el")
  ;; (info "(emacs) Info Lookup")
  (with-eval-after-load 'info-look
    (dash-register-info-lookup))
  (global-dash-fontify-mode 1))

;; Highlight TODO
(use-package hl-todo
  :ensure nil
  :hook
  ((org-mode  . hl-todo-mode)
   (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))


;;; Builtins

;; Minibuffer
(use-package minibuffer
  :ensure nil
  :custom
  (y-or-n-p-use-read-key t)
  (max-mini-window-height 8)
  (minibuffer-visible-completions t)
  (minibuffer-visible-completions--always-bind t)
  (minibuffer-allow-text-properties t)
  (minibuffer-auto-raise t)
  (minibuffer-scroll-window t)
  (enable-recursive-minibuffers nil)
  (completion-eager-display t)
  :bind
  (:map minibuffer-mode-map
        ("C-p"        . minibuffer-previous-line-completion)
        ("C-n"        . minibuffer-next-line-completion)
        ("C-y"        . minibuffer-complete-and-exit)
        ("M-<return>" . minibuffer-force-complete-and-exit)
        :map minibuffer-local-map
        ("<up>"       . previous-history-elememt)
        ("<down>"     . next-history-element)
        ("<right>"    . minibuffer-complete-word)
        ("ESC"        . abort-recursive-edit)
        ("<escape>"   . abort-recursive-edit)
        ("<tab>"      . minibuffer-complete-defaults)
        ("TAB"        . minibuffer-complete-defaults)
        ("<return>"   . minibuffer-complete-and-exit)
        ("C-<tab>"    . minibuffer-complete-word)
        ("C-i"        . minibuffer-complete-word)
        ("C-y"        . minibuffer-complete-and-exit)
        ("M-<return>" . minibuffer-force-complete-and-exit)
        :map minibuffer-visible-completions-map
        ("<down>"     . minibuffer-next-history-element)
        ("<up>"       . minibuffer-previous-history-element)
        ("<backtab>"  . minibuffer-previous-completion)
        ("S-TAB"      . minibuffer-previous-completion)
        ("<tab>"      . minibuffer-next-completion)
        ("TAB"        . minibuffer-next-completion)
        ("C-p"        . minibuffer-previous-line-completion)
        ("C-n"        . minibuffer-next-line-completion)
        ("C-y"        . minibuffer-complete-and-exit)
        ("<return>"   . minibuffer-choose-completion-or-exit)
        ("RET"        . minibuffer-choose-completion-or-exit)
        ("M-<return>" . minibuffer-force-complete-and-exit)
        ("C-<escape>" . minibuffer-hide-completions)
        ))

;; simple
(use-package simple
  :ensure nil
  :custom
  (case-fold-search t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (kill-read-only-ok t)
  (blink-matching-paren t)
  (indent-tabs-mode nil)
  (column-number-mode t)
  :bind
  (:map global-map
        ("M-o"        . split-line)
        ("M-O"        . delete-blank-lines)
        ("M-_"        . delete-indentation)
        ("M-+"        . open-line)
        ("C-o"        . open-line)
        ("M-SPC"      . cycle-spacing)
        ("C-v"        . cf/scroll-up-and-recenter)
        ("M-v"        . cf/scroll-down-and-recenter))
  :config
  (defun cf/message-killed-text (&rest _args)
    "Display the latest killed text in the echo area."
    (when (stringp (current-kill 0 t))
      (message (format "%s" (current-kill 0 t)))))

  (dolist (fn '(kill-region
                kill-line
                kill-whole-line
                kill-ring-save
                kill-sexp
                kill-word
                backward-kill-word
                backward-kill-sexp))
    (advice-add fn :after #'cf/message-killed-text))
  ;; (advice-remove 'indent-region "indent-region-delete-trailing-ws")
  (advice-add 'indent-region
              :after (lambda (&rest _) (delete-trailing-whitespace))
              '((name . "indent-region-delete-trailing-ws"))))

;; find-at-point
(use-package ffap
  :ensure nil
  :init
  (require 'thingatpt)
  (require 'ffap)
  :custom
  (ffap-rfc-path "~/Documents/rfc/")
  :config
  (ffap-bindings))

;; :vm:View Mode
(use-package view
  :ensure nil
  :diminish " "
  :hook (woman-mode Man-mode help-mode shortdoc-mode)
  :init
  (require 'sdcv-definition)
  (require 'multiple-cursors)
  :bind
  (:map view-mode-map
        ("d" . cf/scroll-up-and-recenter)
        ("u" . cf/scroll-down-and-recenter)
        ("n" . forward-line)
        ("p" . previous-line)
        ("N" . mc/mark-next-like-this)
        ("P" . mc/mark-previous-like-this)
        ("b" . backward-char)
        ("k" . kill-line)
        ("f" . forward-char)
        ("v" . scroll-up-command)
        ("l" . recenter-top-bottom)
        ("c" . cf/copy-line-or-region)
        ("w" . forward-word)
        ("o" . backward-word)
        ("i" . crux-move-beginning-of-line)
        ("x" . isearch-forward-symbol-at-point)
        ("X" . cf/exact-lookup-word)
        ("," . cf/lookup-word)
        ("." . xref-find-definitions-other-window)
        ("e" . forward-sentence)
        ("a" . backward-sentence)
        ("]" . forward-paragraph)
        ("[" . backward-paragraph)
        (";" . er/expand-region)
        ("m" . cf/mark-construct-dwim))
  :config
  (display-line-numbers-mode -1)
  (whitespace-mode -1)
  (add-hook 'view-mode-hook
            (lambda ()
              (setq olivetti-body-width (window-body-width))))
 (add-hook 'xref-after-jump-hook #'view-mode))

;; (defvar cf-common-url-regexp
;;   (concat
;;    "~?\\<\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]*\\)"
;;    "[.@]"
;;    "\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]+\\)\\>/?")
;;   "Regular expression to match (most?) URLs or email addresses.")

;; isearch
(use-package isearch
  :ensure nil
  :diminish " ⌕"
  :init
  (require 'thingatpt)
  (require 'ffap)
  :config
  ;; From prot's dotemacs (eww "https://protesilaos.com/emacs/dotemacs")
  ;; (find-file "~/.local/eww/protesilaos.com/prot-dotemacs.html")
  (setq search-whitespace-regexp ".*")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (defun cf/query-replace-regexp-at-point ()
    "Use region or word at point as default regexp in `query-replace-regexp`."
    (interactive)
    (let* ((text (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'word t))))
      (unless (and text (not (string-blank-p text)))
        (user-error "No valid region or word at point"))
      (let ((from (regexp-quote text)))
        (call-interactively
         (lambda (from-regexp to-string &optional delimited start end)
           (interactive
            (let ((from from))
              (list
               (read-regexp "Query replace regexp" from)
               (read-string (format "Replace %s with: " from))
               current-prefix-arg
               (when (use-region-p) (region-beginning))
               (when (use-region-p) (region-end)))))
           (perform-replace from-regexp to-string t t delimited start end))))))

  (defun cf/isearch-mark-and-exit ()
    "Marks the current search-string.  Can be used as a building
block for a more complex chain, such as to kill a region, or
place multiple cursors."
    (push-mark isearch-other-end t 'activate)
    (setq deactivate-mark nil)
    (isearch-done))

  (defun cf/isearch-repeat-forward (&optional arg)
    "Move forward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
    (interactive "p")
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end))
    (isearch-repeat-forward (or arg 1)))

  (defun cf/isearch-repeat-backward (&optional arg)
    "Move backward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
    (interactive "p")
    (when (and (not isearch-forward) isearch-other-end)
      (goto-char isearch-other-end))
    (isearch-repeat-backward (or arg 1)))

  (defun cf/isearch-other-end ()
    "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
    (interactive)
    (isearch-done)
    (when isearch-other-end
      (goto-char isearch-other-end)))

  (defun cf/isearch-abort-dwim ()
    "Delete failed `isearch' input, single char, or cancel search.

This is a modified variant of `isearch-abort' that allows us to
perform the following, based on the specifics of the case: (i)
delete the entirety of a non-matching part, when present; (ii)
delete a single character, when possible; (iii) exit current
search if no character is present and go back to point where the
search started."
    (interactive)
    (if (eq (length isearch-string) 0)
        (isearch-cancel)
      (isearch-del-char)
      (while (or (not isearch-success) isearch-error)
        (isearch-pop-state)))
    (isearch-update))

  (require 'goto-addr)
  (require 'button)

  (defvar cf-common-url-regexp
    (concat
     "\\b\\(?:https?\\|ftp\\|file\\|mailto\\):"
     "//?[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]")
    "Regexp to match most URLs including common schemes.")

  (defun cf/occur-urls ()
    "Show a list of unique, buttonized URLs in the current buffer.
Trailing periods are removed before deduplication to normalize sentence-final punctuation."
    (interactive)
    (let* ((seen (make-hash-table :test 'equal))
           (urls (save-excursion
                   (goto-char (point-min))
                   (let (matches)
                     (while (re-search-forward cf-common-url-regexp nil t)
                       (let* ((raw-url (match-string-no-properties 0))
                              (url (if (string-suffix-p "." raw-url)
                                       (substring raw-url 0 -1)
                                     raw-url)))
                         (unless (gethash url seen)
                           (puthash url t seen)
                           (push url matches))))
                     (nreverse matches))))
           (buf-name (format "*links in <%s>*" (buffer-name)))
           (buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (dolist (url urls)
          (insert url "\n"))
        (goto-address-mode 1)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((start (point)))
            (forward-line 1)
            (unless (next-single-property-change start 'button nil (line-end-position))
              (delete-region start (point)))))
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer buf)))

  (defun cf/occur-browse-url ()
    "Point browser at a URL in the buffer using completion.
Which web browser to use depends on the value of the variable
`browse-url-browser-function'.

Also see `cf/occur-urls'."
    (interactive)
    (let ((matches nil))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp cf-common-url-regexp nil t)
          (push (match-string-no-properties 0) matches)))
      (funcall browse-url-browser-function
               (completing-read "Browse URL: " matches nil t))))
  :bind
  (("C-s"    . isearch-forward)
   ("M-r"    . isearch-backward)
   ("M-["    . isearch-backward)
   ("M-^"    . isearch-backward-regexp)
   ("M-#"    . isearch-forward-regexp)
   ("M-&"    . cf/query-replace-regexp-at-point)
   ("M-*"    . isearch-forward-symbol-at-point)
   ("M-]"    . isearch-forward-thing-at-point)
   ("M-\\"   . cf/occur-browse-url)
   ("C-M-\\" . cf/occur-urls)
   :map isearch-mode-map
   ("C-s"        . cf/isearch-repeat-forward)
   ("C-r"        . cf/isearch-repeat-backward)
   ("C-<return>" . cf/isearch-other-end)
   ("C-<escape>" . cf/isearch-abort-dwim))
)

;; replace
(use-package replace
  :ensure nil
  :custom
  (list-matching-lines-jump-to-current-line t))

;; display-line-numbers-mode
(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-width-start t)
  (header-line-indent-width 4)
  :config
  (add-hook 'display-line-numbers-mode-hook 'header-line-indent-mode))

;; Whitespace
(use-package whitespace
  :ensure nil
  :diminish
  :custom
  (whitespace-display-mappings
   '((space-mark   ?\     [?·]     [?.])
     (newline-mark ?\n    [?¬ ?\n]  [?$ ?\n])
     (tab-mark     ?\t    [?» ?\t] [?\\ ?\t]))))

;; Mouse & Drag and Drop
(use-package mouse
  :ensure nil
  :custom
  (mouse-drag-copy-region t)
  (mouse-yank-at-point t)
  (mouse-drag-and-drop-region "shift")
  (mouse-drag-and-drop-show-cursor t)
  (mouse-drag-and-drop-show-tooltip t)
  (mouse-drag-and-drop-region-cross-program t)
  :bind
  (:map global-map
        ("<down-mouse-2>"     . mouse-drag-drag)
        ("<mouse-3>"          . er/expand-region)
        ("<mouse-2>"          . mouse-yank-primary)
        ("S-<mouse-2>"        . mouse-yank-clipboard)
        ("C-<mouse-2>"        . eww-search-words)
        ("C-M-<mouse-1>"      . cf/lookup-word)
        ("C-M-<mouse-2>"      . dictionary-lookup-definition)
        ("<mouse-8>"          . kill-ring-save)
        ("<mouse-9>"          . eww-search-words)
        ("M-S-<mouse-2>"      . mouse-yank-primary)
        ("<down-mouse-1>"     . mouse-drag-region)
        ("S-<down-mouse-1>"   . mouse-drag-and-drop-region)
        ("C-<down-mouse-1>"   . mouse-drag-and-drop-region)
        ("M-S-<down-mouse-1>" . mouse-drag-region-rectangle)
        :map eww-mode-map
        ("<mouse-1>"        . eww-follow-link))
  :config
  (require 'eww)
  (setq track-mouse nil))

;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind ("C-x b" . ibuffer-other-window))

;; Save History
(use-package savehist
   :ensure nil
   :custom
   (savehist-additional-variables
    '(command-history))
   :config
   (savehist-mode))

;; Electric
(use-package electric
  :ensure nil
  :custom
  (delete-pair-blink-delay 0)
  (delete-pair-push-mark t) ; v:31.1:
  :config
  (electric-indent-mode -1)
  (electric-pair-mode 1)
  (setq electric-delete-pair t)
  (setq delete-pair-push-mark t)
  (defun cf/delete-pair-copy-region-as-kill ()
    "Delete the nearest pair of matching characters.
When `delete-pair-push-mark' is t, also kills the region's sexp.
If not at a matching pair, move to the start of the containing sexp first."
    (interactive)
    (let ((syntax (syntax-ppss)))
      (if (and (not (nth 1 syntax))
               (not (looking-at-p (regexp-opt '("(" "{" "[" "\"" "'" "<")))))
          (backward-sexp))
      (when (looking-at-p (regexp-opt '("(" "{" "[" "\"" "'" "<")))
        (delete-pair)
        (copy-region-as-kill (region-beginning) (region-end)))
      (message "%s" (current-kill 0))))

  (defun mark-sexp-region ()
    "Mark the previous sexp, highlight the region, and copy it.
Leaves point after the sexp and region active."
    (interactive)
    (let ((end (point))
          (start (save-excursion (backward-sexp) (point))))
      (goto-char end)
      (push-mark start nil t)
      (copy-region-as-kill start end))
    (exchange-point-and-mark)
    (message "%s" (current-kill 0))
    (exchange-point-and-mark))
  :bind
  (("C-M-{" . mark-sexp-region)
   ("C-M-}" . cf/delete-pair-copy-region-as-kill)))

;; Delete Selection
(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode 1))

;; Outline
(use-package outline
  :ensure nil
  :diminish (outline-minor-mode . " 🞃")
  :custom (outline-minor-mode-cycle t)
  :config (outline-minor-mode 1))

;; ElDoc
(use-package eldoc
  :ensure nil
  :diminish
  :config
  (global-eldoc-mode 1))

;; ElDoc-Box
(use-package eldoc-box
  :ensure nil
  :after eldoc
  :hook (global-eldoc-mode . eldoc-box-hover-mode)
  :diminish (eldoc-box-hover-mode . "ElDoc")
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-only-multi-line nil)
  (eldoc-box-fringe-use-same-bg t)
  (eldoc-box-cleanup-interval 7)
  (eldoc-box-max-pixel-width 1200)
  (eldoc-box-max-pixel-height 1000)
  :bind ("C-c C-h" . eldoc-box-help-at-point)
  :config
  (require 'eldoc-box)
  (set-face-attribute 'eldoc-box-border nil :background "#181818")
  (set-face-attribute 'eldoc-box-body nil :background "#181818")
  (defun cf/eldoc-eshell-box ()
    "Display function or command in `eldoc-box' in `eshell-mode'."
    (when (eq major-mode 'eshell-mode)
      (let ((sym (or (eldoc-fnsym-in-current-sexp)
                     (save-excursion
                       (goto-char eshell-last-output-end)
                       (eldoc-current-symbol)))))
        (when sym
          (let ((doc (eldoc-get-fnsym-args-string sym)))
            (when doc
              (eldoc-box-message doc)))))))
  (add-hook 'post-command-hook #'cf/eldoc-eshell-box)
  (setq eldoc-box-self-insert-command-list
        '(self-command-insert outshine-self-insert-command))
  (eldoc-box-hover-mode 1))

;; ido
(use-package ido
  :ensure nil
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  :config
  (ido-mode 1))



;;; Functions

(use-package prot-scratch
  :ensure nil
  :load-path "~/.emacs.d/custom/"
  :init
  (require 'prot-common)
  (require 'prot-scratch)
  :bind ("C-c s" . prot-scratch-buffer))

;;; Maximise window + kill buffer (and close window)
;; `cf/window-single-toggle' is based on `windower' by Pierre
;; Neidhardt (ambrevar on GitLab)
(use-package emacs
  :ensure nil
  :config
  (defvar cf/window-configuration nil
    "Current window configuration.
Intended for use by `cf/window-monocle'.")

  (define-minor-mode cf/window-single-toggle
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as \\='monocle\\='."
    :lighter " [M]"
    :global nil
    (if (one-window-p)
        (when cf/window-configuration
          (set-window-configuration cf/window-configuration))
      (setq cf/window-configuration (current-window-configuration))
      (delete-other-windows)))

  (defun cf/kill-buffer-current (&optional arg)
    "Kill current buffer or abort recursion when in minibuffer."
    (interactive "P")
    (if (minibufferp)
        (abort-recursive-edit)
      (kill-buffer (current-buffer)))
    (when (and arg
               (not (one-window-p)))
      (delete-window)))
  :bind (("C-c w" . cf/window-single-toggle)
         ("C-c k" . cf/kill-buffer-current)))

;; Marking constructs :: From protesilaos' backlog
(use-package emacs
  :ensure nil
  :commands (cf/mark-symbol
             cf/mark-sexp-backward)
  :config
  (defmacro cf/mark (name object &optional docstring)
    "Produce function for marking small syntactic constructs.
NAME is how the function should be called.  OBJECT is its scope.
Optional DOCSTRING describes the resulting function.

This is a slightly modified version of the built-in `mark-word'."
    `(defun ,name (&optional arg allow-extend)
       ,docstring
       (interactive "P\np")
       (let ((x (format "%s-%s" "forward" ,object)))
         (cond ((and allow-extend
                     (or (and (eq last-command this-command) (mark t))
                         (region-active-p)))
                (setq arg (if arg (prefix-numeric-value arg)
                            (if (< (mark) (point)) -1 1)))
                (set-mark
                 (save-excursion
                   (goto-char (mark))
                   (funcall (intern x) arg)
                   (point))))
               (t
                (let ((bounds (bounds-of-thing-at-point (intern ,object))))
                  (unless (consp bounds)
                    (user-error "No %s at point" ,object))
                  (if (>= (prefix-numeric-value arg) 0)
                      (goto-char (car bounds))
                    (goto-char (cdr bounds)))
                  (push-mark
                   (save-excursion
                     (funcall (intern x) (prefix-numeric-value arg))
                     (point)))
                  (activate-mark)))))))

  (cf/mark
   cf/mark-word
   "word"
   "Mark the whole word at point.
This function is a slightly modified version of the built-in
`mark-word', that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required.  For a general purpose utility, use `cf/mark-symbol'
instead.")

  (cf/mark
   cf/mark-symbol
   "symbol"
   "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining
ARGth symbols away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next
symbol in the direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

  (defun cf/mark-sexp-backward (&optional arg)
    "Mark previous or ARGth balanced expression[s].
Just a convenient backward-looking `mark-sexp'."
    (interactive "P")
    (if arg
        (mark-sexp (- arg) t)
      (mark-sexp (- 1) t)))

  (defun cf/mark-construct-dwim (&optional arg)
    "Mark symbol or balanced expression at point.
A do-what-I-mean wrapper for `cf/mark-sexp-backward',
`mark-sexp', and `cf/mark-symbol'.

When point is over a symbol, mark the entirety of it.  Regular
words are interpreted as symbols when an actual symbol is not
present.

For balanced expressions, a backward match will happen when point
is to the right of the closing delimiter.  A forward match is the
fallback condition and should work when point is before a
balanced expression, with or without whitespace in between it an
the opening delimiter.

Optional ARG will mark a total of ARGth objects while counting
the current one (so 3 would be 1+2 more).  A negative count moves
the mark backward (though that would invert the backward-moving
sexp matching of `cf/mark-sexp-backward', so be mindful of
where the point is).  Repeated invocations of this command
incrementally mark objects in the direction originally
specified."
    (interactive "P")
    (cond
     ((symbol-at-point)
      (cf/mark-symbol arg t))
     ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (cf/mark-sexp-backward arg))
     (t
      (mark-sexp arg t))))
  :bind (("M-@" . cf/mark-word)       ; replaces `mark-word'
         ("C-M-SPC" . cf/mark-construct-dwim)
         ("C-x C-k" . cf/kill-buffer-current)))

(defun cf/reset-display-table ()
  "Interactively resets `buffer-display-table'."
  (interactive)
  (setq buffer-display-table nil)
  (cf-theme-settings))

;; Region to linkhandler
(defun cf/region-to-linkhandler (&optional buffer)
  "Send input region, marked file or BUFFER to linkhandler script.
- If BUFFER prefix argument, use the current buffer as playlist.
- Handles other MIME-type redirections to pre-defined applications.
- Extracts valid links using regexp."
  (interactive "Pr")
  (let* ((content
          (cond
           ((use-region-p)
            (split-string (buffer-substring-no-properties (region-beginning) (region-end)) "\n" t))
           ((derived-mode-p 'dired-mode)
            (dired-get-marked-files))
           (t
            (let* ((current-line (thing-at-point 'line t))
                   (urls (when current-line
                           (seq-filter
                            (lambda (s) (string-match-p "https?://" s))
                            (split-string current-line)))))
              (if urls
                  urls
                (let ((buffer-content (buffer-string)))
                  (seq-filter
                   (lambda (s) (string-match-p "https?://" s))
                   (split-string buffer-content "\n" t))))))))
         (grouped-content (string-join content "\n")))
    (if buffer
        (start-process "linkhandler-process" "*linkhandler-output*" "linkhandler" grouped-content)
      (start-process "linkhandler-process" nil "linkhandler" grouped-content))
    (message "linkhandler: %s" grouped-content)))

;; Url at point to linkhandler
(defun cf/link-handler ()
  "Handle link at point with linkhandler script."
  (interactive)
  (start-process-shell-command
   "linkhandler" nil (format "linkhandler '%s'" (thing-at-point 'url))))

;; Keyboard Quit
(defun cf/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.
The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:
- When the region, is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (deactivate-mark))
   ((bound-and-true-p isearch-mode)
    (isearch-exit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))
(keymap-global-set "C-g" #'cf/keyboard-quit-dwim)
(keymap-global-set "<escape>" #'keyboard-quit)
(keymap-global-set "C-<escape>" #'keyboard-escape-quit)

;; Basic Navigation
(defun cf/scroll-down-and-recenter (&optional count)
  "Scroll down and recenter. If COUNT is specified, scroll COUNT lines down."
  (interactive "P")
  (if (numberp count)
      (forward-line (- count))
    (scroll-down-command))
  (recenter))

(defun cf/scroll-up-and-recenter (&optional count)
  "Scroll up and recenter. If COUNT is specified, scroll COUNT lines up."
  (interactive "P")
  (if (numberp count)
      (forward-line count)
    (scroll-up-command))
  (recenter))

(defun cf/split-and-follow-horizontally (&optional no-follow)
  "Split Window horizontally and enter the new split.
Optional prefix arguments means NO-FOLLOW"
  (interactive "P")
  (split-window-below)
  (balance-windows)
  (unless no-follow
    (other-window 1)))
(keymap-global-set "C-x 2" #'cf/split-and-follow-horizontally)

(defun cf/split-and-follow-vertically (&optional no-follow)
  "Split Window vertically and enter the new split.
Optional prefix argument means NO-FOLLOW."
  (interactive "P")
  (split-window-right)
  (balance-windows)
  (unless no-follow
    (other-window 1)))
(keymap-global-set "C-x 3" #'cf/split-and-follow-vertically)

(defun cf/kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (mapc (lambda (buf)
          (unless (eq buf (current-buffer))
            (kill-buffer buf)))
        (buffer-list)))
(keymap-global-set "C-c M-1" #'cf/kill-other-buffers)

(defun cf/kill-all-buffers ()
  "Kills all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun create-buffer (&optional window)
  "Create a new buffer in WINDOW direction and switch to it.
Specify split direction with prefix WINDOW argument:
  - 1: Use whole frame (delete other windows).
  - 2: Split horizontally.
  - 3: Split vertically.
  - 4: Create a new frame."
  (interactive "P")
  (let ((buffer (generate-new-buffer "*new*")))
    (cond
     ((equal window 1)
      (delete-other-windows)
      (switch-to-buffer buffer))
     ((equal window 2)
      (split-window-below)
      (other-window 1)
      (switch-to-buffer buffer))
     ((equal window 3)
      (split-window-right)
      (other-window 1)
      (switch-to-buffer buffer))
     ((equal window 4)
      (let ((new-frame (make-frame '((name . "*new*")
                                     (width . 80)
                                     (height . 24)))))
        (select-frame-set-input-focus new-frame)
        (switch-to-buffer buffer)))
     (t
      (split-window-right)
      (other-window 1)
      (switch-to-buffer buffer)))))
(keymap-global-set "C-c n" #'create-buffer)

;; TTS
(defun cf/send-region-to-tts (start end)
  "Sends marked region START to END to text-to-speech handler for playback."
  (interactive "r")
  (let ((region-text (buffer-substring-no-properties start end)))
    (start-process-shell-command
     "tts" nil
     (format "echo %s | tts"
             (shell-quote-argument region-text)))))
(keymap-global-set "C-c t" #'cf/send-region-to-tts)

;; Timestamp
(defun cf/insert-locale-timestamp-at-point (&optional format)
  "Insert the current locale timestamp FORMAT at point.
If prefix ARG, insert `#+date:` before the timestamp."
  (interactive "P")
  (if format (insert (format-time-string "#+date: <%Y-%m-%d %H:%M>" (current-time)))
    (insert (format-time-string "%Y-%m-%d %H:%M" (current-time)))))
(keymap-global-set "M-<f1>" #'cf/insert-locale-timestamp-at-point)

;; Kill Ring Yanka
(defun insert-kill-ring-item ()
  "Insert item from `kill-ring', selected with completion."
  (interactive)
  (let ((filtered-kill-ring (seq-filter (lambda (s) (not (string-empty-p s))) kill-ring)))
    (insert (completing-read "Yank: " filtered-kill-ring nil t))))
(keymap-global-set "C-c C-y" #'insert-kill-ring-item)

;; yank-pop-forward
(defun undo-yank (arg)
  "Undo the yank you just did.  Really, adjust just-yanked text
like \\[yank-pop] does, but in the opposite direction."
  (interactive "p")
  (yank-pop (- arg)))
(keymap-global-set "C-M-y" 'undo-yank)

;; Insert palette color at point
(defun cf/insert-palette-color (&optional show)
  "Insert a hex color value from a selected palette.
With prefix SHOW argument, prompt to select a defined palette.
Displays color names with their hex color in the completion menu."
  (interactive "P")
  (let* ((default-palette
          '(("fg" . "#e4e4ef") ("fg+1" . "#f4f4ff") ("fg+2" . "#f5f5f5")
            ("white" . "#ffffff") ("black" . "#000000") ("bg-1" . "#101010")
            ("bg" . "#181818") ("bg+1" . "#282828") ("bg+2" . "#453d41")
            ("bg+3" . "#484848") ("bg+4" . "#52494e") ("red-1" . "#c73c3f")
            ("red" . "#f43841") ("red+1" . "#ff4f58") ("green" . "#73c936")
            ("yellow" . "#ffdd33") ("brown" . "#cb9c3c") ("quartz" . "#95a99f")
            ("niagara-2" . "#303540") ("niagara-1" . "#565f73") ("niagara" . "#96a6c8")
            ("wisteria" . "#9e95c7")))
         (solarized-palette
          '(("base03" . "#002b36") ("base02" . "#073642")
            ("base01" . "#586e75") ("base00" . "#657b83")
            ("yellow" . "#b58900") ("blue" . "#268bd2")
            ("green" . "#859900")))
         (x-colors-palette
          (mapcar (lambda (color)
                    (cons color (apply #'format "#%02x%02x%02x"
                                       (mapcar (lambda (c) (/ c 256))
                                               (color-values color)))))
                  (defined-colors))))
    (let* ((palettes `(("Gruber Darker" . ,default-palette)
                       ("Solarized"     . ,solarized-palette)
                       ("X Colors"      . ,x-colors-palette)))
           (palette (if show
                        (cdr (assoc (completing-read "Select palette: " (mapcar #'car palettes) nil t)
                                    palettes))
                      (list-colors-display)))
           (formatted-choices
            (cl-loop for (name . hex) in palette
                     collect (cons (propertize
                                    (format " %-12s  %s " name hex)
                                    'face `(:background ,hex :foreground "black"))
                                   hex))))
      (when formatted-choices
        (let* ((choice (completing-read "Select color: " (mapcar #'car formatted-choices) nil t))
               (color-hex (cdr (assoc choice formatted-choices))))
          (when color-hex
            (insert color-hex)))))))

;; Async Run
(defun cf/async-run (&optional name)
  "Run a process asynchronously by NAME, with completion from `dmenu-items'."
  (interactive
   (list (completing-read "Run process: "
                          (with-temp-buffer
                            (insert-file-contents "~/.emacs.d/dmenu-items")
                            (read (current-buffer))))))
  (unless (string-empty-p name)
    (start-process name nil name)))

;; Config Edit
(defun cf/config-edit ()
  "Jump to `init.el' configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(keymap-global-set "C-c e" #'cf/config-edit)

(defun cf/config-reload ()
  "Reload `init.el' configuration."
  (interactive)
  (load-file  "~/.emacs.d/init.el")
  (message "c'fait."))
(keymap-global-set "C-c R" #'cf/config-reload)

(defvar last-evaluated-expression nil
  "Holds the last evaluated expression.")

(defvar last-evaluated-result nil
  "Holds the result of the last evaluated expression.")

(defvar evaluated-expressions-alist nil
  "Alist to store all evaluated expressions and their results.")

(defun region-eval (start end)
  "Evaluate the region between START and END.
If no region is active, evaluate the entire buffer using `eval-buffer`.
Store the expression and result for later use in `evaluated-expressions-alist`."
  (interactive "r")
  (let ((expression (if (use-region-p)
                        (buffer-substring-no-properties start end)
                      (buffer-substring-no-properties (point-min) (point-max)))))
    (setq last-evaluated-expression expression)
    (setq last-evaluated-result (eval (read expression)))
    (add-to-list 'evaluated-expressions-alist
                 (cons expression last-evaluated-result) t)
    (message "eval: %s => %s" last-evaluated-expression last-evaluated-result)))

(defun show-evaluated-expressions ()
  "Display all evaluated expressions and their results."
  (interactive)
  (if evaluated-expressions-alist
      (dolist (entry evaluated-expressions-alist)
        (message "Expression: %s => Result: %s" (car entry) (cdr entry)))
    (message "No evaluated expressions recorded.")))
(keymap-set emacs-lisp-mode-map "C-c r e" #'region-eval)

(defun eval-and-replace (&optional insert)
  "Evaluate and replace the expression preceding point or the active region.
Moves to the end of the current sexp if necessary.
If INSERT (prefix argument), insert value after the sexp without replacing."
  (interactive "P")
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (expr (read (buffer-substring-no-properties beg end)))
             (value (eval expr)))
        (delete-region beg end)
        (insert (format "(%s)\n" value)))
    (progn
      (end-of-line)
      (forward-sexp)
      (newline)
      (let ((value (eval (elisp--preceding-sexp))))
        (if insert
            (insert (format "(%s)" value))
          (backward-kill-sexp)
          (insert (format "(%s)" value)))))))
(keymap-global-set "C-x r r" #'eval-and-replace)
(keymap-global-set "C-x C-r" #'eval-print-last-sexp)

;; Kill whole word
(defun cf/kill-word-or-region ()
  "Wrapper for \\[cf/kill-word-or-region] to kill word.
When mark is active, `kill-region' takes precedence.
Killed text is shown in the echo area."
  (interactive)
  (if (eq mark-active t)
      (kill-region (region-beginning) (region-end))
    (backward-word)
    (kill-word 1))
  (message "%s" (current-kill 0)))
(keymap-global-set "C-w" #'cf/kill-word-or-region)

;; Prepend kill-word
(defun cf/prepend-kill-word ()
  "Kill word prepending it to the last-kill."
  (interactive)
  (backward-word)
  (forward-word)
  (append-next-kill)
  (backward-kill-word 1)
  (message "%s" (current-kill 0)))
(keymap-global-set "C-c C-u" #'cf/prepend-kill-word)

;; Copy line or region
(defun cf/copy-line-or-region (&optional count)
  "Copy region if active, otherwise copy the whole current line.
With optional COUNT prefix argument (C-u N), insert the copied text COUNT times after point."
  (interactive "P")
  (let* ((text (if (use-region-p)
                   (buffer-substring (region-beginning) (region-end))
                 (concat (buffer-substring (line-beginning-position)
                                           (line-end-position))
                         "\n")))
         (times (prefix-numeric-value count)))
    (kill-new text)
    (when count
      (dotimes (_ times)
        (insert text)))
    (message "%s" (string-trim-right text))))
(keymap-global-set "C-c v" #'cf/copy-line-or-region)

;; Copy whole line
(defun cf/copy-whole-line ()
  "Copies the whole line."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (pos-bol)
      (pos-eol)))
    (message "%s" (current-kill 0))))

;; Clear the `kill-ring'
(defun cf/clear-kill-ring ()
  "Clears the `kill-ring'."
  (interactive)
  (setq kill-ring nil)
  (message "Kill ring cleared."))
(keymap-global-set "C-c z" #'cf/clear-kill-ring)

;; EwwSurfraw
(defvar cf-sr-default-elvi
  '((1 . "duckduckgo")
    (2 . "W")
    (3 . "S")
    (4 . "archwiki")
    (5 . "searx")
    (6 . "aur")
    (7 . "youtube")
    (8 . "wikipedia")
    (9 . "github"))
  "Alist mapping numeric prefix arguments to Surfraw elvi engines.")

(defun cf/sr-elvi-search (&optional elvi query)
  "Search for QUERY with Surfraw ELVI.
If REGION is active, use the selected text as the query.
Numeric prefix argument maps to preset elvis defined in `cf-sr-default-elvi'.
With a prefix argument, uses \\='S\\=' as the default elvi.
If missing, prompts for ELVI and/or QUERY."
  (interactive
   (list
    (if current-prefix-arg
        (let ((num-arg (prefix-numeric-value current-prefix-arg)))
          (or (alist-get num-arg cf-sr-default-elvi) "S"))
      (let* ((elvi-raw (shell-command-to-string "surfraw -elvi"))
             (elvi-list (split-string elvi-raw "\n" t))
             (selection (completing-read "[sr]: " elvi-list)))
        (car (split-string selection "[ \t]+"))))
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Query: "))))
  (let* ((url (string-trim (shell-command-to-string (format "surfraw -p %s '%s'" elvi query)))))
    (if (not (string-empty-p url))
        (eww url)
      (message "Failed to retrieve URL."))))


;;; Completion

;; Diminish
(use-package diminish
  :ensure t
  :init
  (require 'diminish)
  (require 'nerd-icons)
  :config
  (diminish 'visual-line-mode " ⤸")
  (diminish 'auto-fill-function " ↩ "))

;; Rainbow Mode
(use-package rainbow-mode
  :ensure t
  :diminish
  :hook prog-mode)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook prog-mode)

;; Mct
(use-package mct
  :ensure t
  :custom
  (mct-live-completion t)
  (mct-completion-window-size 12)
  (mct-hide-completion-mode-line t)
  (mct-completion-passlist
   '(cf/insert-palette-color Info-index icomplete-completions ido-completions find-file consult-imenu imenu find-buffer switch-buffer completing-read imenu-list))
  :config
  (mct-mode 1))

;; Embark
(use-package embark
  :ensure t
  :after ivy
  :bind
  (:map minibuffer-mode-map
        ("C-," . embark-collect)
        ("C->"   . embark-act)
        ("C-<"   . embark-dwim)
        ("C-h b" . embark-bindings)))

(use-package keycast
  :ensure t
  :config
  (setq keycast-mode-line-insert-after 'mode-line-modes)
  (setq keycast-mode-line-remove-tail-elements nil)
  (custom-set-faces
   '(keycast-key
     ((t (:inherit fixed-pitch
                   :background "#181818"
                   :foreground "#696969"
                   :box (:line-width (-1 . -1) :color "black" :style flat-button)
                   :weight bold))))))

;; Counsel
(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :bind
  (("C-c i"     . counsel-imenu)
   ("C-c b"     . counsel-ibuffer)
   ("C-c o"     . counsel-outline)
   ("C-c f"     . find-file-at-point)
   ("C-c I"     . imenu-list)
   ("C-c C-SPC" . counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r"       . counsel-minibuffer-history))
  :custom
  (ivy-initial-inputs-alist nil)
  (counsel-find-file-at-point t)
  :config (counsel-mode))

;; Consult
(use-package consult
  :ensure t
  :bind
  (:map shrface-mode-map
        ("C-c M-l" . shrface-links-consult)
        ("C-c C-l" . shrface-links-selectable-list)))

;; Marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; VertiCo
(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("?"          . minibuffer-completion-help)
        ("C-M-n"      . minibuffer-next-history-element)
        ("C-M-p"      . miibuffer-previous-history-element)
        ("C-n"        . minibuffer-next-line-completion)
        ("C-p"        . minibuffer-previous-line-completion)
        ("C-y"        . minibuffer-complete-and-exit)
        ("M-RET"      . minibuffer-force-complete-and-exit)
        ("M-<return>" . minibuffer-force-complete-and-exit))
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

;; Company
(use-package company
  :ensure t
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 3)
  (company-show-numbers nil)
  (company-tooltip-align-annotations 't)
  (company-dabbrev-code-completion-styles t)
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-code-everywhere t)
  :bind
  (("C-."   . company-complete-common-or-cycle)
   ("<tab>" . company-indent-or-complete-common)
   ("C-c y" . company-yasnippet)
   :map company-active-map
   ("TAB"   . company-complete-common-or-cycle)
   ("RET"   . company-complete-selection)
   ("C-n"   . company-select-next)
   ("C-p"   . company-select-previous)
   ("C-y"   . company-complete-selection))
  :config
  (global-company-mode)
  (setq company-backends
        '((company-capf :with company-yasnippet)
          (company-capf company-dabbrev-code)))
  (setq-local company-backends
              '(company-dabbrev)
              company-dabbrev-other-buffers nil
              company-dabbrev-ignore-case t
              company-dabbrev-downcase nil)
  (with-eval-after-load 'company
    (add-to-list 'company-backends
                 '(company-capf
                   company-yasnippet
                   company-abbrev
                   company-dabbrev
                   company-ispell)))
  (add-hook 'after-init-hook 'company-tng-mode))

;; Ivy
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-S-s"   . swiper)
         ("C-c C-r" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer-other-window)
         :map ivy-minibuffer-map
         ("<return>" . ivy-done)
         ("C-y"      . ivy-alt-done)
         ("C-n"      . ivy-next-line-and-call)
         ("C-p"      . ivy-previous-line-and-call)
         :map ivy-switch-buffer-map
         ("C-n"      . ivy-next-line-and-call)
         ("C-p"      . ivy-previous-line-and-call)
         ("<return>" . ivy-done)
         ("C-y"      . ivy-alt-done)
         ("C-k"      . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p"      . ivy-previous-line)
         ("C-n"      . ivy-next-line)
         ("C-k"      . ivy-reverse-i-search-kill))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (defun ivy-display-function-window (text)
    (let ((buffer (get-buffer-create "*ivy-candidate-window*"))
          (str (with-current-buffer (get-buffer-create " *Minibuf-1*")
                 (let ((point (point))
                       (string (concat (buffer-string) "  " text)))
                   (add-face-text-property
                    (- point 1) point 'ivy-cursor t string)
                   string))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert str)))
      (with-ivy-window
        (display-buffer
         buffer
         `((display-buffer-reuse-window
            display-buffer-below-selected)
           (window-height . ,(1+ (ivy--height (ivy-state-caller ivy-last)))))))))
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  :init
  (icomplete-mode -1)
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :diminish
  :after ivy
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-configure 'ivy-switch-buffer
    :display-transformer-fn #'ivy-switch-buffer-transformer))

;; Prescient
(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
  ;; (eww "https://github.com/daviwil/emacs-from-scratch/blob/805bba054513e3a2a2aa48648d7bebb1536ea4bc/show-notes/Emacs-Tips-Prescient.org")
  ;; (setq prescient-sort-length-enable nil)
  (ivy-prescient-mode 1))

;; yaSnippets
(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "yaS")
  :hook (prog-mode . yas-minor-mode)
  :bind ("C-c y" . company-yasnippet)
  :config
  (use-package yasnippet-snippets
    :ensure t
    :after yasnippet
    :config
    (yas-reload-all)))



;;; Language Support

;; Tree Sitter
(use-package treesit
  :ensure nil
  :config
  (use-package treesit-auto
    :ensure t
    :custom
    (treesit-auto-install t)
    :config
    (add-hook 'after-init-hook 'global-treesit-auto-mode)))

;; FlyCheck
(use-package flycheck
  :ensure t
  :defer t
  :config
  (global-flycheck-mode 1))

;; Eglot
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) "clangd"
                 (lua-mode) "luaJIT"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

;; Simple C
(use-package simpc-mode
  :ensure nil
  :diminish " "
  :init (require 'simpc-mode)
  :mode
  (("\\.c\\'" . simpc-mode)
   ("\\.h\\'" . simpc-mode))
  :config
  (defun cf-simpc-mode-comment-setup ()
    "Set up multi-line comment behavior for simpc-mode."
    (setq comment-start "/* "
          comment-end   " */"
          comment-style 'multi-line))
  (add-hook 'simpc-mode-hook 'cf-simpc-mode-comment-setup))

;; Go
(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'")

;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.md\\'")

;; html
(use-package htmlize
  :ensure t
  :defer t
  :mode "\\.htm?\\'")

;; lua
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :defer t)

;; php
(use-package php-mode
  :ensure t
  :defer t
  :mode "\\.php\\'")

(use-package sed-mode
  :ensure t
  :defer t
  :mode "\\.sed\\'")

;; Ledger
(use-package ledger-mode
  :ensure t
  :defer t
  :mode "\\.ldg\\'")



;;; Conviniences

;; Magit
(use-package magit
  :ensure t)

(use-package browse-at-remote
  :ensure t
  :defer t
  :bind ("C-c g g" . browse-at-remote))

;; Paredit
(use-package paredit
  :ensure t
  :diminish "[λ]"
  :hook (emacs-lisp-mode))

;; iy-go-to-char
(use-package iy-go-to-char
  :ensure nil
  :commands (iy-go-to-char iy-go-to-char-backward)
  :init
  (require 'iy-go-to-char)
  :bind
  (:map global-map
        ("M-m"   . iy-go-to-char)
        ("M-M"   . iy-go-to-char-backward)
        ("C-c ;" . iy-go-to-or-up-to-continue)
        ("C-c ," . iy-go-to-or-up-to-continue-backward)))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure nil
  :load-path "~/.emacs.d/custom/multiple-cursors/"
  :init
  (require 'mc-mark-more)
  (require 'rectangular-region-mode)
  (require 'mc-cycle-cursors)
  :custom
  (mc/always-run-for-all t)
  (mc/list-file "~/.emacs.d/mc-list")
  :bind
  (:map global-map
        ("C-<return>"       . set-rectangular-region-anchor)
        ("C-M-z"            . mc/edit-lines)
        ("C-S-y"            . yank-rectangle)
        ("C-c SPC"          . mc/mark-more-like-this-extended)
        ("C-M-n"            . mc/mark-next-like-this)
        ("C-S-p"            . mc/mark-previous-like-this)
        ("C-S-n"            . mc/mark-next-like-this)
        ("C-M-p"            . mc/mark-previous-like-this)
        ("M-<down>"         . mc/mark-next-like-this)
        ("M-<up>"           . mc/mark-previous-like-this)
        ("M-<left>"         . mc/unmark-previous-like-this)
        ("M-<right>"        . mc/unmark-next-like-this)
        ("C-M-'"            . mc/mark-all-like-this-dwim)
        ("M-<down-mouse-1>" . mc/toggle-cursor-on-click)
        :map mc/keymap
        ("M-<down>"         . mc/mark-next-like-this)
        ("M-<up>"           . mc/mark-previous-like-this)
        ("M-<left>"         . mc/unmark-previous-like-this)
        ("M-<right>"        . mc/unmark-next-like-this)
        ("C-M-:"            . mc/repeat-command)
        ("C->"              . mc/skip-to-next-like-this)
        ("C-<"              . mc/skip-to-previous-like-this)
        ("C-M-<"            . mc/unmark-previous-like-this)
        ("C-M->"            . mc/unmark-next-like-this)
        ("C-v"              . mc/cycle-forward)
        ("M-v"              . mc/cycle-backward))
  :config
  (with-eval-after-load 'iy-go-to-char
    (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)))

;; Drag Stuff
(use-package drag-stuff
  :ensure t
  :diminish
  :bind
  (:map global-map
        ("M-p" . drag-stuff-up)
        ("M-n" . drag-stuff-down)
   :map drag-stuff-mode-map
        ("C-M-S-p" . drag-stuff-up)
        ("C-M-S-n" . drag-stuff-down)
        ("C-M-S-h" . drag-stuff-left)
        ("C-M-S-l" . drag-stuff-right))
  :init
  (drag-stuff-global-mode 1))

;; Expand Region
(use-package expand-region
  :ensure t
  :bind ("C-;" . er/expand-region))

;; Pulsar
(use-package pulsar
  :load-path "~/.emacs.d/custom/pulsar/"
  :ensure nil
  :hook (prog-mode text-mode)
  :custom
  (pulsar-pulse t)
  (pulsar-inhibit-hidden-buffers t)
  (pulsar-pulse-on-window-change nil)
  (pulsar-resolve-pulse-function-aliases t)
  :bind ("C-x L" . pulsar-highlight-dwim)
  :config
  (defface pulsar-gray
    '((default :extend t)
      (((class color) (min-colors 88) (background light))
       :background "#323232")
      (((class color) (min-colors 88) (background dark))
       :background "#323232")
      (t :inverse-video t))
    "Custom alternative gray face for `pulsar-face'."
    :group 'pulsar-faces)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-gray)
  (setq pulsar-region-face 'pulsar-gray)
  (setq pulsar-highlight-face 'pulsar-gray)
  (setq pulsar-region-change-face 'pulsar-gray)
  (setq pulsar-window-change-face 'pulsar-gray)
  (setq pulsar-pulse-functions
        '(handle-switch-frame
          imenu
          logos-backward-page-dwim
          logos-forward-page-dwim
          handle-select-window
          move-to-window-line-top-bottom
          narrow-to-defun
          narrow-to-page
          narrow-to-region
          next-buffer
          next-error
          next-error-recenter
          next-multiframe-window
          occur-mode-goto-occurrence
          org-backward-heading-same-level
          org-forward-heading-same-level
          org-next-visible-heading
          org-previous-visible-heading
          other-window
          outline-backward-same-level
          outline-forward-same-level
          outline-next-visible-heading
          outline-previous-visible-heading
          outline-up-heading
          previous-buffer
          previous-error
          recenter-top-bottom
          reposition-window
          scroll-down-command
          scroll-up-command
          tab-close
          tab-new
          tab-next
          tab-previous
          widen
          windmove-down
          windmove-left
          windmove-right
          windmove-swap-states-down
          windmove-swap-states-left
          windmove-swap-states-right
          windmove-swap-states-up
          windmove-up))

  (setq pulsar-pulse-region-functions
        '(yank
          kill-line
          kill-region
          kill-ring-save
          append-next-kill
          kill-whole-line
          kill-visual-line
          kill-word
          backward-kill-word
          kill-sentence
          backward-kill-sentence
          kill-paragraph
          backward-kill-paragraph
          kill-sexp
          backward-kill-sexp
          kill-rectangle
          yank-rectangle
          open-rectangle undo))
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)
  (pulsar-global-mode 1))

;; Tramp
(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "sshx")
  (tramp-use-connection-share t))

;; SudoEdit
(use-package sudo-edit
  :ensure t
  :custom
  (sudo-edit-remote-method "sudo")
  (sudo-edit-local-method "sudo")
  :commands (sudo-edit sudo-edit-find-file)
  :bind
  (("C-c C-M-s" . sudo-edit)
   ("C-c C-M-f" . sudo-edit-find-file))
  :init (require 'sudo-edit))

;; Ediff
(use-package ediff
  :ensure nil
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Which Key :: wk
(use-package which-key
  :ensure t
  :diminish
  :init (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-allow-imprecise-window-fit nil
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 3
        which-key-side-window-slot -1
        which-key-side-window-max-height 0.3
        which-key-idle-delay .2
        which-key-max-description-length 30
        which-key-separator " → " ))

;; Projectile
(use-package projectile
  :ensure t
  :diminish
  :config
  (projectile-mode 1))

;; Eshell
(use-package eshell-toggle
  :ensure t
  :commands (eshell-toggle eshell)
  :bind ("M-<f3>" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
  (eshell-rc-script (concat user-emacs-directory "eshell/profile")
                    eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
                    eshell-history-size 5000
                    eshell-buffer-maximum-lines 5000
                    eshell-hist-ignoredups t
                    eshell-scroll-to-bottom-on-input t
                    eshell-destroy-buffer-when-process-dies t
                    eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh")
                    :config
                    (eshell-syntax-highlighting-global-mode 1)))
(use-package eshell-syntax-highlighting  ; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
  :ensure t                              ; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
  :after esh-mode)                       ; eshell-aliases-file -- sets an aliases file for the eshell.

(use-package ielm
  :ensure nil
  :bind ("M-<f2>" . ielm))

;; Vterm
(use-package vterm
  :ensure t
  :commands (vterm vterm-toggle)
  :config
  (setq shell-file-name "/usr/bin/zsh"
        vterm-max-scrollback 5000)
  (use-package vterm-toggle
    :ensure t
    :after vterm
    :bind ("C-\\" . vterm-toggle)
    :config
    (define-key vterm-mode-map (kbd "<escape>") #'vterm--self-insert)
    (define-key vterm-mode-map (kbd "C-\\") #'vterm-toggle)
    (setq vterm-toggle-fullscreen-p nil)
    (setq vterm-toggle-scope 'frame)
    (add-to-list 'display-buffer-alist
                 '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                   (display-buffer-reuse-window display-buffer-at-bottom)
                   (dedicated . t)
                   (reusable-frames . visible)
                   (window-height . 10)
                   (window-parameters . ((mode-line-format . none)))))
    (add-hook 'vterm-toggle-show-hook #'(lambda ()
                                          (let* ((sep1 (propertize "" 'face '(:foreground "#181818" :background "#282828")))
                                                 (sep2 (propertize "" 'face '(:foreground "#181818" :background "#282828")))
                                                 (inner (propertize "   %b   "
                                                                    'face '(:background "#181818" :box (:line-width (0 . -1) :color "#181818"))
                                                                    'local-map mode-line-buffer-identification-keymap)))
                                            (setq-default mode-line-buffer-identification (list sep1 inner sep2)))
                                          (window-divider-mode -1)
(force-mode-line-update)))

    (add-hook 'vterm-toggle-hide-hook #'(lambda ()
                                          (let* ((sep1 (propertize "" 'face '(:foreground "#282828" :background "#282828" :box (:line-width (0 . -1) :color "#181818" :style none))))
                                                 (sep2 (propertize "" 'face '(:foreground "#282828" :background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))))
                                                 (sep3 (propertize "" 'face '(:foreground "#181818" :background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))))
                                                 (sep4 (propertize "" 'face '(:foreground "#282828" :background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))))
                                                 (inner (propertize "   %b  "
                                                                    'face '(:background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))
                                                                    'local-map mode-line-buffer-identification-keymap)))
                                            (setq-default mode-line-buffer-identification (list sep1 sep2 inner sep3 sep4)))
                                          (window-divider-mode 1)
                                          (force-mode-line-update)))))

;; WindMove
(use-package windmove
  :ensure t
  :custom
  (windmove-wrap-around t)
  :config
  (windmove-default-keybindings)
  ;; Basic Nav
  (defun cf/shrink-window ()
    "Shrink the current window based on its relative position."
    (interactive)
    (let ((win (selected-window)))
      (cond
       ((window-combined-p win t)
        (if (eq win (window-left win))
            (enlarge-window-horizontally 5)
          (shrink-window-horizontally 5)))
       ((window-combined-p win nil)
        (if (eq win (window-top-child win))
            (enlarge-window 5)
          (shrink-window 5)))))
    (cf-activate-window-resize-map))

  (defun cf/enlarge-window ()
    "Enlarge the current window based on its relative position."
    (interactive)
    (let ((win (selected-window)))
      (cond
       ((window-combined-p win t)
        (if (eq win (window-left win))
            (shrink-window-horizontally 5)
          (enlarge-window-horizontally 5)))
       ((window-combined-p win nil)
        (if (eq win (window-top-child win))
            (shrink-window 5)
          (enlarge-window 5)))))
    (cf-activate-window-resize-map))

  (defvar cf-window-resize-map
    (let ((map (make-sparse-keymap)))
      (define-key map "-" #'cf/shrink-window)
      (define-key map "=" #'cf/enlarge-window)
      map)
    "Transient keymap for window resizing.")

  (defun cf-activate-window-resize-map ()
    "Activate the transient map for window resizing."
    (set-transient-map cf-window-resize-map))

  (keymap-global-set "C-c -" #'cf/shrink-window)
  (keymap-global-set "C-c =" #'cf/enlarge-window)

  (defun cf/dwim-windmove-buffer-in-direction (direction &optional no-follow)
    "Move current buffer in the window in DIRECTION, or split if needed.
DIRECTION is one of: \\='left, \\='right, \\='up, or \\='down.
If NO-FOLLOW is non-nil, don't select the moved buffer."
    (let ((win (selected-window)))
      (let ((target (windmove-find-other-window direction nil win)))
        (if target
            (progn
              (window-swap-states win target)
              (unless no-follow
                (select-window target)))
          (let ((new-win (pcase direction
                           ('left  (split-window win nil 'left))
                           ('right (split-window win nil 'right))
                           ('up    (split-window win nil 'above))
                           ('down  (split-window win nil 'below)))))
            (set-window-buffer new-win (current-buffer))
            (switch-to-prev-buffer win)
            (unless no-follow
              (select-window new-win)))))))

  (defun cf/windmove-left (&optional arg)
    (interactive "P")
    (cf/dwim-windmove-buffer-in-direction 'left arg))

  (defun cf/windmove-down (&optional arg)
    (interactive "P")
    (cf/dwim-windmove-buffer-in-direction 'down arg))

  (defun cf/windmove-up (&optional arg)
    (interactive "P")
    (cf/dwim-windmove-buffer-in-direction 'up arg))

  (defun cf/windmove-right (&optional arg)
    (interactive "P")
  (cf/dwim-windmove-buffer-in-direction 'right arg))

  (defvar-keymap cf-windmove-buffer-map
    :doc "Prefix keymap for buffer windmove commands."
    "h"        #'cf/windmove-left
    "j"        #'cf/windmove-down
    "k"        #'cf/windmove-up
    "l"        #'cf/windmove-right
    "<left>"   #'cf/windmove-left
    "<down>"   #'cf/windmove-down
    "<up>"     #'cf/windmove-up
    "<right>"  #'cf/windmove-right)
  (keymap-set global-map "C-s-/" cf-windmove-buffer-map)
  :bind
  (("C-c C-w C-j" . windmove-down)
   ("C-c C-w C-h" . windmove-left)
   ("C-c C-w C-k" . windmove-up)
   ("C-c C-w C-l" . windmove-right)
   ("C-c C-w j"   . cf/windmove-down)
   ("C-c C-w h"   . cf/windmove-left)
   ("C-c C-w k"   . cf/windmove-up)
   ("C-c C-w l"   . cf/windmove-right)))

;; PDF Tools
(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-loader-install)
  :diminish ""
  :mode "\\.pdf\\'"
  :bind
  (:map pdf-view-mode-map
        ("n"       . pdf-view-next-line-or-next-page)
        ("p"       . pdf-view-previous-line-or-previous-page)
        ("d"       . pdf-view-scroll-up-or-next-page)
        ("u"       . pdf-view-scroll-down-or-previous-page)
        ("C-="     . pdf-view-enlarge)
        ("C--"     . pdf-view-shrink)
        ("C-c C-t" . pdf-view-themed-minor-mode))
  :init
  (pdf-loader-install)
  :config
  (add-to-list 'revert-without-query ".pdf")
  (display-line-numbers-mode -1)
  (add-to-list 'display-buffer-alist
               '((major-mode . pdf-view-mode)
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (window-width . fit-window-to-buffer)
                 (side . right)
                 (window-parameters . ((mode-line-format . none))))))

(use-package highlight-numbers
  :ensure t
  :diminish
  :config
  (highlight-numbers-mode 1))

(use-package olivetti
  :ensure nil
  :diminish " õ"
  :hook (sdcv-definition-mode eww-mode Info-mode markdown-mode pdf-view-mode org-mode view-mode rfc-mode)
  :bind
  (:map olivetti-mode-map
        ("}" . olivetti-expand)
        ("{" . olivetti-shrink))
  :config
  (require 'olivetti)
  (add-hook 'xref-after-jump-hook #'olivetti-mode))


;;; Documentation

;; SDCV Local Dictionary Definitions
(use-package sdcv-definition
  :ensure nil
  :custom
  (sdcv-default-face 'variable-pitch)
  (sdcv-default-dictionary "Concise Oxford Thesaurus 2nd Ed. (Eng-Eng)")
  (sdcv-buffer-name "*sdcv*")
  :bind
  (("C-c S" . sdcv-eldoc-mode)
   ("C-?"   . cf/lookup-word)
   ("C-M-?" . sdcv-eldoc-mode)
   :map sdcv-definition-mode-map
   ("d" . cf/scroll-up-and-recenter)
   ("u" . cf/scroll-down-and-recenter))
  :config
  (require 'sdcv-definition))

;; Dictionary
(use-package dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org")
  (dictionary-use-single-buffer t)
  (dictionary-tooltip-dictionary "*")
  (dictionary-default-dictionary "*")
  :bind
  (:map global-map
        ("C-c d d" . dictionary-lookup-definition)
        ("C-c d w" . dictionary-search)
        ("C-c d m" . dictionary-match-words)
        ("C-c d t" . dictionary-tooltip-mode)
        :map dictionary-mode-map
        ("q" . quit-window)))

;; Info
(use-package info
  :ensure nil
  :bind
  (:map Info-mode-map
        ("U"            . Info-up)
        ("D"            . Info-directory)
        ("d"            . cf/scroll-up-and-recenter)
        ("u"            . cf/scroll-down-and-recenter)
        ("M-p"          . Info-history-back-menu)
        ("M-n"          . Info-history-forward-menu)
        ("C-c C-d"      . Info-last-preorder)
        ("C-c C-u"      . Info-next-preorder)
        ("<mouse-1>"    . Info-mouse-follow-link)
        ("<wheel-down>" . Info-mouse-scroll-down)
        ("<wheel-up>"   . Info-mouse-scroll-up)
        ("C-M-n"        . Info-next-menu-item)
        ("C-M-p"        . Info-last-menu-item)
        ("C-r"          . Info-search-backward))
  :config
  (require 'info)
  (setq Info-default-directory-list
        '("/home/cf/.local/share/info/" "/usr/local/share/info/" "/home/cf/.local/src/emacs/info/"
          "/usr/share/info/" "/usr/local/share/info/" "/home/cf/.emacs.d/elpaca/texis/" "/home/cf/.emacs.d/elpa/"
          "/home/cf/.emacs.d/elpa/archives/gnu/" "/home/cf/.emacs.d/elpa/archives/melpa/" "/home/cf/.emacs.d/elpa/archives/org/"
          "/home/cf/.emacs.d/elpa/archives/elpa/" "/home/cf/.emacs.d/elpa/archives/nongnu/" "/home/cf/.emacs.d/elpaca/builds/"
          "/home/cf/.emacs.d/melpa/" "/usr/share/emacs/site-lisp/" "/usr/share/emacs/30.1/lisp/"))

  (defun update-info-directories ()
    "Ensure `Info-directory-list` is updated with all available paths."
    (interactive)
    (setq Info-directory-list (delete-dups (flatten-list Info-default-directory-list))))

  (update-info-directories)

  (defun cf/install-info-manuals ()
    "Install or update .info manuals from .texi files."
    (interactive)
    (let ((texi-files (directory-files-recursively "~/.local/share/texis/" "\\.texi$")))
      (dolist (texi-file texi-files)
        (let* ((base-name (file-name-base texi-file))
               (info-file (concat base-name ".info"))
               (info-path (concat "~/.local/share/info/" info-file)))
          (when (file-newer-than-file-p texi-file info-path)
            (shell-command (format "makeinfo --ifinfo --number-sections --fill-column=79 --no-split --trace-includes %s -o %s" texi-file info-path))
            (message "Generated and installed %s" info-file))))))
  ;; (cf/install-info-manuals)
  (add-hook 'Info-mode-hook #'(lambda ()
                                (add-to-list 'Info-directory-list
                                             (expand-file-name "~/.local/share/info"))))

  (defvar info-manual-language-alist
    '((c       . "libc\\|/c\\b")
      (shell   . "bash\\|sh\\|zsh\\|shell")
      (elisp   . "emacs\\|elisp")
      (awk     . "awk\\|gawk\\|awkinet")
      (python  . "python\\|py")
      (java    . "java")
      (scheme  . "scheme\\|guile")
      (lisp    . "common lisp\\|clisp\\|sbcl")
      (tex     . "latex\\|tex\\b"))
    "Alist mapping programming languages to regex patterns for matching Info manual topics.")

  (defun info-detect-language ()
    "Detect programming language in `Info-mode' buffer via content or Info file name."
    (save-excursion
      (goto-char (point-min))
      (or
       (cond
        ((re-search-forward "^[ \t]*(defun " nil t) 'elisp)
        ((re-search-forward "^[ \t]*#include" nil t) 'c)
        ((re-search-forward "^[ \t]*#!/*" nil t) 'shell)
        ((re-search-forward "^[ \t]*def " nil t) 'python)
        ((re-search-forward "^[ \t]*class " nil t) 'java))
       (let ((file (and (boundp 'Info-current-file)
                        Info-current-file)))
         (when (stringp file)
           (cl-some (lambda (entry)
                      (let ((lang (car entry))
                            (pattern (cdr entry)))
                        (when (string-match-p pattern (downcase file))
                          lang)))
                    info-manual-language-alist)))
       'text)))
  (font-lock-add-keywords 'Info-mode
                          '(("^[*] \\([^:]+\\)::" . '(1 font-lock-function-name-face 'font-lock-keyword-face))))

  (add-hook 'Info-mode-hook
            (lambda ()
              (when (fboundp 'treesit-available-p)
                (let ((lang (info-detect-language)))
                  (when (treesit-language-available-p lang)
                    (treesit-parser-create lang)
                    (treesit-major-mode-setup))))))

  (add-hook 'Info-mode-hook
            (lambda ()
              (when (fboundp 'highlight-numbers-mode)
                (highlight-numbers-mode 1))
              (when (fboundp 'rainbow-delimiters-mode)
                (rainbow-delimiters-mode 1))
              (when (fboundp 'highlight-quoted-mode)
                (highlight-quoted-mode 1)))))

;; WoMan
(use-package woman
  :ensure nil
  :defer t
  :commands woman
  :bind
  (:map woman-mode-map
        ("d" . cf/scroll-up-and-recenter)
        ("u" . cf/scroll-down-and-recenter))
  :custom
  (woman-use-own-frame nil)
  (woman-font-lock t)
  (font-lock-maximum-decoration t)
  (woman-manpath '("/usr/man/" "/usr/share/man/" "/usr/share/man/man1/"
                   "/var/lib/snapd/snap/man/" "/opt/man/" "/usr/local/man/"
                   "/usr/X11R6/man/" "/home/cf/.local/share/.nvm/versions/node/v22.6.0/share/man/"
                   "/home/cf/.local/share/man/" "/home/cf/.local/share/perl5/man/"
                   "/usr/lib/jvm/default/man/" "/usr/local/share/man/"
                   "/usr/local/share/man/man1/" "/usr/local/share/man/man3/"
                   "/usr/local/share/man/man5/" "/usr/local/share/man/man7/"
                   "/usr/local/share/man/man8/" "/usr/local/texlive/2024/bin/x86_64-linux/man/"
                   "/usr/local/texlive/2024/texmf-dist/doc/man/"
                   "/usr/local/texlive/2024/texmf-dist/doc/man/man1/"
                   "/usr/local/texlive/2024/texmf-dist/doc/man/man5/"))
  :config
  (defun woman-detect-language ()
    "Detect the programming language in the current `woman-mode' buffer.
      Uses common macro sections and syntax patterns found in manual pages."
    (save-excursion
      (goto-char (point-min))
      (cond
       ((re-search-forward "^\\.\\(SH\\|SS\\|TH\\)" nil t) 'man)
       ((re-search-forward "^[ \t]*\\(#include\\|typedef\\|struct\\|enum\\|void\\)" nil t) 'c)
       ((re-search-forward "^[ \t]*\\(def \\|class \\|import \\|from \\|#!.*python\\)" nil t) 'python)
       ((re-search-forward "^#!.*sh\\|^[ \t]*\\(echo\\|if \\|then\\|fi\\|case \\|esac\\|while \\|done\\|for \\|in \\)" nil t) 'shell)
       ((re-search-forward "^[ \t]*(\\(defun\\|defmacro\\|lambda\\)" nil t) 'lisp)
       ((re-search-forward "^[ \t]*\\(public \\|private \\|class \\|static \\|void \\)" nil t) 'java)
       ((re-search-forward "^#!.*perl\\|^[ \t]*sub " nil t) 'perl)
       ((re-search-forward "^#!.*ruby\\|^[ \t]*\\(def \\|class \\|module \\)" nil t) 'ruby)
       ((re-search-forward "^[ \t]*\\(package \\|func \\|import \\)" nil t) 'go)
       (t 'text))))

  (defun woman-setup-highlighting ()
    "Enable syntax highlighting in `woman-mode' buffers.
Uses `treesit' for parsing if available and enables relevant highlight modes."
    (when (fboundp 'treesit-available-p)
      (let ((lang (woman-detect-language)))
        (when (and (treesit-available-p) (treesit-language-available-p lang))
          (treesit-parser-create lang)
          (treesit-major-mode-setup))))
    (when (derived-mode-p 'woman-mode)
      (when (fboundp 'font-lock-mode) (font-lock-mode 1))
      (when (fboundp 'highlight-numbers-mode) (highlight-numbers-mode 1))
      (when (fboundp 'rainbow-delimiters-mode) (rainbow-delimiters-mode 1))
      (when (fboundp 'highlight-quoted-mode) (highlight-quoted-mode 1))
      (when (fboundp 'hl-todo-mode) (hl-todo-mode 1))
      (when (fboundp 'olivetti-mode) (olivetti-mode 1))
      (when (fboundp 'treesit-auto-mode) (treesit-auto-mode 1))
      ))
  (add-hook 'woman-mode-hook #'woman-setup-highlighting))

;; rfc-mode
(use-package rfc-mode
  :ensure t
  :custom
  (rfc-mode-directory (expand-file-name "~/Documents/rfc/"))
  :commands (rfc-mode-browse rfc-mode-read)
  :bind
  (:map rfc-mode-map
        ("d" . cf/scroll-up-and-recenter)
        ("u" . cf/scroll-down-and-recenter))
  :config
  (defun cf/update-local-rfc-mirror ()
    "Synchronize local RFC mirror using rsync."
    (interactive)
    (let ((default-directory rfc-mode-directory))
      (async-shell-command
       (format "rsync -avz --delete rsync.rfc-editor.org::rfcs-text-only %s" (shell-quote-argument rfc-mode-directory)))))
  (unless (file-directory-p rfc-mode-directory)
    (make-directory rfc-mode-directory t)
    (cf/update-local-rfc-mirror)))

;; TLDR
(use-package tldr
  :ensure t
  :commands tldr
  :defer t)

;; Describe Helpers
(defmacro cf/describe-thing (type describe-fn)
  "If (thing-at-point TYPE) is non-nil, call DESCRIBE-FN on it, else do nothing."
  `(let ((thing (thing-at-point ',type t)))
     (when thing
       (,describe-fn (intern thing)))))

(defun cf/symbol-at-point ()
  "Return symbol at point or nil."
  (let ((s (thing-at-point 'symbol t)))
    (and s (intern-soft s))))

(defun cf/describe-symbol-at-point ()
  "Describe symbol at point, if any."
  (interactive)
  (cf/describe-thing symbol describe-symbol))

(defun cf/describe-function-at-point ()
  "Describe function at point, if any."
  (interactive)
  (let ((sym (cf/symbol-at-point)))
    (when (and sym (fboundp sym))
      (describe-function sym))))

(defun cf/describe-variable-at-point ()
  "Describe variable at point, if any."
  (interactive)
  (let ((sym (cf/symbol-at-point)))
    (when (and sym (boundp sym))
      (describe-variable sym))))

(defun cf/describe-symbol-at-point ()
  "Describe symbol at point, if any."
  (interactive)
  (let ((sym (cf/symbol-at-point)))
    (when sym
      (describe-symbol sym))))

(defun cf/describe-face-at-point ()
  "Describe face at point, if any."
  (interactive)
  (let ((sym (cf/symbol-at-point)))
    (when (and sym (facep sym))
      (describe-face sym))))

(defun cf/describe-keymap-at-point ()
  "Describe keymap at point, if any."
  (interactive)
  (let ((sym (cf/symbol-at-point)))
    (when (and sym (boundp sym) (keymapp (symbol-value sym)))
      (describe-keymap sym))))



;;; Org-Mode
(use-package org
  :ensure nil
  :defer t
  :config
  (setq org-confirm-babel-tangle nil)
  (setq org-export-with-toc nil)
  (setq org-image-actual-width nil)
  (setq org-list-allow-alphabetical t)
  (setq org-hide-emphasis-markers t)
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)
  (setq org-fold-catch-invisible-edits 'show-and-error)
  (setq org-special-ctrl-a/e t)
  (setq org-insert-heading-respect-content t)
  (setq org-pretty-entities t)
  (setq org-startup-with-inline-images t)
  (setq org-image-max-width 'window)
  (setq org-agenda-tags-column 0)
  (setq org-ellipsis "…")
  (setq org-export-backends '(md html))
  (setq org-html-htmlize-output-type 'inline-css)
  (setq org-directory "~/Documents/org/")
  (setq org-agenda-files (list "~/Documents/Agenda/"))
  (add-hook 'org-mode-hook #'(lambda ()
                               (setq-local electric-pair-inhibit-predicate
                                           `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (setq org-return-follows-link t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (shell . t)
     (lua . t)
     (perl . t)
     (emacs-lisp . t)
     (python . t)
     (awk . t)
     (sed . t)))

  (org-link-set-parameters
   "info"
   :follow (lambda (path) (info path)))

  (defun cf/org-man-to-org (topic)
    "Fetch a man page for TOPIC, convert it to properly formatted Org mode."
    (let* ((man-file (expand-file-name (format "%s.org" topic) "~/Documents/org/man/"))
           (cmd (format "man %s | col -b | awk '
BEGIN { first_section=1 }
/^[A-Z][A-Z ]*$/ {
    if (!first_section) print \"\";
    first_section=0;
    print \"* \" $0;
    next;
}
{
    printf \"    %%s\\n\", $0;
}' | sed -E '1s/^\\* (.*)/#+title: \\1/' > %s"
                        (shell-quote-argument topic)
                        (shell-quote-argument man-file))))
      (unless (file-directory-p "~/Documents/org/man/")
        (make-directory "~/Documents/org/man/" t))
      (shell-command cmd)
      (find-file man-file)))

  (org-link-set-parameters
   "man"
   :follow (lambda (path) (cf/org-man-to-org path))
   :face '(:foreground "light blue" :underline t))

  ;; (use-package org-indent
  ;;   :ensure nil
  ;;   :hook (org-mode . org-indent-mode)
  ;;   :diminish)

  (use-package toc-org
    :ensure t
    :config
    :hook org-mode)

  (use-package org-cliplink
    :ensure nil
    :load-path "~/.emacs.d/custom/org-cliplink/"
    :bind
    (("C-x p i" . org-cliplink)
     ("C-x p w" . cf/org-kill-heading-name-save))
    :config
    (require 'org-cliplink)
    (defun cf/org-increment-move-counter ()
      (interactive)
      (defun default (x d)
        (if x x d))
      (let* ((point (point))
             (move-counter-name "MOVE_COUNTER")
             (move-counter-value (-> (org-entry-get point move-counter-name)
                                     (default "0")
                                     (string-to-number)
                                     (1+))))
        (org-entry-put point move-counter-name
                       (number-to-string move-counter-value)))
      nil)
    (defun cf/org-get-heading-name ()
      (nth 4 (org-heading-components)))

    (defun cf/org-kill-heading-name-save ()
      (interactive)
      (let ((heading-name (cf/org-get-heading-name)))
        (kill-new heading-name)
        (message "Killed \"%s\"" heading-name)))


    (defun cf/cliplink-task ()
      "Capture an Org mode task from a URL in the clipboard using `org-cliplink'.

This function retrieves the title and URL from the current clipboard content
and inserts an Org mode TODO entry with a link to the URL.
If `org-cliplink' suceeds to retrive a title, the entry is formatted as \\='* TODO TITLE\n  [[URL][TITLE]]\\=', otherwise as \\='* TODO URL\n  [[URL]]\\='.

The function can be called interactively with \\[cf/cliplink-task] or as an
`org-capture-template'."
      (interactive)
      (org-cliplink-retrieve-title
       (substring-no-properties (current-kill 0))
       '(lambda (url title)
          (insert (if title
                      (concat "* TODO " title "\n  [[" url "][" title "]]")
                    (concat "* TODO " url "\n  [[" url "]]")))))))

  (setq org-capture-templates
        '(("p" "Capture task" entry (file "~/Documents/Agenda/Tasks.org")
           "* TODO %?\n  SCHEDULED: %T\n"
           :empty-lines 1)
          ("K" "Cliplink capture task" entry
           (file "~/Documents/Agenda/Tasks.org")
           "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n"
           :empty-lines 1)
          ("s" "Symbol Reference" entry
           (file+headline "~/Documents/org/references.org" "Captured Symbols")
           "* %?"
           :empty-lines 1)
          ("n" "Note" entry
           (file+headline "~/Documents/Agenda/notes.org" "Notes")
           "** %? %T\n"
           :empty-lines 1)
          ("t" "Téléphone" entry
           (file+headline "~/Documents/Agenda/Bottin.org" "Bottin")
           "** %? \n"
           :empty-lines 1)
          ("d" "Définition" entry
           (file+headline "~/Documents/org/definitions.org" "Définitions")
           "** %? \n\n -- %T\n"
           :empty-lines 1)))

 (setq org-feed-alist
           '(("Slashdot"
              "https://rss.slashdot.org/Slashdot/slashdot"
              "~/Documents/Agenda/feeds.org" "Slashdot Entries")))

  (setq org-agenda-custom-commands
        '(("u" "Unscheduled" tags "+personal-SCHEDULED={.+}-DEADLINE={.+}/!+TODO"
           ((org-agenda-sorting-strategy '(priority-down))))
          ("p" "Personal" ((agenda "" ((org-agenda-tag-filter-preset (list "+personal"))))))
          ("w" "Work" ((agenda "" ((org-agenda-tag-filter-preset (list "+work"))))))))

  (defun cf/update-org-date-property ()
    "Update the #+date: property in an Org file with its last modification time.
Only checks the first 3 lines for the presence of #+date:."
    (when (derived-mode-p 'org-mode)
      (let* ((mtime (nth 5 (file-attributes buffer-file-name)))
             (mtime-str (format-time-string "<%Y-%m-%d %H:%M:%S %Z>" mtime)))
        (save-excursion
          (goto-char (point-min))
          (let ((search-limit (save-excursion (forward-line 3) (point))))
            (if (re-search-forward "^#\\+date: .*" search-limit t)
                (replace-match (concat "#+date: " mtime-str))
              (goto-char (point-min))
              (insert (concat "#+date: " mtime-str "\n"))))))))
  (add-hook 'before-save-hook #'cf/update-org-date-property)

  (use-package org-modern
    :ensure nil
    :diminish " "
    :config
    (require 'org-modern)
    (require 'org-modern-indent)
    (with-eval-after-load 'org
      (global-org-modern-mode)))

  (mapc #'(lambda (x)
            (add-to-list 'org-structure-template-alist x))
        '(("el"     . "src emacs-lisp")
          ("txt"    . "src text")
          ("sho"    . "src shell :results output")
          ("cfg"    . "src conf")
          ("awk"    . "src awk")
          ("cc"     . "src C")
          ("cout"   . "src C :results output")
          ("pl"     . "src perl :results output")
          ("gp"     . "src gnuplot")
          ("li"     . "src lisp")
          ("la"     . "src latex")
          ("sh"     . "src shell")
          ("py"     . "src python :results output")
          ("pys"    . "src python :results output :session yes")))

  (require 'org-tempo)

  (defun cf/org-complete-or-cycle ()
    "Run `org-cycle-level' if `org-tempo-complete-tag' returns nil.
Used as a workaround for \\[TAB] in `org-mode' until a better fix."
    (interactive)
    (cond
     ((org-in-src-block-p)
      (indent-for-tab-command))
     ((org-at-heading-p)
      (org-cycle-level))
     ((not (org-in-src-block-p))
      (when (org-tempo-complete-tag)
        (org-tempo-complete-tag)))))

  :bind
  (("C-x a"     . org-agenda)
   ("C-c c"     . org-capture)
   ("C-c l"     . org-store-link)
   ("C-x p t"   . cf/cliplink-task)
   ("C-c L"     . org-insert-link-global)
   ("C-c O"     . org-open-at-point-global)
   ("C-c C-x j" . org-clock-goto)
   ("C-c C-x g" . org-feed-update-all)
   ("C-c C-x G" . org-feed-goto-inbox)
   :map org-mode-map
   ("TAB"       . cf/org-complete-or-cycle)
   ("<tab>"     . cf/org-complete-or-cycle)
   ("C-c C--"   . org-table-insert-hline)))



;;; Directory Editor

;; yt-dired buffer
(defun cf/yt-dired ()
  "Open yt directory with Dired in a side window.
Allows multiple subdirectories in split windows with thumbnails."
  (interactive)
  (let ((buffer (dired "~/Videos/yt")))
    (display-buffer-reuse-mode-window
     buffer `((slot . -1)
              (side . left)
              (window-width . 0.45)
              (window-parameters . ((no-delete-window . t)
                                    (no-delete-other-windows . t)
                                    (mode-line-format . (" " "%b"))))))
    (with-current-buffer buffer
      (rename-buffer "*yt-dired*"))))

(setq browse-url-handlers
      '(("https://www\\.youtube\\.com/watch\\?v=.*" . cf/browse-url-mpv)
        ("https://www\\.youtube\\.com/playlist\\?list=.*" . cf/browse-url-mpv)))

(defun cf/browse-url-mpv (url &rest _args)
  "Open the given URL in mpv."
  (start-process-shell-command "mpv" nil (format "mpv %s" url)))

(setq browse-url-browser-function
      (lambda (url &rest args)
        (if (or (string-match-p "^https://www\\.youtube\\.com/watch\\?v=" url)
                (string-match-p "^https://www\\.youtube\\.com/playlist\\?list=" url))
            (cf/browse-url-mpv url)
          (apply 'eww url args))))

;; DirEd
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :custom
  (dired-mouse-drag-files t)
  (dired-hide-details-mode 1)
  (dired-hide-details-hide-absolute-location t)
  (find-file-visit-truename t)
  (dired-omit-verbose nil)
  (dired-hide-details-hide-symlink-targets t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash nil)
  (dired-dwim-target t)
  :config
  (require 'dired-x)
  (require 'dired-hacks-utils)
  (require 'dired-list)
  (require 'dired-open)
  (require 'dired-subtree)
  (defun cf/dired-open (file &optional filelist)
    "Open FILE or FILELIST in mpv, or use `dired-find-file` as a fallback.
If FILE is a JPEG image, attempt to extract an embedded link with `exifjpg'.
and open the target link in mpv.

If FILELIST contains marked files, generate an HLS playlist output to a buffer,
and open it in mpv.

For non-media files, fall back to `dired-find-file`."
    (interactive (list (dired-get-file-for-visit) (dired-get-marked-files)))
    (if filelist
        (let* ((playlist (format "playlist_%s.m3u8" (format-time-string "%Y-%m-%d_%H%M%S")))
               (playlist-path (expand-file-name playlist temporary-file-directory)))
          (with-temp-file playlist-path
            (insert "#EXTM3U\n")
            (dolist (f filelist)
              (insert (format "%s\n"
                              (string-trim (shell-command-to-string
                                            (format "exifjpg %s -M" (shell-quote-argument f))))))))
          (find-file-other-window playlist-path)
          (dired-open--start-process playlist-path "mpv"))

      (if (and filelist (= (length filelist) 1))
          (start-process-shell-command "linkhandler" nil
                                       (format "linkhandler %s" (shell-quote-argument file)))
        (message (format "linkhandler: %s" file))
        (dired-find-file))))

  (defun cf/dnd-uri-to-file (uri)
    "Convert a file:// URI to a fully resolved local path (follows symlinks)."
    (when (string-prefix-p "file://" uri)
      (let* ((path (url-unhex-string (string-remove-prefix "file://" uri))))
        (file-truename path))))

  (defun cf/dnd-exifjpg-M (event)
    "Extract EXIF data from JPG via `exifjpg -M` on drag event in Dired buffers."
    (interactive "e")
    (let* ((posn (event-start event))
           (window (posn-window posn))
           (buffer (window-buffer window)))
      (with-current-buffer buffer
        (when (eq major-mode 'dired-mode)
          (let* ((file (dired-get-filename nil t)))
            (when (and file (string-match-p "\\.jpe?g\\'" file))
              (let ((output (shell-command-to-string
                             (format "exifjpg %s -M"
                                     (shell-quote-argument file)))))
                (unless (string-blank-p output)
                  (with-current-buffer (window-buffer (selected-window))
                    (insert output))))))))))

  (defun cf/dnd-exifjpg-handler (uri action)
    "Handle dropped JPG files by inserting output of `exifjpg -M'."
    (let ((file (cf/dnd-uri-to-file uri)))
      (if (and file (string-match-p "\\.jpe?g\\'" file) (file-readable-p file))
          (let ((output (shell-command-to-string
                         (format "exifjpg %s -M" (shell-quote-argument file)))))
            (unless (string-blank-p output)
              (insert output)))
        (dnd-open-local-file uri action))))
  (add-to-list 'dnd-protocol-alist '("^file:" . cf/dnd-exifjpg-handler))

  (defun cf-dired-truncate-symlinks-display ()
    "Truncate the symlink display by hiding the ' -> TARGET' part in Dired."
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward " -> .*" nil t)
          (put-text-property (match-beginning 0) (match-end 0) 'invisible t)))))

 (defun dired-init-hooks ()
    "Disables whitespaces and display-line-numbers in dired buffers."
    (whitespace-mode -1)
    (visual-line-mode -1)
    (global-whitespace-mode -1)
    (display-line-numbers-mode -1)
    (setq-local truncate-lines t)
    (setq buffer-display-table nil))
  (add-hook 'dired-mode-hook 'dired-init-hooks)
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
  (add-hook 'dired-after-readin-hook #'cf-dired-truncate-symlinks-display)
  :bind
  (:map dired-mode-map
        ("h"     . dired-up-directory)
        ("l"     . dired-find-file)
        ("<"     . cf/dired-open)
        (">"     . cf/region-to-linkhandler)
        ("o"     . dired-open-file)
        ("O"     . dired-find-file-other-window)
        ("("     . dired-hide-details-mode)
        (")"     . dired-omit-mode)
        ("C-c L" . dired-list-mpc))
  :init
  (whitespace-mode -1)
  (global-whitespace-mode -1)
  (global-display-line-numbers-mode -1))

;; Dired-X
(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :init (require 'dired-x)
  :custom
  (dired-omit-files "\\`[.]?#\\|\\`\\.[.]?\\'\\|^\\..+\\|package-lock\\.json\\|vimwiki")
  (dired-omit-extensions
   '(".hi" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof"
     ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/"
     ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl"
     ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl"
     ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl"
     ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp"
     ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof" ".lot"
     ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp"
     ".tps" ".vr" ".vrs"))
  :bind
  (:map dired-mode-map
        (")" . dired-omit-mode)))

;; Dired Preview
(use-package dired-preview
  :ensure t
  :custom
  (dired-preview-delay 0.1)
  (dired-preview-max-size (expt 2 30))
  (dired-preview-ignored-extensions-regexp "\\.\\(opus\\|flac\\|mp4a\\|mp3\\|mp4\\|webm\\|avi\\|mkv\\)")
  (dired-preview-image-extensions-regexp "\\.\\(png\\|jpg\\|jpeg\\|tiff\\|svg\\|webp\\)")
  :bind
  (:map dired-mode-map
        ("P" . dired-preview-global-mode)
        :map dired-preview-mode-map
        ("C-c C-x" . dired-preview-hexl-toggle)
        ("C-c C-f" . dired-preview-find-file)
        ("C-c C-o" . dired-preview-open-dwim)
        ("C-c C-u" . dired-preview-page-up)
        ("C-c C-d" . dired-preview-page-down))
  :config
  (require 'dired-preview)
  (defun cf-dired-preview-right-or-top ()
    "Display dired-preview on the right if there’s enough width, otherwise on top."
    (let ((width (window-total-width))
          (height (window-total-height)))
      (if (> width (* height 3.5))
          `((display-buffer-in-side-window display-buffer-reuse-mode-window)
            (window-width . 0.6)
            (side . right)
            (window-height . ,height)
            (slot . 1)
            (window-parameters . ((no-other-window . t)
                                  (mode-line-format . none))))
        `((display-buffer-in-side-window display-buffer-reuse-mode-window)
          (window-height . 0.5)
          (side . top)
          (slot . 0)
          (window-parameters . ((no-other-window . t)
                                (mode-line-format . none)))))))
  (setq dired-preview-display-action-alist #'cf-dired-preview-right-or-top))

;; Dired Subtree
(use-package dired-subtree
  :ensure t
  :after dired-x
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil)
  :config
  (add-hook 'dired-subtree-after-remove-hook
            (lambda () dired-omit-mode))
  (add-hook 'dired-subtree-after-insert-hook
            (lambda () dired-omit-mode))
  (add-hook 'dired-subtree-after-insert-hook #'cf-dired-truncate-symlinks-display))

;; Dired Git Info
(use-package dired-git-info
    :ensure t
    :after dired-x
    :config
    (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable))

;; Dired Hack Utils
(use-package dired-hacks-utils
  :ensure nil
  :config (require 'dired-hacks-utils))

(use-package dired-open
  :ensure nil
  :bind (:map dired-mode-map
              ("o" . dired-open-file)
              ("<" . cf/dired-open))
  :custom
  (dired-open-extensions
   '(("gif" . "nsxiv")
     ("png" . "nsxiv")
     ("webp" . "nsxiv")
     ("m3u" . "linkhandler")
     ("m3u8" . "linkhandler")
     ("jpeg" . "linkhandler")
     ("mkv" . "linkhandler")
     ("webm" . "linkhandler")
     ("mp4" . "linkhandler")
     ("pdf" . "zathura")
     ("jpg" . "linkhandler")))
  :config
  (require 'dired-open))

(use-package emms
  :ensure t
  :config
  (emms-all)
  (require 'emms-player-simple)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (setq emms-player-list '(emms-player-mpd
                           emms-player-vlc
                           emms-player-mpv))
  (require 'emms-browser)

  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-browser-thumbnail-small-size 64)
  (setq emms-browser-thumbnail-medium-size 128)
  (emms-browser-make-filter "all" #'ignore)
  (emms-browser-make-filter "recent"
                            (lambda (track) (< 30
                                               (time-to-number-of-days
                                                (time-subtract (current-time)
                                                               (emms-info-track-file-mtime track))))))
  (emms-browser-set-filter (assoc "all" emms-browser-filters))
  (emms-history-load)
  (setq-default
   emms-source-file-default-directory "~/Music/"
   emms-source-playlist-default-format 'm3u
   emms-playlist-mode-center-when-go t
   emms-playlist-default-major-mode 'emms-playlist-mode
   emms-show-format "NP: %s"
   emms-player-list '(emms-player-mpv)
   emms-player-mpv-environment '("PULSE_PROP_media.role=music")
   emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display" "--force-window=no" "--vo=null")
   emms-volume-change-function 'emms-volume-mpv-change
   emms-volume-mpv-method 'smart)
  (setq emms-info-asynchronously nil)
  (setq emms-playlist-buffer-name "*Music*"))

(use-package ytdious
  :after emms
  :ensure t)



;;; Bongo
(use-package bongo
  :ensure t
  :config
  (setq bongo-default-directory "~/Music")
  (setq bongo-prefer-library-buffers nil )
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-inline-playback-progress t)
  (setq bongo-join-inserted-tracks t)
  (setq bongo-field-separator (propertize " . " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-enabled-backends '(mpv vlc))
  (setq bongo-vlc-program-name "cvlc")

  (defvar cf-bongo-playlist-delimiter
    "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"
    "Delimiter for inserted entries in `bongo playlist' buffers.")

  (defun cf/bongo-playlist-section ()
    (bongo-insert-comment-text
     cf-bongo-playlist-delimiter))

  (defun cf/bongo-playlist-section-next ()
    "Move to next `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-forward section nil t))
          (progn
            (goto-char (pos-eol))
            (re-search-forward section nil t))
        (goto-char (point-max)))))

  (defun cf/bongo-playlist-section-previous ()
    "Move to previous `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (goto-char (pos-bol))
            (re-search-backward section nil t))
        (goto-char (point-min)))))

  (defun cf/bongo-playlist-mark-section ()
    "Mark `bongo' playlist section, delimited by custom markers.
The marker is `cf-bongo-playlist-delimiter'."
    (interactive)
    (let ((section "^\\*+$"))
      (search-forward-regexp section nil t)
      (push-mark nil t)
      (forward-line -1)
      ;; REVIEW any predicate to replace this `save-excursion'?
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (search-backward-regexp section nil t)
            (forward-line 1))
        (goto-char (point-min)))
      (activate-mark)))

  (defun cf/bongo-playlist-kill-section ()
    "Kill `bongo' playlist-section at point.
This operates on a custom delimited section of the buffer.  See
`cf/bongo-playlist-kill-section'."
    (interactive)
    (cf/bongo-playlist-mark-section)
    (bongo-kill))

  (defun cf/bongo-playlist-play-random ()
    "Play random `bongo' track and determine further conditions."
    (interactive)
    (unless (bongo-playlist-buffer)
      (bongo-playlist-buffer))
    (when (or (bongo-playlist-buffer-p)
              (bongo-library-buffer-p))
      (unless (bongo-playing-p)
        (with-current-buffer (bongo-playlist-buffer)
          (bongo-play-random)
          (bongo-random-playback-mode 1)
          (bongo-recenter)))))

  (defun cf/bongo-playlist-random-toggle ()
    "Toggle `bongo-random-playback-mode' in playlist buffers."
    (interactive)
    (if (eq bongo-next-action 'bongo-play-random-or-stop)
        (bongo-progressive-playback-mode)
      (bongo-random-playback-mode)))

  (defun cf/bongo-playlist-reset ()
    "Stop playback and reset `bongo' playlist marks.
To reset the playlist is to undo the marks produced by non-nil
`bongo-mark-played-tracks'."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-reset-playlist)))

  (defun cf/bongo-playlist-terminate ()
    "Stop playback and clear the entire `bongo' playlist buffer.
Contrary to the standard `bongo-erase-buffer', this also removes
the currently-playing track."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-erase-buffer)))

  (defun cf/bongo-playlist-insert-playlist-file ()
    "Insert contents of playlist file to a `bongo' playlist.
Upon insertion, playback starts immediately, in accordance with
`cf/bongo-play-random'.

The available options at the completion prompt point to files
that hold filesystem paths of media items.  Think of them as
\='directories of directories' that mix manually selected media
items.

Also see `cf/bongo-dired-make-playlist-file'."
    (interactive)
    (let* ((path "~/Music/playlists/")
           (dotless directory-files-no-dot-files-regexp)
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files path nil dotless)))
           (choice (completing-read "Insert playlist: " playlists nil t)))
      (if (bongo-playlist-buffer-p)
          (progn
            (save-excursion
              (goto-char (point-max))
              (bongo-insert-playlist-contents
               (format "%s%s" path choice))
              (cf/bongo-playlist-section))
            (cf/bongo-playlist-play-random))
        (user-error "Not in a `bongo' playlist buffer"))))

  ;; Bongo + Dired (bongo library buffer)
  (defmacro cf/bongo-dired-library (name doc val)
    "Create `bongo' library function NAME with DOC and VAL."
    `(defun ,name ()
       ,doc
       (when (string-match-p "\\`~/Music/" default-directory)
         (bongo-dired-library-mode ,val))))

  (cf/bongo-dired-library
   cf/bongo-dired-library-enable
   "Set `bongo-dired-library-mode' when accessing \\='~/Music\\='.

Add this to `dired-mode-hook'.  Upon activation, the directory
and all its sub-directories become a valid library buffer for
Bongo, from where we can, among others, add tracks to playlists.
The added benefit is that Dired will continue to behave as
normal, making this a superior alternative to a purpose-specific
library buffer.

Note, though, that this will interfere with `wdired-mode'.  See
`cf/bongo-dired-library-disable'."
   1)

  ;; NOTE `cf/bongo-dired-library-enable' does not get reactivated
  ;; upon exiting `wdired-mode'.
  ;;
  ;; TODO reactivate bongo dired library upon wdired exit
  (cf/bongo-dired-library
   cf/bongo-dired-library-disable
   "Unset `bongo-dired-library-mode' when accessing ~/Music.
This should be added `wdired-mode-hook'.  For more, refer to
`cf/bongo-dired-library-enable'."
   -1)

  (defun cf/bongo-dired-insert-files ()
    "Add files in a `dired' buffer to the `bongo' playlist."
    (let ((media (dired-get-marked-files)))
      (with-current-buffer (bongo-playlist-buffer)
        (goto-char (point-max))
        (mapc 'bongo-insert-file media)
        (cf/bongo-playlist-section))
      (with-current-buffer (bongo-library-buffer)
        (dired-next-line 1))))

  (defun cf/bongo-dired-insert ()
    "Add `dired' item at point or marks to `bongo' playlist.

The playlist is created, if necessary, while some other tweaks
are introduced.  See `cf/bongo-dired-insert-files' as well as
`cf/bongo-playlist-play-random'.

Meant to work while inside a `dired' buffer that doubles as a
library buffer (see `cf/bongo-dired-library')."
    (interactive)
    (when (bongo-library-buffer-p)
      (unless (bongo-playlist-buffer-p)
        (bongo-playlist-buffer))
      (cf/bongo-dired-insert-files)
      (cf/bongo-playlist-play-random)))

  (defun cf/bongo-dired-make-playlist-file ()
    "Add `dired' marked items to playlist file using completion.

These files are meant to reference filesystem paths.  They ease
the task of playing media from closely related directory trees,
without having to interfere with the user's directory
structure (e.g. a playlist file \\='rock\\=' can include the paths of
~/Music/Scorpions and ~/Music/Queen).

This works by appending the absolute filesystem path of each item
to the selected playlist file.  If no marks are available, the
item at point will be used instead.

Selecting a non-existent file at the prompt will create a new
entry whose name matches user input.  Depending on the completion
framework, such as with `icomplete-mode', this may require a
forced exit (e.g. \\[exit-minibuffer] to parse the input without
further questions).

Also see `cf/bongo-playlist-insert-playlist-file'."
    (interactive)
    (let* ((dotless directory-files-no-dot-files-regexp)
           (pldir "~/Music/playlists")
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files pldir nil dotless)))
           (plname (completing-read "Select playlist: " playlists nil nil))
           (plfile (format "%s/%s" pldir plname))
           (media-paths
            (if (derived-mode-p 'dired-mode)
                ;; TODO more efficient way to do ensure newline ending?
                ;;
                ;; The issue is that we need to have a newline at the
                ;; end of the file, so that when we append again we
                ;; start on an empty line.
                (concat
                 (mapconcat #'identity
                            (dired-get-marked-files)
                            "\n")
                 "\n")
              (user-error "Not in a `dired' buffer"))))
      ;; The following `when' just checks for an empty string.  If we
      ;; wanted to make this more robust we should also check for names
      ;; that contain only spaces and/or invalid characters…  This is
      ;; good enough for me.
      (when (string-empty-p plname)
        (user-error "No playlist file has been specified"))
      (unless (file-directory-p pldir)
        (make-directory pldir))
      (unless (and (file-exists-p plfile)
                   (file-readable-p plfile)
                   (not (file-directory-p plfile)))
        (make-empty-file plfile))
      (append-to-file media-paths nil plfile)
      (with-current-buffer (find-file-noselect plfile)
        (delete-duplicate-lines (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (save-buffer)
        (kill-buffer))))
  :hook ((wdired-mode-hook . cf/bongo-dired-library-disable)
         (dired-mode-hook  . cf/bongo-dired-library-enable))
  :bind (("<C-XF86AudioPlay>" . bongo-pause/resume)
         ("<C-XF86AudioNext>" . bongo-next)
         ("<C-XF86AudioPrev>" . bongo-previous)
         ("<M-XF86AudioPlay>" . bongo-show)
         ("<S-XF86AudioNext>" . bongo-seek-forward-10)
         ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
         :map bongo-playlist-mode-map
         ("J"   . dired-jump)
         ("j"   . bongo-dired-line)
         ("n"   . bongo-next-object)
         ("p"   . bongo-previous-object)
         ("N"   . cf/bongo-playlist-section-next)
         ("P"   . cf/bongo-playlist-section-previous)
         ("M-h" . cf/bongo-playlist-mark-section)
         ("M-d" . cf/bongo-playlist-kill-section)
         ("g"   . cf/bongo-playlist-reset)
         ("D"   . cf/bongo-playlist-terminate)
         ("r"   . cf/bongo-playlist-random-toggle)
         ("R"   . bongo-rename-line)
         ("i"   . cf/bongo-playlist-insert-playlist-file)
         ("I"   . bongo-insert-special)
         :map bongo-dired-library-mode-map
         ("<C-return>" . cf/bongo-dired-insert)
         ("C-c SPC"    . cf/bongo-dired-insert)
         ("C-c +"      . cf/bongo-dired-make-playlist-file)))



;;; Web

;; Shr html renderer
(use-package shr
  :ensure nil
  :custom
  (shr-use-fonts nil))

(use-package shr-color
  :ensure nil
  :custom
  (shr-color-visible-luminance-min 1)
  (shr-color-visible-luminance-min 70))

(use-package language-detection
  :ensure t
  :after eww)

;; Shr support for code blocks
(use-package shr-tag-pre-highlight
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

;; ShrFace
(use-package shrface
  :ensure t
  :after shr
  :config
  (setq shr-cookie-policy nil)
  (setq shrface-bullets-bullet-list
   '("▼" "▽" "▿" "▾"))
  (require 'eww)
  (require 'shrface)
  (require 'shr-tag-pre-highlight)
  (require 'cl-lib)

  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let ((mode (eww-buffer-auto-detect-mode)))
        (when mode
          (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region
     (point-min) (point-max) nil))

  (defun eww-buffer-auto-detect-mode ()
    (let* ((map '((ada ada-mode)
                  (awk awk-mode)
                  (c c-mode)
                  (cpp c++-mode)
                  (clojure clojure-mode lisp-mode)
                  (csharp csharp-mode java-mode)
                  (css css-mode)
                  (dart dart-mode)
                  (delphi delphi-mode)
                  (emacslisp emacs-lisp-mode)
                  (erlang erlang-mode)
                  (fortran fortran-mode)
                  (fsharp fsharp-mode)
                  (go go-mode)
                  (groovy groovy-mode)
                  (haskell haskell-mode)
                  (html html-mode)
                  (java java-mode)
                  (javascript javascript-mode)
                  (json json-mode javascript-mode)
                  (latex latex-mode)
                  (lisp lisp-mode)
                  (lua lua-mode)
                  (matlab matlab-mode octave-mode)
                  (objc objc-mode c-mode)
                  (perl perl-mode)
                  (php php-mode)
                  (prolog prolog-mode)
                  (python python-mode)
                  (r r-mode)
                  (ruby ruby-mode)
                  (rust rust-mode)
                  (scala scala-mode)
                  (shell shell-script-mode)
                  (smalltalk smalltalk-mode)
                  (sql sql-mode)
                  (swift swift-mode)
                  (visualbasic visual-basic-mode)
                  (xml sgml-mode)))
           (language (language-detection-string
                      (buffer-substring-no-properties (point-min) (point-max))))
           (modes (cdr (assoc language map)))
           (mode (cl-loop for mode in modes
                          when (fboundp mode)
                          return mode)))
      (message (format "%s" language))
      (when (fboundp mode)
        mode)))

  (setq shr-external-rendering-functions
        '((pre . eww-tag-pre)))

  (defun eww-link-buttonize ()
    "Ensure that links in EWW buffers are properly buttonized."
    (interactive)
    (setq shr-external-rendering-functions
          (append '((a . shr-tag-a)) shr-external-rendering-functions)))

  (add-hook 'eww-after-render-hook #'eww-link-buttonize)

  (with-eval-after-load 'eww
    (keymap-set eww-mode-map "<tab>" #'forward-button)
    (keymap-set eww-mode-map "C-<tab>" #'shrface-outline-cycle)
    (keymap-set eww-mode-map "C-<backtab>" #'shrface-outline-cycle-buffer)
    (keymap-set eww-mode-map "C-t" #'shrface-toggle-bullets)
    (keymap-set eww-mode-map ">" #'shrface-next-headline)
    (keymap-set eww-mode-map "<" #'shrface-previous-headline)
    (keymap-set eww-mode-map "L" #'shrface-links-counsel)
    (keymap-set eww-mode-map "H" #'shrface-headline-counsel))

  (defun shrface-mu4e-advice (orig-fun &rest args)
    (require 'eww)
    (require 'shr-tag-pre-highlight)
    (let ((shrface-org nil)
          (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
          (shr-table-vertical-line "|")
          (shr-width 90)
          (shr-indentation 3)
          (shr-external-rendering-functions
           (append '((title . eww-tag-title)
                     (form . eww-tag-form)
                     (input . eww-tag-input)
                     (button . eww-form-submit)
                     (textarea . eww-tag-textarea)
                     (select . eww-tag-select)
                     (link . eww-tag-link)
                     (meta . eww-tag-meta)
                     (pre . eww-tag-pre)
                     (a . shr-tag-a))
                   shrface-supported-faces-alist))
          (shrface-toggle-bullets t)
          (shrface-href-versatile t)
          (shr-use-fonts nil))
      (apply orig-fun args)))
  (with-eval-after-load 'mu4e
  (advice-add 'mu4e-shr2text :around #'shrface-mu4e-advice))
)

;;; EWW Emacs Web Wowser
(use-package eww
  :ensure nil
  :custom
  (eww-search-confirm-send-region nil)
  :bind
  (:map global-map
        ("s-/" . eww-search-words)
        :map eww-mode-map
        ("<tab>" . forward-button)
        ("C-<mouse-1>" . eww-follow-link-other-window)
        ("o" . eww-follow-link)
        ("O" . eww-follow-link-dwim-other-window)
        ("d" . cf/scroll-up-and-recenter)
        ("u" . cf/scroll-down-and-recenter)
        ("n" . next-line)
        ("p" . previous-line)
        ("N" . eww-next-url)
        ("P" . eww-previous-url)
        ("r" . eww-readable)
        ("R" . eww-reload)
        ("W" . eww-beginning-of-text)
        ("a" . eww-copy-alternate-url)
        ("w" . eww-copy-page-url)
        ("E" . eww--dwim-expand-url)
        ("g" . eww-readable)
        ("M" . eww--open-url-in-new-buffer)
        ("D" . eww-download)
        ("C-c h" . shrface-headline-counsel)
        :map url-cookie-mode-map
        ("DEL" . url-cookie-delete)
        ("u" . url-cookie-undo))
  :custom
  (browse-url-browser-function 'eww)
  (browse-url-secondary-browser-function 'browse-url-qutebrowser)
  :config
  (require 'eww)
  (require 'shrface)
  (require 'shr-tag-pre-highlight)
  (shrface-mode 1)
  (olivetti-mode 1)
  (add-hook 'eww-after-render-hook #'eww-readable))

;; ici première.
(defun cf/mpc-radio-can ()
  "Ajoute le stream de Radio-Canada dans la playlist mpc."
  (interactive)
  (start-process-shell-command "radio" nil "museplay radio"))



;;; Keymaps

;; Crux
(use-package crux
  :ensure nil
  :custom
  (crux-shell "/usr/bin/zsh")
  (crux-term-func 'vterm)
  (crux-move-visually t)
  :bind
  (:map global-map
        ("C-k"     . crux-smart-kill-line)
        ("M-o"     . crux-smart-open-line-above)
        ("M-i"     . crux-move-beginning-of-line)
        ("C-c C-d" . crux-cleanup-buffer-or-region)
        ("C-c C-v" . crux-duplicate-current-line-or-region)))

;; Keychords
(use-package key-chord
  :ensure nil
  :init
  (require 'key-chord)
  (require 'iy-go-to-char)
  (require 'sdcv-definition)
  (require 'cf-common-abbrev-table)
  (require 'crux)
  :config
  (key-chord-mode 1)
  (key-chord-define-global "fv" #'iy-go-to-char)
  (key-chord-define-global "dc" #'iy-go-to-char-backward)

  (key-chord-define-global "vm" #'view-mode)
  (key-chord-define-global "wf" #'follow-mode)
  (key-chord-define-global "wq" #'quit-window)
  (key-chord-define-global ",-" #'text-scale-adjust)

  (key-chord-define-global "w0" #'delete-window)
  (key-chord-define-global "w1" #'delete-other-windows)
  (key-chord-define-global "w2" #'cf/split-and-follow-horizontally)
  (key-chord-define-global "w3" #'cf/split-and-follow-vertically)
  (key-chord-define-global "w4" #'switch-to-buffer-other-window)
  (key-chord-define-global "w5" #'tear-off-window)
  (key-chord-define-global "wt" #'crux-transpose-windows)

  (key-chord-define-global "w." #'xref-find-definitions-other-window)
  (key-chord-define-global "f." #'xref-find-definitions-other-frame)
  (key-chord-define-global "x." #'cf/link-handler)
  (key-chord-define-global "r." #'cf/region-to-linkhandler)
  (key-chord-define-global "z." #'isearch-forward-thing-at-point)
  (key-chord-define-global "s1" #'isearch-forward-symbol-at-point)

  (key-chord-define-global "fg" #'other-window)
  (key-chord-define-global "f0" #'delete-frame)
  (key-chord-define-global "fz" #'suspend-frame)
  (key-chord-define-global "f1" #'find-file-at-point)
  (key-chord-define-global "f4" #'ffap-other-window)
  (key-chord-define-global "f5" #'ffap-other-frame)
  (key-chord-define-global "bm" #'(lambda () (interactive)
                                    (bookmark-set)
                                    (bookmark-save)))
  (key-chord-define-global "bs" #'bookmark-save)
  (key-chord-define-global "m," #'set-mark-command)
  (key-chord-define-global "rt" #'er/expand-region)
  (key-chord-define-global "ml" #'crux-duplicate-current-line-or-region)
  (key-chord-define-global "mw" #'cf/mark-word)
  (key-chord-define-global "m." #'cf/mark-construct-dwim)

  (key-chord-define-global "p]" #'forward-page)
  (key-chord-define-global "p[" #'backward-page)
  (key-chord-define-global "b]" #'next-buffer)
  (key-chord-define-global "b[" #'previous-buffer)
  (key-chord-define-global "bn" #'next-buffer)
  (key-chord-define-global "bk" #'cf/kill-buffer-current)
  (key-chord-define-global "sb" #'save-buffer)

  (key-chord-define-global "rg" #'undo-redo)
  (key-chord-define-global "hu" #'undo)

  (key-chord-define-global "d1" #'dired-jump)
  (key-chord-define-global "d2" #'dired-jump-other-window)

  (key-chord-define-global "o0" #'crux-smart-open-line-above)
  (key-chord-define-global "o1" #'crux-smart-open-line)

  (key-chord-define-global "e1" #'eval-last-sexp)
  (key-chord-define-global "e2" #'elisp-eval-region-or-buffer)
  (key-chord-define-global "e3" #'region-eval)
  (key-chord-define-global "e4" #'crux-eval-and-replace)
  (key-chord-define-global "e5" #'eval-and-replace)

  (key-chord-define-global "i0" #'delete-indentation)
  (key-chord-define-global "i9" #'crux-top-join-line)
  (key-chord-define-global "i1" #'crux-move-beginning-of-line)
  (key-chord-define-global "i2" #'end-of-line)
  (key-chord-define-global "i3" #'forward-button)
  (key-chord-define-global "i4" #'backward-sentence)
  (key-chord-define-global "i5" #'forward-sentence)
  (key-chord-define-global "lp" #'previous-line)
  (key-chord-define-global "l," #'forward-line)

  (key-chord-define-global "bw" #'backward-word)
  (key-chord-define-global "fw" #'forward-word)
  (key-chord-define-global "df" #'cf/lookup-word)
  (key-chord-define-global "tb" #'cf/toggle-mode-line)
  (key-chord-define-global "a0" #'abbrev-mode)

  (key-chord-define-global "km" #'crux-kill-and-join-forward)
  (key-chord-define-global "jn" #'crux-top-join-line)
  (key-chord-define-global "kl" #'cf/copy-whole-line)

  ;; :kc:help
  (key-chord-define-global "hb" #'cf/describe-symbol-at-point)
  (key-chord-define-global "hg" #'cf/describe-function-at-point)
  (key-chord-define-global "hv" #'cf/describe-variable-at-point)
  (key-chord-define-global "hf" #'cf/describe-face-at-point)

)

;; Toggle Prefix Map
(defvar-keymap cf-toggle-prefix-map
  :doc "Toggle Prefix Map"
  "a" #'abbrev-mode
  "b" #'cf/toggle-mode-line
  "c" #'cf/insert-palette-color
  "d" #'eldoc-box-hover-mode
  "e" #'eww-search-words
  "f" #'follow-mode
  "g" #'cf/reset-display-table
  "h" #'hl-line-mode
  "i" #'info
  "j" #'dictionary-tooltip-mode
  "k" #'keycast-mode-line-mode
  "l" #'cf/link-handler
  "m" #'mpc
  "n" #'display-line-numbers-mode
  "o" #'olivetti-mode
  "p" #'paredit-mode
  "q" #'reveal-mode
  "r" #'visual-line-mode
  "s" #'scratch-buffer
  "t" #'toggle-truncate-lines
  "u" #'fill-flowed
  "v" #'view-mode
  "w" #'whitespace-mode
  "y" #'cf/yt-dired
  "z" #'list-keyboard-macros
  "B" #'toggle-debug-on-error
  "D" #'eldoc-box-hover-at-point-mode
  "E" #'emacs-lisp-mode
  "H" #'cf/toggle-header-line
  "T" #'visual-line-mode
  "L" #'cf/region-to-linkhandler
  "O" #'org-mode
  "R" #'cf/mpc-radio-can
  "%" #'isearch-query-replace
  "&" #'cf/async-run
  ";" #'drag-stuff-global-mode
  "`" #'eww-search-words)
(keymap-set global-map "C-`" cf-toggle-prefix-map)

;; Crux Prefix Map
(defvar-keymap cf-crux-prefix-map
  "C-i" #'crux-indent-defun
  "C-u" #'crux-kill-line-backwards
  "k"   #'crux-smart-kill-line
  "o"   #'crux-smart-open-line
  "O"   #'crux-smart-open-line-above
  "e"   #'crux-eval-and-replace
  "j"   #'crux-kill-and-join-forward
  "s"   #'crux-create-scratch-buffer
  "t"   #'crux-transpose-windows
  "i"   #'nerd-icons-insert
  "I"   #'crux-indent-rigidly-and-copy-to-clipboard
  "Y"   #'crux-kill-buffer-truename
  "C"   #'crux-copy-file-preserve-attributes
  "W"   #'crux-sudo-edit
  "L"   #'crux-kill-whole-line
  "R"   #'crux-rename-buffer-and-file
  "E"   #'elisp-eval-region-or-buffer
  "T"   #'crux-insert-date
  ","   #'crux-switch-to-previous-buffer
  ";"   #'crux-duplicate-and-comment-current-line-or-region
  "`"   #'crux-transpose-windows
  "'"   #'crux-swap-windows
  "l"   #'crux-view-url)
(keymap-set global-map "C-'" cf-crux-prefix-map)

;; :gkbd: Global Keymaps

;; Remaps
(keymap-global-set "C-z" #'undo)
(keymap-global-set "C-r" #'undo-redo)
(keymap-global-set "C-=" #'text-scale-adjust)

(keymap-global-set "C-<wheel-up>" #'text-scale-increase)
(keymap-global-set "C-<wheel-down>" #'text-scale-decrease)

(keymap-global-set "C-<kp-add>" #'text-scale-increase)
(keymap-global-set "C-<kp-subtract>" #'text-scale-decrease)

(require 'mark-url-and-save)
(keymap-global-set "C-*" #'cf/mark-url-and-save-dwim)
(keymap-global-set "C-:" #'cf/sr-elvi-search)
(keymap-global-set "C-\\" #'vterm-toggle)
(keymap-global-set "C-M-`" #'eww-search-words)
(keymap-global-set "M-s-\\" #'cf/link-handler)

(defun cf/load-mail ()
  "Interactively loads the mail account config file."
  (interactive)
  (if (file-exists-p "~/.emacs.d/custom/accounts.el")
      (load-file "~/.emacs.d/custom/accounts.el")
    (keymap-global-set "C-x m" #'sendmail-query-once)))
(keymap-global-set "C-x m" #'cf/load-mail)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)


;;; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'org-babel-post-tangle-hook
          #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'activate-mark-hook (lambda() (setq cursor-type 'bar)))
(add-hook 'deactivate-mark-hook (lambda() (setq cursor-type t)))

(setq sentence-end-double-space nil)
(defvar modes-with-autofill
  '(text-mode-hook message-mode-hook
    adoc-mode-hook markdown-mode-hook
    emacs-lisp-mode-hook org-mode-hook
    lisp-interaction-mode-hook)
  "Modes that benefit from auto-fill mode.")

(mapc (lambda (mode)
        (add-hook mode #'turn-on-auto-fill))
      modes-with-autofill)



;;; _
(provide 'init)
;; Local Variables:
;; coding: utf-8
;; fill-column: 90
;; require-final-newline: t
;; no-byte-compile: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here

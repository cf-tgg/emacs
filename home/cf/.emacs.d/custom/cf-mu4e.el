;;; cf-mu4e.el -- Accounts config. -*- lexical-binding: t; -*-

;;; Commentary:
;;     Only loaded on the box with proper authsource.

;;; Code:

(use-package nano-mu4e
  :ensure nil
  :after mu4e
  :load-path "~/.emacs.d/custom/"
  :commands mu4e
  :custom-face
  (nano-default :family "Iosevka Nerd Font Mono")
  :custom
  (nano-mu4e-style 'simple))

;;; Mu4e
(use-package mu4e
  :ensure nil
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :init
  (require 'mu4e)
  (require 'transient)
  (require 'mu4e-transient)
  (require 'mu4e-org)
  (require 'eww)
  (require 'shrface)
  (require 'shr-tag-pre-highlight)
  (require 'nano-mu4e)
  :custom
  (epa-file-inhibit-auto-save t)
  (auth-sources '("/home/cf/.config/mu4e/authinfo.gpg"))
  (auth-source-debug t)
  (mu4e-mu-binary "/usr/local/bin/mu")
  (mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbsyncrc -a")
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-update-interval (* 45 60))
  (mu4e-org-support t)
  (mu4e-eldoc-support t)
  (mu4e-modeline-support t)
  (mu4e-notification-support t)
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)
  (mu4e-view-scroll-to-next t)
  (mu4e-compose-keep-self-cc nil)
  (message-dont-reply-to-names t)
  (mu4e-context-policy 'pick-first)
  (mu4e-index-update-error-warning t)
  (mu4e-sent-messages-behavior 'sent)
  (mu4e-message-kill-buffer-on-exit t)
  (mu4e-thread-fold-single-children t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-modeline-prefer-bookmark-name t)
  :bind
  (:map global-map
        ("C-x m" . mu4e-compose-new) ; `compose-mail' override
        ("C-c M" . mu4e-transient-menu))

  :config
  (setq mu4e-split-view 'single-window)
  (setq user-mail-address "felix.6agnon@gmail.com"
        user-full-name "Félix Gagnon")
  (setq message-signature-file "/home/cf/.local/keys/.signature")
  (setq mu4e-attachment-dir "/home/cf/.local/mail/felix.6agnon@gmail/[Gmail]/Downloads"
        mu4e-sent-folder "/felix.6agnon@gmail/[Gmail]/Sent Mail"
        mu4e-drafts-folder "/felix.6agnon@gmail/[Gmail]/Drafts"
        mu4e-trash-folder "/felix.6agnon@gmail/[Gmail]/Trash"
        mu4e-refile-folder "/felix.6agnon@gmail/[Gmail]/All Mail")

  (setq mu4e-headers-fields
        '((:human-date . 12) (:flags . 6) (:from . 22) (:subject)))

  (setq mu4e-modeline-all-clear    '("C:" . "   "))
  (setq mu4e-modeline-all-read     '("R:" . "   "))
  (setq mu4e-modeline-unread-items '("U:" . "   "))
  (setq mu4e-modeline-new-items    '("N:" . " 🔥 "))

  (setq mu4e-bookmarks
        '((:name "Courriels" :key ?i :query "NOT flag:trashed")
          (:name "Hui"       :key ?h :query "date:today..now NOT flag:trashed")
          (:name "Semaine"   :key ?s :query "date:7d..now AND NOT flag:trashed")
          (:name "Mois"      :key ?m :query "date:31d..now AND NOT flag:trashed")
          (:name "Images"    :key ?I :query "mime:image/*")))

  (setq mu4e-maildir-shortcuts
        '((:name "All"       :maildir "/felix.6agnon@gmail/[Gmail]/All Mail"  :key ?a)
          (:name "Sent"      :maildir "/felix.6agnon@gmail/[Gmail]/Sent Mail" :key ?s)
          (:name "Trash"     :maildir "/felix.6agnon@gmail/[Gmail]/Trash"     :key ?t)
          (:name "Drafts"    :maildir "/felix.6agnon@gmail/[Gmail]/Drafts"    :key ?d)
          (:name "Downloads" :maildir "/felix.6agnon@gmail/[Gmail]/Downloads" :key ?D)
          (:name "Archived"  :maildir "/felix.6agnon@gmail/Archive"           :key ?A)
          (:name "Inbox"     :maildir "/felix.6agnon@gmail/Inbox"             :key ?i)
          (:name "schat"     :maildir "/sshchat" :key ?l)))

  (defun m/mu4e-no-header-line ()
    (setq-local header-line-format nil))
  (add-hook 'mu4e-headers-mode-hook #'m/mu4e-no-header-line)

  (defvar mu4e-split-resize-map
    (let ((map (make-sparse-keymap)))
      (define-key map "-" #'mu4e-headers-split-view-shrink)
      (define-key map "=" #'mu4e-headers-split-view-grow)
      map)
    "Keymap activated after `C-x -/+` to allow consecutive resizing.")

  (defun m/activate-mu4e-resize-map ()
    "Activate mu4e transient map for mu4e headers split resizing."
    (set-transient-map mu4e-split-resize-map))

  (defun m/mu4e-split-shrink ()
    "Shrink mu4e headers window recursively."
    (interactive)
    (when (mu4e--view-split-view-p)
      (mu4e-headers-split-view-shrink)
      (m/activate-mu4e-resize-map)))

  (defun m/mu4e-split-grow ()
    "Grow mu4e headers window recursively."
    (interactive)
    (when (mu4e--view-split-view-p)
      (mu4e-headers-split-view-grow)
      (m/activate-mu4e-resize-map)))

  (keymap-set mu4e-view-mode-map "C-x -" #'m/mu4e-split-shrink)
  (keymap-set mu4e-view-mode-map "C-x =" #'m/mu4e-split-grow)

  (defun m/mu4e-view-hooks ()
    "Hooks to be run in mu4e articles."
    (shrface-mode 1)
    (whitespace-mode -1)
    (global-whitespace-mode -1)
    (display-line-numbers-mode -1)
    (setq buffer-display-table nil)
    (olivetti-mode 1))

  (add-hook 'mu4e-view-rendered-hook #'m/mu4e-view-hooks)

   ;;;; Smtp
  (require 'smtpmail)
  (setq smtpmail-queue-dir "~/.local/mail")
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq starttls-use-gnutls t)
  (setq smtpmail-starttls-credentials
        '(("smtp.gmail.com" 587 nil nil)))
  (setq smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "felix.6agnon@gmail.com" nil)))
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-stream-type 'starttls)
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "felix.6agnon@gmail.com" nil)))
  (when (mu4e t)
    (mu4e-modeline-mode 1)))


;;; _
(provide 'cf-mu4e)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; cf-mu4e.el ends here

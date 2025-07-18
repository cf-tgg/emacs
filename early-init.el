;;; early-init.el --- Emacs early-init-file -*- lexical-binding: t; -*-

;;; Commentary:
;;    See: (info "(emacs) Init File")

;;; Code:

(setq package-enable-at-startup t)

(defun cf-add-to-list (list element)
  "Add to symbol of LIST the given ELEMENT.
Simplified version of `add-to-list'.
From Protesilaos Stravou \"dotemacs\": <https://protesilaos.com/emacs/dotemacs/>."
  (set list (cons element (symbol-value list))))

(mapc
 (lambda (var)
   (cf-add-to-list var '(width . (text-pixels . 900)))
   (cf-add-to-list var '(height . (text-pixels . 900))))
 '(default-frame-alist initial-frame-alist))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%F")
      ring-bell-function 'ignore
      use-dialog-box nil
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

;; From Protesilaos' backlog <https://protesilaos.com/emacs/dotemacs/>
(defun cf-emacs-avoid-initial-flash-of-light ()
  "Sets dark background and fonts for initial frame."
  (setq-default mode-line-format nil)
  (set-face-attribute 'default nil :family "Iosevka Nerd Font" :height 100 :background "#181818" :foreground "#f0f3f0")
  (set-face-attribute 'mode-line nil :family "Iosevka Nerd Font Mono" :background "#282828" :foreground "#484848")
  (set-face-attribute 'variable-pitch nil :family "Iosevka Nerd Font Propo" :background "#181818")
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font Mono" :background "#181818"))
(cf-emacs-avoid-initial-flash-of-light)

(modify-all-frames-parameters
 '((right-divider-width . 4)
   (internal-border-width . 4)
   (alpha 99 96)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)

(setq-default initial-scratch-message ";; -*- lexical-binding: t; -*-\n\n")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; (setq gc-cons-threshold (* 100 1000 1000))
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1)))

(add-hook 'after-init-hook
          (lambda ()
            (message "Init. en %.2f s. avec %d vidanges"
                     (float-time
                      (time-subtract after-init-time before-init-time))
                     gcs-done)
            (setq read-process-output-max (* 1024 1024))
            (let ((version (car (split-string emacs-version " ("))))
              (set-frame-name (concat "Emacs " version)))))

;; Speed up package initialization.
(setopt package-quickstart nil)


;; -
(provide 'early-init)
;; Local Variables:
;; coding: utf-8-emacs
;; fill-column: 80
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:
;;; early-init.el ends here

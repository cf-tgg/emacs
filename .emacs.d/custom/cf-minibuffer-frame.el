;;; cf-minibuffer-frame.el --- MInibuffer-Only Frame -*- lexical-binding: t -*-

;;; Commentary:
;;     cf. Info node `(emacs)Minibuffer'.
;;    Additionnal WM specific configuration is needed for this frame.

;;    For example, in my dwm's config.h file:
;;      #define EMACSCLASS = "emacs"
;;
;;      static Sp scratchpads[] = {
;;          /* name         cmd  */
;;          {\"minibf\",     spcmd6},
;;      };
;;
;;      typedef struct {
;;        const char *name;
;;        const void *cmd;
;;      } Sp;
;;
;;      const char *spcmd6[] = {
;;          \"emacsclient\", \"-c\", \"--alternate-editor=\",
;;          \"--socket-name=/run/user/1000/emacs/server\",
;;          \"-e\", \"(cf-emacs-minibuffer-frame)\", NULL
;;      };
;;
;;      static const Rule rules[] = {
;;          {EMACSCLASS,  NULL, \"minibuffer\", SPTAG(5), 1, 0.96, 0.93, 0, 1, -1},
;;      };
;;
;; (setq display-buffer-alist
;;       '(("\\*\\(Compile-Log\\|[Hh]elp\\|Messages\\)\\*"
;;          (display-buffer-in-direction display-buffer-in-side-window)
;;          (window-width . 0.5)
;;          (side . right)
;;          (slot . 1)
;;          (window-parameters . ((mode-line-format " " "%b"))))

;;         ("\\*scratch\\*"
;;          (display-buffer-reuse-mode-window
;;           display-buffer-below-selected
;;           display-buffer-in-direction)
;;          (window-heigth . 0.2)
;;          (window-width . 0.5)
;;          (side . right)
;;          (slot . 1)
;;          (window-parameters . ((mode-line-format " " "%b"))))

;;         ("\\*Occur\\*"
;;          (display-buffer-in-side-window display-buffer-reuse-mode-window)
;;          (window-heigth . 0.2)
;;          (side . bottom)
;;          (slot . 1)
;;          (window-parameters . ((mode-line-format " " "%b"))))

;;         ("\\*\\(Backtrace\\|Warnings\\|Disabled Command\\|mu4e-last-update\\|Async-native-compile-log\\)\\*"
;;          (display-buffer-no-window)
;;          (allow-no-window . t)
;;          (inhibit-same-window . t))))

;; (add-to-list 'display-buffer-alist
;;              '("\\*Completions\\*"
;;                (display-buffer-in-side-window)
;;                (side . bottom)
;;                (window-height . fit-window-to-buffer)))

;; Minibuffer config.
;; (use-package minibuffer
;;   :ensure nil
;;   :custom
;;   (y-or-n-p-use-read-key t)
;;   (max-mini-window-height nil)
;;   (minibuffer-visible-completions t)
;;   (minibuffer-visible-completions--always-bind t)
;;   (minibuffer-follows-selected-frame nil)
;;   (minibuffer-scroll-window t)
;;   (completion-cycle-threshold 3)
;;   (resize-mini-windows 'grow-only)
;;   (resize-mini-frames t)
;;   (minibuffer-auto-raise t)
;;   (minibuffer-completing-file-name t)
;;   (minibuffer-allow-text-properties t)
;;   :bind
;;   (:map minibuffer-mode-map
;;         ("C-n" . minibuffer-next-line-completion)
;;         ("C-p" . minibuffer-previous-line-completion)
;;         :map minibuffer-local-map
;;         ("<return>"  . minibuffer-complete-and-exit)
;;         ("<escape>"  . abort-recursive-edit)
;;         ("<tab>"     . minibuffer-complete-defaults)
;;         ("C-."       . minibuffer-complete-word)
;;         ("C-a"       . minibuffer-beginning-of-buffer)
;;         :map minibuffer-visible-completions-map
;;         ("<backtab>"  . minibuffer-previous-completion)
;;         ("<tab>"      . minibuffer-next-completion)
;;         ("C-p"        . minibuffer-previous-line-completion)
;;         ("C-n"        . minibuffer-next-line-completion)
;;         ("<return>"   . minibuffer-choose-completion-or-exit)
;;         ("M-RET"      . minibuffer-force-complete-and-exit)
;;         ("C-g"        . minibuffer-hide-completions)))
;;
;;  Dedicated Minibuffer Frame
;;  (require 'cf-minibuffer-frame)
;;  (setq default-minibuffer-frame (cf/minibuffer-frame))
;;   (setq default-frame-alist
;;        (append
;;         '((minibuffer . nil))
;;         default-frame-alist))
;;   (setq minibuffer-frame-alist
;;        '((name . "minibuffer")
;;          (minibuffer . only)
;;          (undecorated . t)
;;          (visibility . nil)
;;          (skip-taskbar . t)
;;          (tool-bar-lines . 0)
;;          (menu-bar-lines . 0)
;;          (internal-border-width . 10)
;;          (width . 140)
;;          (height . 1)
;;          (left . 600)
;;          (top . 100)))

;;; Code:

(defvar cf-minibuffer-frame nil
  "The dedicated minibuffer-only frame.")

;;;###autoload
(defun cf/minibuffer-frame ()
  "Create or return the existing minibuffer-only frame."
  (interactive)
  (unless (and (frame-live-p cf-minibuffer-frame)
               (eq (frame-parameter cf-minibuffer-frame 'minibuffer) 'only))
    (setq cf-minibuffer-frame
          (make-frame
           '((name . "minibuffer")
             (minibuffer . only)
             (fullscreen . 0)
             (undecorated . t)
             (auto-raise . t)
             (tool-bar-lines . 0)
             (menu-bar-lines . 0)
             (internal-border-width . 10)
             (width . 140)
             (height . 1)
             (visibility . nil)
             (skip-taskbar . t)
             (left . 600)
             (top . 100)))))
  cf-minibuffer-frame)

(defun cf--init-minibuffer-frame ()
  "Initialize a reusable hidden minibuffer-only frame at startup."
  (setq cf-minibuffer-frame (cf/minibuffer-frame))
  (setq default-minibuffer-frame cf-minibuffer-frame))
(add-hook 'after-init-hook #'cf--init-minibuffer-frame)

;;; _
(provide 'cf-minibuffer-frame)
;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; require-final-newline: t
;; indent-tabs-mode: nil
;; End:
;;; cf-minibuffer-frame.el ends here

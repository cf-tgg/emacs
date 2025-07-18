;;; app-launchers.el --- Possible alternatives to dmenu/rofi -*- lexical-binding: t -*-

;;; Commentary:
;;    Counsel-Linux-App
;;    Since we have counsel installed, we can use 'counsel-linux-app' to launch our Linux apps.
;;    It list the apps by their executable command, so it's kind of tricky to use.

;;; Code:

;; App-Launcher
;; The 'app-launcher' is a better run launcher since it reads the desktop applications on your system and you can search them by their names as defined in their desktop file.
;;  This means that sometimes you have to search for a generic term rather than the actual binary command of the program.
;; create a global keyboard shortcut with the following code
;; emacsclient -cF "((visibility . nil))" -e "(emacs-run-launcher)"

;; (use-package app-launcher
;;   :ensure '(app-launcher :host github :repo "SebastienWae/app-launcher"))
;; (use-package app-launcher
;;   :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

(require 'app-launcher)

(defun cf/emacs-counsel-launcher ()
  "Generate emacs-counsel-launcher minibuffer frame.
Runs counsel-linux-app on that frame, which has dmenu-like behavior.
Delete the frame after entered command has exited."
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                    (minibuffer . only)
                    (fullscreen . 0)
                    (undecorated . t)
                    (auto-raise . t)
                    (tool-bar-lines . 0)
                    (menu-bar-lines . 0)
                    (internal-border-width . 10)
                    (width . 80)
                    (height . 11)))
    (unwind-protect
        (counsel-linux-app)
      (delete-frame))))

(defun cf/emacs-run-launcher ()
  "Run app-launcher-run-app in minibuffer dedicated frame."
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                    (minibuffer . only)
                    (fullscreen . 0)
                    (undecorated . t)
                    (auto-raise . t)
                    (tool-bar-lines . 0)
                    (menu-bar-lines . 0)
                    (internal-border-width . 10)
                    (width . 100)
                    (height . 1))
                  (unwind-protect
                      (app-launcher-run-app)
                    (delete-frame)))))



;;; _
(provide 'app-launchers)
;;; app-launchers.el ends here

;;; spmacs.el --- Named scratchpad Emacs frame for dwm -*- lexical-binding: t -*-

;;; Commentary:
;;   Emacsclient popup frame for the WM's

;;; Code:

(eval-when-compile
  (require 'frame)
  (require 'cl-lib))

;;;###autoload
(defun spmacs/toggle-frame ()
  "Toggle visibility of the 'spmacs' scratchpad frame."
  (interactive)
  (let ((frame
         (cl-find-if (lambda (f)
                       (equal (frame-parameter f 'name) "spmacs"))
                     (frame-list))))
    (if frame
        (if (frame-visible-p frame)
            (make-frame-invisible frame)
          (make-frame-visible frame))
      (make-frame
       '((name . "spmacs")
         (title . "spmacs")
         (width . 100)
         (height . 30)
         (minibuffer . t)
         (visibility . nil)
         (auto-raise . t)
         (skip-taskbar . t)
         (undecorated . nil)
         (server-frame)
         (unsplittable . t)
         (frame-title-format . '("%F")))))))
(keymap-global-set "C-c s" #'spmacs/toggle-frame)

(add-hook 'after-init-hook #'spmacs/toggle-frame)


;;; _
(provide 'spmacs)
;;; spmacs.el ends here

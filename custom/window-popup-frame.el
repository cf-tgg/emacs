;;; window-popup-frames.el --- Run commands in a popup frame -*- lexical-binding:t -*-

;;; Commentary:

;;    Raw link: https://www.youtube.com/watch?v=vbWxT8tht9A

;;    In this 15-minute video I show the custom code I have to run any Emacs command
;;    in a popup frame. The advantage of this is that I do not need to be in Emacs
;;    already. The examples I show cover org-capture and my tmr package.

;;    This is the code I used in the video. Remember to assign the emacsclient calls
;;    to a system-level key binding.

;;; Code:

(defun cf-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `cf-window-popup-frame'.
      Use this function via a hook."
  (when (frame-parameter nil 'cf-window-popup-frame)
    (delete-frame)))

(defmacro cf-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
      Make the new frame have the `cf-window-popup-frame' parameter."
  `(defun ,(intern (format "cf-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `cf-window-popup-frame' parameter.
      Also see `cf-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((cf-window-popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " cf-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

;;;###autoload (autoload 'cf-window-popup-org-capture "cf-window")
(cf-window-define-with-popup-frame org-capture)
(add-hook 'org-capture-after-finalize-hook #'cf-window-delete-popup-frame)

;; (declare-function tmr "tmr" (time &optional description acknowledgep))
;; (defvar tmr-timer-created-functions)

;; ;;;###autoload (autoload 'cf-window-popup-tmr "cf-window")
;; (cf-window-define-with-popup-frame tmr)
;; (add-hook 'tmr-timer-created-functions #'cf-window-delete-popup-frame)

;;; The emacsclient call depends on the daemon or `server-mode' (I use the latter)

;;; The emacsclient calls this ought to be bound to system-wide keys
;; emacsclient -e '(cf-window-popup-org-capture)'
;; emacsclient -e '(cf-window-popup-tmr)'



;;; _
(provide 'window-popup-frame)
;; Local Variables:
;; coding: utf-8
;; fill-column: 90
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:
;;; window-pop-up-frames.el ends here

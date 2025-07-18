;;; cf-popup-frame.el --- Popup frame management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 cf

;; Author: cf <cf.gg.tty@protonmail.com>
;; URL: <https://github.com/cf-tgg/>
;; Gitlab: <https://gitlab.com/cf-gg/>
;; Codeberg: <https://codeberg.org/cfggtty/>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, frames

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; Adapted from "popup-frame.el" (C) 2011
;; written by Guillaume Papin <guillaume.papin@epitech.eu>

;;; Commentary:

;; Utility functions for managing undecorated popup frames.
;; These are borderless, title-less, taskbar-skipped subwindows that
;; can be used for transient UI like completions, popups, etc.
;;
;; Currently supported only on X window systems.

;;; Usage example:

;; (let ((the-frame (popup-frame-create :title "Popup" :name "popup-frame")))
;;   (popup-frame-show the-frame)
;;   (set-frame-position the-frame 200 200)
;;   (set-frame-size the-frame 80 24)
;;   (popup-frame-hide the-frame)
;;   (popup-frame-destroy the-frame))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defgroup popup-frame nil
  "Popup frame management."
  :version "30.1"
  :group 'frames)

(defcustom popup-frame-x-window-type "_NET_WM_WINDOW_TYPE_UTILITY"
  "X window type for popup frames.
This informs the window manager how to handle the frame.
For example, it can prevent tiling in tiling window managers.

See: https://specifications.freedesktop.org/wm-spec/latest/ar01s05.html"
  :type 'string
  :group 'popup-frame)

(defun popup-frame/x-make-floating (&optional frame)
  "Set `_NET_WM_WINDOW_TYPE' on FRAME to mark it as floating.
Defaults to the currently selected frame if FRAME is nil."
  (when (and (eq window-system 'x) popup-frame-x-window-type)
    (x-change-window-property "_NET_WM_WINDOW_TYPE"
                              (list popup-frame-x-window-type)
                              (or frame (selected-frame))
                              "ATOM" 32 t)))

(defun popup-frame/x-skip-taskbar (&optional frame)
  "Request that FRAME be skipped from taskbar.
Defaults to the currently selected frame if FRAME is nil."
  (when (eq window-system 'x)
    (let ((target (or frame (selected-frame))))
      (x-send-client-message target 0 target "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_SKIP_TASKBAR" 0)))))

(cl-defun popup-frame-create (&key title name minibuffer scroll-bar)
  "Create an invisible popup frame and return it.
Call `popup-frame-show' to make it visible.

TITLE: String, window title.
NAME: String, frame name.
MINIBUFFER: Non-nil means create a minibuffer.
SCROLL-BAR: 'left or 'right enables a scrollbar on that side."
  (unless (eq window-system 'x)
    (error "popup-frame: only supported on X window systems"))
  (let* ((after-make-frame-functions
          (cons #'popup-frame/x-make-floating after-make-frame-functions))
         (frame-params `((window-system . x)
                         (minibuffer . ,minibuffer)
                         (visibility . nil)
                         (border-width . 0)
                         (internal-border-width . 0)
                         (menu-bar-lines . 0)
                         (tool-bar-lines . 0)
                         (left-fringe . 0)
                         (right-fringe . 0))))
    (when title
      (push `(title . ,title) frame-params))
    (when name
      (push `(name . ,name) frame-params))
    (when scroll-bar
      (push `(vertical-scroll-bars . ,scroll-bar) frame-params))
    (make-frame frame-params)))

(defalias 'popup-frame-destroy #'delete-frame)

(defun popup-frame-show (frame)
  "Make FRAME visible and request taskbar skipping."
  (make-frame-visible frame)
  (popup-frame/x-skip-taskbar frame))

(defun popup-frame-hide (frame)
  "Make FRAME invisible."
  (make-frame-invisible frame t))



;;; _
(provide 'cf-popup-frame)
;;; cf-popup-frame.el ends here

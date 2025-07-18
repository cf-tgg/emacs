;;; cf-pjumper.el ---- Placeholder Jumper -*- lexical-binding: t; -*-

;; Copyright (C) 2025 cf dot gg

;; Author: cf <cf.gg.tty@protonmail.com>
;; URL: <https://github.com/cf-tgg/>
;; Gitlab: <https://gitlab.com/cf-gg/>
;; Codeberg: <https://codeberg.org/cfggtty/>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;;  This package attempts to port my old vim bindings:
;;
;; ```vimscript
;;
;;  " Placeholder navigation through  portals
;;    map ,, :keepp /<++><CR>ca<
;;    imap ,, <esc>:keepp /<++><CR>ca<
;;
;;  " Drop some portals
;;  " [a]ppend [p]ortal after contiguous text under the cursor
;;    nnoremap ,ap :normal! viWA <++><Esc>
;;  " [i]nsert [p]ortal before the word under the cursor
;;    nnoremap ,ip :normal! bi<++> <Esc>2w
;;  " Replace contiguous text under the cursor with a portal, then selects
;;  " the next word (in case you'd want to replace it with a portal too)
;;    nnoremap \, :normal viWc<++><ESC>wviW
;;  " Replace contiguous text with portals while in visual modes too
;;    xnoremap \, <ESC>viWc<++><ESC>wviW
;;
;;  " Quick enclosures
;;    map \l i[<Esc>ea]<Esc>
;;    map \p i(<Esc>ea)<Esc>
;;    map \c i{<Esc>ea}<Esc>
;;    map \" i"<Esc>ea"<Esc>
;;    map \q i"<Esc>ea"<Esc>
;;    map \' i'<Esc>ea'<Esc>
;;    map \` i`<Esc>ea`<Esc>
;;
;;  " select inside enclosures
;;    nnoremap ;p vi)
;;    nnoremap ;< vi>
;;
;;  ```

;;; Code:


(defvar cf-portal-char "\x1F"
  "Invisible character used as a jump portal in editing buffers.")

(defun cf/insert-portal-at-point ()
  "Append a portal after the current word."
  (interactive)
  (insert cf-portal-char))

(defun cf/insert-portal-after ()
  "Append a portal after the current word."
  (interactive)
  (save-excursion
    (forward-word)
    (insert " " cf-portal-char)))

(defun cf/insert-portal-before ()
  "Insert a portal before the current word."
  (interactive)
  (save-excursion
    (backward-word)
    (insert cf-portal-char " ")))

(defun cf/replace-word-with-portal (&optional beg end)
  "Replace word at point with portal and jump to next word.
If region is active, replace marked region with portal instead."
  (interactive "r")
  (if (use-region-p)
      (progn
        (delete-region beg end)
        (insert cf-portal-char)
        (forward-word))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (delete-region (car bounds) (cdr bounds))
        (insert cf-portal-char)
        (forward-word)))))

(defun cf/jump-to-next-portal ()
  "Jump to next portal character."
  (interactive)
  (when (search-forward cf-portal-char nil t)
    (backward-delete-char 1)))

(defun cf/jump-to-prev-portal ()
  "Jump to previous portal character."
  (interactive)
  (when (search-backward cf-portal-char nil t)
    (delete-char 1)))

(defun cf/replace-region-with-portal (beg end)
  "Replace selected region with a portal."
  (interactive "r")
  (delete-region beg end)
  (insert cf-portal-char)
  (forward-word))

;; Bindings (with `evil` if available, else use standard keys)
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
                   (kbd ",ap") #'cf/insert-portal-after
                   (kbd ",ip") #'cf/insert-portal-before
                   (kbd "\\,") #'cf/replace-word-with-portal
                   (kbd ",,") #'cf/jump-to-next-portal)
  (evil-define-key 'insert 'global
                   (kbd ",,") (lambda ()
                                (interactive)
                                (evil-normal-state)
                                (cf/jump-to-next-portal)
                                (evil-insert-state)))
  (evil-define-key 'visual 'global
                   (kbd "\\,") #'cf/replace-region-with-portal))

;; Fallback bindings if not using evil
(defvar-keymap cf-pjumper-prefix-map
  :doc ""
  "[" #'cf/insert-portal-before
  "]" #'cf/insert-portal-after
  "." #'cf/insert-portal-at-point
  "," #'cf/replace-word-with-portal
  "r" #'cf/replace-region-with-portal
  "n" #'cf/jump-to-next-portal
  "p" #'cf/jump-to-prev-portal)
(keymap-set global-map "M-`" cf-pjumper-prefix-map)

(global-set-key (kbd "M-[") #'cf/jump-to-prev-portal)
(global-set-key (kbd "M-]") #'cf/jump-to-next-portal)

(defvar cf-pjumper-mode-map (make-sparse-keymap)
  "Keymap for `cf-pjumper-mode`.")

(define-minor-mode cf-pjumper-mode
  "Toggle cf-pjumper-mode.
Provides portal-based editing navigation and manipulation."
  :global t
  :lighter " PJump"
  :keymap cf-pjumper-mode-map
  (if cf-pjumper-mode
      (progn
        (keymap-set cf-pjumper-mode-map "M-`" cf-pjumper-prefix-map)
        (keymap-set cf-pjumper-mode-map "M-]" #'cf/jump-to-next-portal)
        (keymap-set cf-pjumper-mode-map "M-[" #'cf/jump-to-prev-portal))
    (progn
      (keymap-unset cf-pjumper-mode-map "M-`")
      (keymap-unset cf-pjumper-mode-map "M-[")
      (keymap-unset cf-pjumper-mode-map "M-]"))))


;;; _
(provide 'cf-pjumper)
;;; cf-pjumper.el ends here

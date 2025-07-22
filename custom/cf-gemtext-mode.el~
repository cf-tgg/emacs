;;; gemtext-mode.el ---- Major-mode for editing gemtext -*- lexical-binding: t; -*-

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
;;  Defining a derived-mode from scratch as elisp practice while
;;  learning the gemtext specification.

;;; Code:

(define-derived-mode gemtext-mode text-mode "Gemtext"
  "Major mode for editing Gemtext files.")

(defvar gemtext-mode-abbrev-table nil
  "Abbrev table used in `gemtext-mode`.")

(define-abbrev-table 'gemtext-mode-abbrev-table
  '(("ul" "" abrv/insert-gemtext-ul :system t)))

(defun abrv/insert-gemtext-ul ()
  "Insert Gemtext unordered list item."
  (insert "* ")
  (backward-char))

(defun cf/insert-gemtext-header-1 (&optional n)
  "Insert N Gemtext level 1 headers. Defaults to 1."
  (interactive "p")
  (dotimes (_ n)
    (insert "# \n"))
  (backward-char))

(defun cf/insert-gemtext-header-2 (&optional n)
  "Insert N Gemtext level 2 headers. Defaults to 1."
  (interactive "p")
  (dotimes (_ n)
    (insert "## \n"))
  (backward-char))

(defun cf/insert-gemtext-header-3 (&optional n)
  "Insert N Gemtext level 3 headers. Defaults to 1."
  (interactive "p")
  (dotimes (_ n)
    (insert "### \n"))
  (backward-char))

(defun cf/insert-gemtext-ul (&optional n)
  "Insert N Gemtext unordered list bullets. Defaults to 1."
  (interactive "p")
  (dotimes (_ n)
    (insert "* \n"))
  (backward-char))

(defun cf/insert-gemtext-link (&optional n)
  "Insert N Gemtext link. Defaults to 1."
  (interactive "p")
  (dotimes (_ n)
    (insert "=> gemini://.gmi\n"))
  (backward-sentence))

(defvar-keymap gemtext-prefix-map
  "1" #'cf/insert-gemtext-header-1
  "2" #'cf/insert-gemtext-header-2
  "3" #'cf/insert-gemtext-header-3
  "u" #'cf/insert-gemtext-ul
  "l" #'cf/insert-gemtext-link)
(keymap-set global-map "C-\"" gemtext-prefix-map)

(defvar gemtext-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") #'cf/insert-gemtext-ul)
    (define-key map (kbd "C-c C-h") #'cf/insert-gemtext-ul)
    (define-key map (kbd "C-c C-l") #'cf/insert-gemtext-link)
    map)
  "Keymap for `gemtext-mode`.")

(setq-local abbrev-mode t)


;;; _
(provide 'gemtext-mode)
;;; gemtext-mode.el ends here

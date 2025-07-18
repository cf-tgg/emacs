;;; cf-scratch.el --- Scratch buffers for editable major mode of choice -*- lexical-binding: t -*-

;;  Copyright © 2025 cf dot gg

;;  Author: cf <cf.gg.tty@protonmail.com>
;;  URL: <https://github.com/cf-tgg/>
;;  Gitlab: <https://gitlab.com/cf-gg/>
;;  Codeberg: <https://codeberg.org/cfggtty/>
;;  Version: 0.1.0
;;  Package-Requires: ((emacs "30.1"))

;;  This file is NOT part of GNU Emacs.

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;; Commentary:
;;     Set up a scratch buffer for an editable major mode of choice.
;;     The idea is based on the `scratch.el' package by Ian Eure:
;;     <https://github.com/ieure/scratch-el>.

;;; Code:

(require 'cf-common)

(defgroup cf-scratch ()
  "Scratch buffers for editable major mode of choice."
  :group 'editing)

(defcustom cf-scratch-default-mode 'text-mode
  "Default major mode for `cf-scratch-scratch-buffer'."
  :type 'symbol
  :group 'cf-scratch)

(defcustom cf-scratch-mode-comment-map
  '(
    sh-mode abrv/insert-shebang
    emacs-lisp-mode initial-scratch-message
    python-mode "# Python scratch buffer.\n\n"
    c-mode "/* C scratch buffer */\n\n"
    org-mode "#+title: Org Scratch Buffer\n\n"
    )
  "Plist mapping `major-mode` symbols to:
- a string (inserted directly),
- a variable (evaluated as a string),
- or a function (called to produce a string or perform insertion)."
  :type '(plist :key-type symbol :value-type sexp)
  :group 'cf-scratch)

(defun cf-scratch--scratch-list-modes ()
  "List known major modes."
  (let (symbols)
    (mapatoms
     (lambda (symbol)
       (when (and (functionp symbol)
                  (or (provided-mode-derived-p symbol 'text-mode)
                      (provided-mode-derived-p symbol 'prog-mode)))
         (push symbol symbols))))
    symbols))

(defun cf-scratch--insert-comment ()
  "Insert a comment header appropriate for the current `major-mode`.
Uses `cf-scratch-mode-comment-map` if the buffer is empty. Values can be
a string, a variable, or a function that returns or inserts a string."
  (when (and (cf-common-empty-buffer-p)
             (stringp comment-start)
             (not (string-empty-p comment-start)))
    (let ((start (point))
          (entry (plist-get cf-scratch-mode-comment-map major-mode))
          (inserted nil))
      (cond
       ((functionp entry)
        (let ((ret (funcall entry)))
          (when (stringp ret)
            (insert ret)
            (setq inserted t))))
       ((and (symbolp entry) (boundp entry))
        (let ((val (symbol-value entry)))
          (when (stringp val)
            (insert val)
            (setq inserted t))))
       ((stringp entry)
        (insert entry)
        (setq inserted t))
       (t
        (insert (format "Scratch buffer for: %s\n\n" (symbol-name major-mode))
                )
        (setq inserted t)))
      ;; Only comment-region if the inserted content wasn't already commented
      (when (and inserted
                 ;; Check if the inserted region starts without a comment prefix
                 (save-excursion
                   (goto-char start)
                   (not (looking-at (regexp-quote comment-start)))))
        (comment-region start (point))))))

(defun cf-scratch--prepare-buffer (region &optional mode)
  "Add contents to scratch buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let ((major (or mode major-mode)))
    (with-current-buffer (pop-to-buffer (format "*%s scratch*" major))
      (funcall major)
      (cf-scratch--insert-comment)
      (goto-char (point-max))
      (unless (string-empty-p region)
        (when (cf-common-line-regexp-p 'non-empty)
          (insert "\n\n"))
        (insert region)))))

(defvar cf-scratch--major-mode-history nil
  "Minibuffer history of `cf-scratch--major-mode-prompt'.")

(defun cf-scratch--major-mode-prompt ()
  "Prompt for major mode and return the choice as a symbol."
  (intern
   (completing-read "Select major mode: "
                    (cf-scratch--scratch-list-modes)
                    nil
                    :require-match
                    nil
                    'cf-scratch--major-mode-history)))

(defun cf-scratch--capture-region ()
  "Capture active region, else return empty string."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

;;;###autoload
(defun cf-scratch-buffer (&optional arg)
  "Produce a scratch buffer matching the current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `cf-scratch-default-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.  Candidates are derivatives of `text-mode' or
`prog-mode'.

If region is active, copy its contents to the new scratch
buffer.

Buffers are named as *MAJOR-MODE scratch*.  If one already exists
for the given MAJOR-MODE, any text is appended to it."
  (interactive "P")
  (let ((region (cf-scratch--capture-region)))
    (pcase (prefix-numeric-value arg)
      (16 (cf-scratch--prepare-buffer region (cf-scratch--major-mode-prompt)))
      (4 (cf-scratch--prepare-buffer region cf-scratch-default-mode))
      (_ (cf-scratch--prepare-buffer region)))))

(provide 'cf-scratch)
;;; cf-scratch.el ends here

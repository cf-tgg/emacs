;;; cf-common-abbrev-table.el --- Common Abbrevs -*- lexical-binding: t; -*-

;; Time-stamp: <2025-07-22 20:38:33 cf>
;; Box: [Linux 6.15.6-zen1-1-zen x86_64 GNU/Linux]

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
;;      Simple abbrevs for what I used to type on a daily basis.

;;; Code:

(require 'ispell)
(require 'abbrev)
(require 's)

;;; Globals

(defmacro cf-abbrev (table &rest definitions)
  "Expand abbrev DEFINITIONS for the given TABLE.
DEFINITIONS is a sequence of (i) string pairs mapping the
abbreviation to its expansion or (ii) a string and symbol pair
making an abbreviation to a function."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  `(if (abbrev-table-p ,table)
       (progn
         ,@(mapcar
            (lambda (pair)
              (let ((abbrev (nth 0 pair))
                    (expansion (nth 1 pair)))
                (if (stringp expansion)
                    `(define-abbrev ,table ,abbrev ,expansion)
                  `(define-abbrev ,table ,abbrev "" ,expansion))))
            (seq-split definitions 2)))
     (error "%s is not an abbrev table" ,table)))

(defun abrv/datetime-insert-dt ()
  (insert (format-time-string "%F %H:%M" (current-time))))

(defun abrv/datetime-insert-dtf ()
  (insert (format-time-string "%F" (current-time))))

(defun abrv/datetime-insert-dtHM ()
  (insert (format-time-string "%H:%M" (current-time))))

(defun abrv/datetime-insert-dtS ()
  (insert (format-time-string "%S" (current-time))))

(defun abrv/datetime-insert-dtY ()
  (insert (format-time-string "%Y" (current-time))))

(defun abrv/datetime-insert-dtH ()
  (insert (format-time-string "%H" (current-time))))

(defun abrv/datetime-insert-dtM ()
  (insert (format-time-string "%M" (current-time))))

(defun abrv/datetime-insert-dtm ()
  (insert (format-time-string "%m" (current-time))))

(defun abrv/datetime-insert-dtd ()
  (insert (format-time-string "%d" (current-time))))

(defun abrv/datetime-insert-dtX ()
  (insert (format-time-string "%X" (current-time))))

(defun abrv/datetime-insert-dtx ()
  (insert (format-time-string "%x" (current-time))))

(defun abrv/datetime-insert-dtZ ()
  (insert (format-time-string "%Z" (current-time))))

(defun abrv/datetime-insert-dtT ()
  (insert (format-time-string "%T" (current-time))))

(defun abrv/datetime-insert-dtA ()
  (insert (format-time-string "%A" (current-time))))

(defun abrv/datetime-insert-dta ()
  (insert (format-time-string "%a" (current-time))))

(define-abbrev global-abbrev-table "dtime" "" 'abrv/datetime-insert-dt)
(define-abbrev global-abbrev-table "dts" "" 'abrv/datetime-insert-dtS)
(define-abbrev global-abbrev-table "dtf" "" 'abrv/datetime-insert-dtf)
(define-abbrev global-abbrev-table "dthm" "" 'abrv/datetime-insert-dtHM)
(define-abbrev global-abbrev-table "dty" "" 'abrv/datetime-insert-dtY)
(define-abbrev global-abbrev-table "dth" "" 'abrv/datetime-insert-dtH)
(define-abbrev global-abbrev-table "dtmin" "" 'abrv/datetime-insert-dtM)
(define-abbrev global-abbrev-table "dtmonth" "" 'abrv/datetime-insert-dtm)
(define-abbrev global-abbrev-table "dtday" "" 'abrv/datetime-insert-dtd)
(define-abbrev global-abbrev-table "dtX" "" 'abrv/datetime-insert-dtX)
(define-abbrev global-abbrev-table "dtx" "" 'abrv/datetime-insert-dtx)
(define-abbrev global-abbrev-table "dtz" "" 'abrv/datetime-insert-dtZ)
(define-abbrev global-abbrev-table "dtt" "" 'abrv/datetime-insert-dtT)
(define-abbrev global-abbrev-table "dtA" "" 'abrv/datetime-insert-dtA)
(define-abbrev global-abbrev-table "dta" "" 'abrv/datetime-insert-dta)

(defun abrv/hls-playlist-header ()
  "HLS playlist header abbrev."
  (insert  "#EXTM3U\n"))
(define-abbrev global-abbrev-table "#m3u" "" 'abrv/hls-playlist-header)

(defvar cf-uname-flags "srmo"
   "Default flags for uname.")

(defun cf-author-header-format (&optional user uname)
  "Format for author header: USER [UNAME]."
  (let* ((user (or user (user-login-name)))
         (uname (or uname (shell-command-to-string (format "uname -%s" cf-uname-flags)))))
    (format "%s [%s]" user (s-trim uname))))

(defun abrv/insert-box-specs ()
  "Expands to insert USER and BOX specific specs."
  (insert (format "%s" (cf-author-header-format))))
(define-abbrev global-abbrev-table "ubox" "" 'abrv/insert-box-specs)

(defun abrv/insert-protonmail ()
  "Inserts protonmail contact."
  (insert  "<cf.gg.tty@protonmail.com>"))
(define-abbrev global-abbrev-table "prmail" "" 'abrv/insert-protonmail)

(defun abrv/insert-github-link ()
  "Inserts cf-tgg Github URL."
  (insert  "<https://github.com/cf-tgg/>"))
(define-abbrev global-abbrev-table "ghb" "" 'abrv/insert-github-link)

(defun abrv/insert-gitlab-link ()
  "Inserts cf-gg Gitlab URL."
  (insert  "<https://gitlab.com/cf-gg/>"))
(define-abbrev global-abbrev-table "glb" "" 'abrv/insert-gitlab-link)

(defun abrv/insert-codeberg-link ()
  "Inserts cfggtty codeberg URL."
  (insert  "<https://codeberg.org/cfggtty/>"))
(define-abbrev global-abbrev-table "cbg" "" 'abrv/insert-codeberg-link)

;;; Org Mode

(defun abrv/insert-org-header ()
  "Insert standard Org mode header if not already present."
  (interactive)
  (unless (save-excursion
            (goto-char (point-min))
            (re-search-forward "^#\\+title:" nil t))
    (goto-char (point-min))
    (insert (format "#+title: %s\n" (file-name-base (or (buffer-file-name) (buffer-name)))))
    (insert (format "#+author: %s\n" (cf-author-header-format)))
    (insert (format "#+date: %s\n" (format-time-string "<%F %H:%M>")))
    (insert "\n")
    (save-buffer)))

(defun abrv/insert-end-src-block ()
  (insert "#+end_src\n\n#+begin_src "))

(with-eval-after-load 'org
  (define-abbrev org-mode-abbrev-table "elhd" "" #'abrv/insert-org-header)
  (define-abbrev org-mode-abbrev-table  "jj" "" #'abrv/insert-end-src-block))

;;; emacs-lisp-mode

(defun abrv/insert-elisp-header ()
  "Insert standard elisp-header."
  (interactive)
  (let* ((file (or (buffer-file-name) "")))
    (insert (format ";;; %s ----  -*- lexical-binding: t; -*-\n\n;;; Commentary:\n;;  \n\n;;; Code:\n\n" file))))
(define-abbrev emacs-lisp-mode-abbrev-table "elhd" "" #'abrv/insert-elisp-header)

(defun abrv/insert-elisp-footer ()
  "Insert standard elisp-footer."
  (interactive)
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (base (file-name-base file))
         (name (file-name-nondirectory file)))
    (insert (format "\f\n;;; _\n(provide '%s)\n;;; %s ends here\n" base name))))
(define-abbrev emacs-lisp-mode-abbrev-table "elft" "" #'abrv/insert-elisp-footer)

(defun abrv/lexical-binding ()
  "Insert lexical binding declaration at point.
Unless line already starts with semi-colon, inserts ';;; ' at `point-min'.
If at end of buffer, inserts a newline before moving to next line."
  (insert "-*- lexical-binding: t; -*-")
  (save-excursion
    (goto-char (point-min))
    (when (looking-at ".*lexical-binding.*")
      (unless (save-excursion
                (beginning-of-line)
                (looking-at-p "[[:space:]]*;"))
        (insert ";;; ")))
    (unless (eolp)
      (move-end-of-line)
      (insert "\n")))
  (forward-line 1))
(define-abbrev emacs-lisp-mode-abbrev-table "lxb" "" 'abrv/lexical-binding)

(defun abrv/insert-commentary-heading ()
  "Insert commentary comment header at point."
  (insert ";;; Commentary:\n\n;; "))
(define-abbrev emacs-lisp-mode-abbrev-table "cmt" "" 'abrv/insert-commentary-heading)

(defun abrv/insert-code-heading ()
  "Insert commentary code header at point."
  (insert ";;; Code:\n\n"))
(define-abbrev emacs-lisp-mode-abbrev-table "cde" "" #'abrv/insert-code-heading)

(defun abrv/insert-emacs-pkg-contact ()
  "Inserts Emacs package contact links."
  (insert ";; Copyright (C) 2025 cf dot gg\n\n;; Author: cf <cf.gg.tty@protonmail.com>\n;; URL: <https://github.com/cf-tgg/>\n;; Gitlab: <https://gitlab.com/cf-gg/>\n;; Codeberg: <https://codeberg.org/cfggtty/>\n;; Version: 0.1.0\n;; Package-Requires: ((emacs \"30.1\"))\n\n;; This file is NOT part of GNU Emacs.\n\n;; This program is free software; you can redistribute it and/or modify\n;; it under the terms of the GNU General Public License as published by\n;; the Free Software Foundation, either version 3 of the License, or\n;; (at your option) any later version.\n;;\n;; This program is distributed in the hope that it will be useful,\n;; but WITHOUT ANY WARRANTY; without even the implied warranty of\n;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n;; GNU General Public License for more details.\n"))
(define-abbrev emacs-lisp-mode-abbrev-table "elpkg" "" #'abrv/insert-emacs-pkg-contact)

;;; sh-mode

(defun abrv/insert-shebang ()
  "Insert standard sh-mode header."
  (insert "#!/bin/sh\n# -*- mode: sh; -*- vim: ft=sh:ts=2:sw=2:norl:et:\n# Time-stamp: <>\n# Box: \n"))

(defun abrv/insert-sh-pkg ()
  "Insert standard sh-mode package header."
  (insert "#  Copyright (C) 2025 cf dot gg\n\n#  Author: cf <cf.gg.tty@protonmail.com>\n#  Github: <https://github.com/cf-tgg/>\n#  Gitlab: <https://gitlab.com/cf-gg/>\n#  Codeberg: <https://codeberg.org/cfggtty/>\n#  Version: 0.1.0\n\n"))

(defun abrv/insert-usage ()
  "Insert a standard POSIX shell usage() function with heredoc formatting."
  (interactive)
  (insert
   (mapconcat
    #'identity
    '("usage() {"
      "    env SC=$0 envsubst <<-'MAN'"
      ""
      "    ${SC} --- <++>"
      ""
      "    NAME"
      "        ${SC}"
      ""
      "    USAGE"
      "        ${SC} [-h] [-v]"
      ""
      "    OPTIONS"
      "        -v    verbose"
      "        -h    help        display this help message"
      ""
      "    SEE ALSO"
      "        <++>"
      ""
      "MAN"
      "}"
      "")
    "\n"))
(delete-char))

(defun abrv/insert-getopts ()
  "Insert a standard POSIX shell usage() function with heredoc formatting."
  (interactive)
  (insert
   (mapconcat
    #'identity
    '("while getopts \"vh\" $OPT ; do"
      "    case $OPT in"
      "        v) VERBOSE=1 ;;"
      "        h) usage >&2 ; exit 1 ;; "
      "    esac"
      "done"
      "shift $((OPTIND-1))"
      "")
    "\n")))

(with-eval-after-load 'sh-mode
  (define-abbrev sh-mode-abbrev-table "shb" "" #'abrv/insert-shebang)
  (define-abbrev sh-mode-abbrev-table "shpkg" "" #'abrv/insert-sh-pkg)
  (define-abbrev sh-mode-abbrev-table "helpfn" "" #'abrv/insert-usage)
  (define-abbrev sh-mode-abbrev-table "gopts" "" #'abrv/insert-getopts))

(defun cf/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

;;;###autoload
(defun cf/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (cf/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (cf/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(define-key ctl-x-map "\C-i" #'cf/ispell-word-then-abbrev)

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

 
;;; _
(provide 'cf-common-abbrev-table)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; auto-fill-mode: nil
;; require-final-newline: t
;; End:
;;; cf-common-abbrev-table.el ends here

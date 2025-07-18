;;; cf-gnus.el --- A gnus config. -*- lexical-binding:t; -*-

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

;;     - [[https://www.gnu.org/software/emacs/manual/html_node/gnus/RSS.html][RSS (Gnus Manual)]]
;;     - [(info "gnus")][]

;;; Code:

(require 'gnus)
(defconst nnrss-description-field "X-Description")
(defconst nnrss-url-field "X-Feed-URL")

(add-to-list 'nnmail-extra-headers nnrss-description-field)
(add-to-list 'nnmail-extra-headers nnrss-url-field)

(add-to-list 'nnmail-extra-headers nnrss-description-field)
(setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-15,15f%]%) %s%uX\n")

(defun gnus-user-format-function-X (header)
  (let ((descr
         (assq nnrss-description-field (mail-header-extra header))))
    (if descr (concat "\n\t" (cdr descr)) "")))

(setq gnus-select-method
      '(nntp "gwene" (nntp-address "news.gwene.org")))

(setq gnus-secondary-select-methods
      '((nntp "feedbase"
              (nntp-open-connection-function nntp-open-tls-stream)
              (nntp-port-number 563)
              (nntp-address "feedbase.org"))
        (nntp "gwene"
              (nntp-address "news.gwene.org"))
        (nntp "yhetil"
              (nntp-address "news.yhetil.org"))
        (nnrss "")))

(defun gnus-user-format-function-X (header)
  (let ((descr
         (assq nnrss-description-field (mail-header-extra header))))
    (if descr (concat "\n\t" (cdr descr)) "")))

(require 'browse-url)

(defun browse-nnrss-url (arg)
  (interactive "p")
  (let ((url (assq nnrss-url-field
                   (mail-header-extra
                    (gnus-data-header
                     (assq (gnus-summary-article-number)
                           gnus-newsgroup-data))))))
    (if url
        (progn
          (browse-url (cdr url))
          (gnus-summary-mark-as-read-forward 1))
      (gnus-summary-scroll-up arg))))

(with-eval-after-load "gnus"
  (define-key gnus-summary-mode-map
    (kbd "<RET>") 'browse-nnrss-url))

(add-to-list 'nnmail-extra-headers nnrss-url-field)

;; Set the default value of mm-discouraged-alternatives.
(with-eval-after-load "gnus-sum"
  (add-to-list
   'gnus-newsgroup-variables
   '(mm-discouraged-alternatives
     . '("text/html" "image/.*"))))

;; Display ‘text/html’ parts in nnrss groups.
(add-to-list
 'gnus-parameters
 '("\\`nnrss:" (mm-discouraged-alternatives nil)))


;;; _
(provide 'cf-gnus)
;; Local Variables:
;; coding: utf-8
;; fill-column: 90
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:
;;; cf-gnus.el ends here

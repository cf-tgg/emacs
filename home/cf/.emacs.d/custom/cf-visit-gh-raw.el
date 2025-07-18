;;; cf-visit-gh-raw.el --- Browse and fetch GitHub raw files from EWW -*- lexical-binding: t; -*-

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
;;    This file aims to circumvent gh heavy js web bloat by providing an
;;  occur-like buffer listing links to all raw file URIs of a GH repository
;;  as clickable buttons which trigger loading a sensible major-mode
;;  (and its custom configuration) for known file types.
;;
;;  Allowing for code browsing of select files, with custom configurations
;;  for copying select files or snippets of code from EWW without the
;;  need to clone the whole repo just yet.
;;
;;  Example use case:
;;
;;   1. Click on `https://raw.githubusercontent.com/fooUser/barRepo/README.md`
;;   2. Have EWW load the raw content of the README using `markdown-mode'.
;;   3. Do whatever I want with the loaded buffer
;;      (most likely read, write, edit and/or dispose of it) and be done with.
;;
;;  Honestly, I only made this to fix browsing GH repos in EWW: I don't
;;  know why most often than not their webpage complains about me needing to
;;  refresh my session to browse readily available files, but that's why.

;;; Code:

(require 'eww)
(require 'regexp-opt)
(require 'thingatpt)
(require 'url)
(require 'cl-lib)
(require 'goto-addr)

(defvar cf-url-suffix-regexp
  (regexp-opt '("md" "org" "el" "go" "cs" "sh" "c" "h" "cpp" "txt" "json" "yml" "yaml" "py" "js" "qml") 'words)
  "Regexp matching known file suffixes for mode inference.")

(defvar cf-url-suffix-mode-alist
  '(("md"   . markdown-mode)
    ("org"  . org-mode)
    ("el"   . emacs-lisp-mode)
    ("go"   . go-mode)
    ("cs"   . csharp-ts-mode)
    ("sh"   . sh-mode)
    ("c"    . simpc-mode)
    ("h"    . simpc-mode)
    ("cpp"  . c++-mode)
    ("txt"  . text-mode)
    ("json" . json-mode)
    ("yml"  . yaml-mode)
    ("yaml" . yaml-mode)
    ("py"   . python-mode)
    ("js"   . js-ts-mode)
    ("qml"  . qml-mode))
  "Alist mapping file suffixes to major modes.")

(defun cf/visit-gh-raw-internal (url)
  "Fetch raw.githubusercontent.com URL, save contents to temp file and open with proper major mode."
  (let ((buf (url-retrieve-synchronously url)))
    (unless buf
      (user-error "Failed to fetch: %s" url))
    (with-current-buffer buf
      (set-buffer-multibyte t)
      (let ((coding-system-for-read 'utf-8-auto)
            (coding-system-for-write 'utf-8-auto))
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((content (buffer-substring-no-properties (point) (point-max)))
               (urlobj (url-generic-parse-url url))
               (raw-path (url-filename urlobj))
               (name (and raw-path (file-name-nondirectory raw-path)))
               (ext (when (and name (string-match "\\.\\([a-zA-Z0-9]+\\)\\'" name))
                      (match-string 1 name)))
               (tmpfile (make-temp-file "eww." nil (concat "." (or name "rawfile")))))
          (kill-buffer buf)
          (with-temp-file tmpfile
            (set-buffer-file-coding-system 'utf-8-auto)
            (insert content))
          (let ((coding-system-for-read 'utf-8-auto))
            (find-file tmpfile)))))))

;;;###autoload
(defun cf/eww-list-gh-raw-files ()
  "List raw.githubusercontent.com file URLs for current GitHub repo or blob in EWW.
Links are displayed as plain URLs with `goto-address-mode` enabled.
Press RET on a URL to fetch and open the raw file with appropriate mode."
  (interactive)
  (unless (derived-mode-p 'eww-mode)
    (user-error "Not in an EWW buffer"))
  (let* ((url (plist-get eww-data :url))
         (repo-url
          (when (string-match "^https://github\\.com/\\([^/]+\\)/\\([^/]+\\)" url)
            (format "https://github.com/%s/%s" (match-string 1 url) (match-string 2 url)))))
    (unless repo-url
      (user-error "Not a recognized GitHub repo or blob URL: %s" url))
    (let ((buf (url-retrieve-synchronously repo-url)))
      (unless buf
        (user-error "Failed to fetch: %s" repo-url))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let ((raw-links '()))
          (while (re-search-forward
                  "href=\"/\\([^/]+\\)/\\([^/]+\\)/blob/\\([^\"]+\\)\"" nil t)
            (let* ((user (match-string 1))
                   (repo (match-string 2))
                   (path (match-string 3))
                   (raw-url (format "https://raw.githubusercontent.com/%s/%s/%s"
                                    user repo path)))
              (push raw-url raw-links)))
          (kill-buffer buf)
          (setq raw-links (delete-dups raw-links))
          (if (null raw-links)
              (message "No raw file links found.")
            (let ((out-buf (get-buffer-create "*github-raw-files*")))
              (with-current-buffer out-buf
                (read-only-mode -1)
                (erase-buffer)
                (dolist (link raw-links)
                  (when (stringp link)
                    (insert link "\n")))
                (goto-char (point-min))
                (read-only-mode 1)
                (goto-address-mode 1)
                (keymap-local-set
                 "RET"
                 #'cf/visit-gh-raw-url))
              (pop-to-buffer out-buf))))))))

(defun cf/visit-gh-raw-url ()
  "Fetch and open raw.githubusercontent.com URL at point."
  (interactive)
  (let ((url (thing-at-point 'url t)))
    (unless (and url (string-match-p "^https://raw\\.githubusercontent\\.com/" url))
      (user-error "No valid raw.githubusercontent.com URL at point"))
    (cf/visit-gh-raw-internal url)))

(provide 'cf-visit-gh-raw)
;;; cf-visit-gh-raw.el ends here

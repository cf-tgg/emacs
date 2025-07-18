;;; cf-mark-url-and-save.el --- Local HTML with baseenc64 embedded images -*- lexical-binding: t; -*-

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

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(require 'url)
(require 'url-parse)
(require 'base64)
(require 'cl-lib)

(defvar cf-eww-html-directory "~/.local/eww/"
  "Base directory for saving HTML pages with embedded images.")

(defun cf/sanitize-file-name (name)
  (replace-regexp-in-string "[^[:alnum:]_.-]" "_" name))

(defun cf/resolve-url (relative base)
  (url-expand-file-name relative base))

(defun cf/escape-replacement (string)
  "Escape \\ and & in STRING for safe use in `replace-regexp-in-string' replacement."
  (replace-regexp-in-string "[\\&]" (lambda (m) (concat "\\" m)) string t t))

(defun cf/base64-encode-url (url callback)
  (url-retrieve
   url
   (lambda (_status)
     (goto-char (point-min))
     (re-search-forward "\r?\n\r?\n")
     (let* ((headers (buffer-substring (point-min) (point)))
            (data (buffer-substring-no-properties (point) (point-max)))
            (mime-type (or (when (string-match "Content-Type: \\([^;\n]+\\)" headers)
                             (match-string 1 headers))
                           "application/octet-stream"))
            (b64 (base64-encode-string data t)))
       (funcall callback (format "data:%s;base64,%s" mime-type b64))))
   nil t))

(defun cf/make-unique-file-name (filename directory)
  "Generate a unique file name in DIRECTORY based on FILENAME."
  (let ((full-path (expand-file-name filename directory))
        (base (file-name-sans-extension filename))
        (ext (file-name-extension filename t))
        (i 1))
    (while (file-exists-p full-path)
      (setq full-path (expand-file-name (format "%s_%d%s" base i ext) directory))
      (setq i (1+ i)))
    full-path))

(defun cf/cleanup-html-tags (html)
  "Remove residual or invalid HTML constructs in HTML string."
  (let ((clean html))
    (setq clean (replace-regexp-in-string
                 "srcset=[\"'][^\"']*[\"']"
                 ""
                 clean t t))
    (setq clean (replace-regexp-in-string
                 "<img\\([^>]*\\)?>"
                 (lambda (img)
                   (if (string-match "src=[\"']data:[^\"']+[\"']" img)
                       img
                     ""))
                 clean))
    (setq clean (replace-regexp-in-string
                 "<\\([^>]+\\)>"
                 (lambda (tag)
                   (replace-regexp-in-string "[ \t]+" " " tag t t))
                 clean))
    (setq clean (replace-regexp-in-string "<!--.*?-->" "" clean t))
    clean))

(defun cf/inline-images-in-html (html base-url callback)
  "Inline all image src and srcset URLs as base64 data URIs in HTML."
  (let ((replacements '())
        (pending 0))
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (while (re-search-forward "<img\\([^>]+\\)>" nil t)
        (let* ((img-tag (match-string 0))
               (start (match-beginning 0))
               (end (match-end 0))
               (attrs img-tag)
               (src (when (string-match "src=[\"']\\([^\"']+\\)[\"']" attrs)
                      (match-string 1 attrs)))
               (srcset (when (string-match "srcset=[\"']\\([^\"']+\\)[\"']" attrs)
                         (match-string 1 attrs)))
               (srcset-urls (when srcset (split-string srcset "[ ,]+" t)))
               (url (or src (car (last srcset-urls)))))
          (when url
            (let ((resolved (cf/resolve-url url base-url)))
              (cl-incf pending)
              (cf/base64-encode-url
               resolved
               (lambda (data-uri)
                 (let* ((escaped-src (cf/escape-replacement (format "src=\"%s\"" data-uri)))
                        (new-tag (replace-regexp-in-string
                                  "srcset=[\"'][^\"']+[\"']" ""
                                  (replace-regexp-in-string
                                   "src=[\"'][^\"']+[\"']"
                                   escaped-src
                                   img-tag t t)
                                  t t)))
                   (push (list start end new-tag) replacements))
                 (cl-decf pending)
                 (when (zerop pending)
                   (let ((output html))
                     (setq replacements (sort replacements (lambda (a b) (> (car a) (car b)))))
                     (dolist (r replacements)
                       (let ((s (nth 0 r))
                             (e (nth 1 r))
                             (rep (nth 2 r)))
                         (setq output (concat (substring output 0 s) rep (substring output e)))))
                     (funcall callback (cf/cleanup-html-tags output))))))))))
      (when (zerop pending)
        (funcall callback (cf/cleanup-html-tags html))))))

(defun cf/download-url-to-file (url directory filename)
  "Download HTML from URL, embed images, and save to DIRECTORY/FILENAME."
  (let ((dir (expand-file-name directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (url-retrieve
     url
     (lambda (status)
       (unless (plist-get status :error)
         (goto-char (point-min))
         (re-search-forward "\r?\n\r?\n")
         (let ((html (buffer-substring-no-properties (point) (point-max))))
           (cf/inline-images-in-html
            html url
            (lambda (final-html)
              (let ((final-path (cf/make-unique-file-name filename dir)))
                (with-temp-buffer
                  (insert final-html)
                  (let ((coding-system-for-write 'utf-8))
                    (write-region (point-min) (point-max) final-path)))
                (message "Saved self-contained HTML: %s" final-path)))))))
     nil t)))

(defun cf/mark-url-and-save (&optional bookmark)
  "Copy URL at point, bookmark optionally, and save HTML with embedded images."
  (interactive "P")
  (let ((bounds (bounds-of-thing-at-point 'url)))
    (if bounds
        (let* ((url (buffer-substring-no-properties (car bounds) (cdr bounds)))
               (parts (url-generic-parse-url url))
               (domain (or (url-host parts) "unknown"))
               (title (file-name-nondirectory (directory-file-name (url-filename parts))))
               (safe-title (if (string-empty-p title) "index" title)))
          (kill-new url)
          (set-mark (car bounds))
          (goto-char (cdr bounds))
          (when bookmark
            (eww-add-bookmark safe-title url))
          (message "Saving: %s" url)
          (cf/download-url-to-file
           url
           (expand-file-name domain cf-eww-html-directory)
           (format "%s.html" (cf/sanitize-file-name safe-title))))
      (message "No URL at point."))))

(defun cf/ensure-html-extension (filename)
  "Ensure FILENAME ends with .html (case-insensitive), else append it."
  (if (string-match-p "\\.html?\\'" filename)
      filename
    (concat filename ".html")))

(defun cf/save-eww-page ()
  "Save current EWW page with embedded images."
  (interactive)
  (if (eq major-mode 'eww-mode)
      (let* ((url (plist-get eww-data :url))
             (parts (url-generic-parse-url url))
             (domain (or (url-host parts) "unknown"))
             (title (plist-get eww-data :title))
             (safe-title (cf/sanitize-file-name (or title "index"))))
        (message "Saving current EWW page: %s" url)
        (cf/download-url-to-file
         url
         (expand-file-name domain cf-eww-html-directory)
         (cf/ensure-html-extension safe-title)))
    (user-error "Not in EWW buffer")))

(defun cf/url-at-point-no-button ()
  "Return the URL at point, excluding buttons and overlays."
  (let ((inhibit-point-motion-hooks t)
        (button (button-at (point))))
    (if button
        nil
      (thing-at-point 'url t))))

(defun cf/alternate-mark-url-and-save (&optional bookmark)
  "Like `cf/mark-url-and-save' but ignores button overlays to get raw URL at point."
  (interactive "P")
  (let ((url (cf/url-at-point-no-button)))
    (if url
        (let* ((parts (url-generic-parse-url url))
               (domain (or (url-host parts) "unknown"))
               (title (file-name-nondirectory (directory-file-name (url-filename parts))))
               (safe-title (if (string-empty-p title) "index" title)))
          (kill-new url)
          (when bookmark
            (eww-add-bookmark safe-title url))
          (message "Saving (alternate): %s" url)
          (cf/download-url-to-file
           url
           (expand-file-name domain cf-eww-html-directory)
           (cf/ensure-html-extension (cf/sanitize-file-name safe-title))))
      (message "No usable URL at point."))))

(defun cf/mark-url-and-save-dwim (&optional bookmark)
  "Save embedded-image HTML from a URL at point or the current EWW page.
1. Try `cf/url-at-point-no-button'.
2. If nil, try `thing-at-point' on 'url.
3. Else fallback to current EWW buffer.
BOOKMARK non-nil triggers bookmarking."
  (interactive "P")
  (let ((url (cf/url-at-point-no-button)))
    (cond
     (url
      (let* ((parts (url-generic-parse-url url))
             (domain (or (url-host parts) "unknown"))
             (title (file-name-nondirectory (directory-file-name (url-filename parts))))
             (safe-title (if (string-empty-p title) "index" title)))
        (kill-new url)
        (when bookmark
          (eww-add-bookmark safe-title url))
        (message "Saving (from raw url): %s" url)
        (cf/download-url-to-file
         url
         (expand-file-name domain cf-eww-html-directory)
         (cf/ensure-html-extension (cf/sanitize-file-name safe-title)))))
     ((let ((url (thing-at-point 'url t)))
        (when url
          (let* ((parts (url-generic-parse-url url))
                 (domain (or (url-host parts) "unknown"))
                 (title (file-name-nondirectory (directory-file-name (url-filename parts))))
                 (safe-title (if (string-empty-p title) "index" title)))
            (kill-new url)
            (when bookmark
              (eww-add-bookmark safe-title url))
            (message "Saving (from thing-at-point): %s" url)
            (cf/download-url-to-file
             url
             (expand-file-name domain cf-eww-html-directory)
             (cf/ensure-html-extension (cf/sanitize-file-name safe-title)))
            t))))
     ((eq major-mode 'eww-mode)
      (cf/save-eww-page))
     (t
      (message "No valid URL found and not in EWW buffer.")))))

;; Keybindings
(keymap-global-set "C-*" #'cf/mark-url-and-save-dwim)
(keymap-global-set "C-M-*" #'cf/mark-url-and-save)
(keymap-global-set "C-M-~" #'cf/alternate-mark-url-and-save)
(keymap-global-set "C-M-#" #'cf/save-eww-page)


;;; _
(provide 'cf-mark-url-and-save)
;;; cf-mark-url-and-save.el ends here

;;; cf-elfeed-fame.el --- Detached ElFeed Frame. -*- lexical-binding: t; -*-

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
;;
;;  Should also configure eww and html renderer from same customized
;;  user environment:
;;            \[(start-process-shell-command
;;                   "emacs --init-directory ~/.config/elfeed/")]

;;; Code:

;;;###autoload
(defun cf/browse-url-mpv (url &rest _args)
  "Open the given URL in mpv."
  (interactive)
  (start-process-shell-command "mpv" nil (format "mpv -quiet %s" url)))

(setq browse-url-handlers
      '(("https://www\\.youtube\\.com/watch\\?v=.*" . cf/browse-url-mpv)
        ("https://www\\.youtube\\.com/playlist\\?list=.*" . cf/browse-url-mpv)))
(setq browse-url-browser-function
      (lambda (url &rest args)
        (if (or (string-match-p "^https://www\\.youtube\\.com/watch\\?v=" url)
          (string-match-p "^https://www\\.youtube\\.com/playlist\\?list=" url))
            (cf/browse-url-mpv url)
          (apply 'eww url args))))

;; EwwSurfraw
(defvar cf-sr-default-elvi
  '((1 . "duckduckgo")
    (2 . "W")
    (3 . "S")
    (4 . "archwiki")
    (5 . "searx")
    (6 . "aur")
    (7 . "youtube")
    (8 . "wikipedia")
    (9 . "github"))
  "Alist mapping numeric prefix arguments to Surfraw elvi engines.")

;;;###autoload
(defun cf/sr-elvi-search (&optional elvi query)
  "Search with Surfraw elvi in EWW.
If REGION is active, use the selected text as the query.
If called with a numeric prefix argument (C-u 1, C-u 2, etc.),
use the corresponding default elvi from `cf/sr-default-elvi`.
If a prefix argument (C-u) is provided, use `S` as the default elvi.
If no prefix argument is given, prompt for elvi choice with completion."
  (interactive
   (list
    (if current-prefix-arg
        (let ((num-arg (prefix-numeric-value current-prefix-arg)))
          (or (alist-get num-arg cf-sr-default-elvi) "S"))
      (let* ((elvi-raw (shell-command-to-string "surfraw -elvi"))
             (elvi-list (split-string elvi-raw "\n" t))
             (selection (completing-read "[sr]: " elvi-list)))
        (car (split-string selection "[ \t]+"))))
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Query: "))))
  (let* ((url (string-trim (shell-command-to-string (format "surfraw -p %s '%s'" elvi query)))))
    (if (not (string-empty-p url))
        (eww url)
      (message "Failed to retrieve URL."))))
(keymap-global-set "C-c s" #'cf/sr-elvi-search)

;; Shr html renderer
(use-package shr
  :ensure nil
  :custom
  (shr-use-fonts nil))

(use-package shr-color
  :ensure nil
  :custom
  (shr-color-visible-luminance-min 1)
  (shr-color-visible-luminance-min 90))

;; Shr support for code blocks
(use-package shr-tag-pre-highlight
  :ensure nil
  :after shr
  :config
  (require 'shr-tag-pre-highlight)
  (require 'language-detection)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

;; ShrFace
(use-package shrface
  :ensure nil
  :after shr
  :config
  (setq shr-cookie-policy nil)
  (setq shrface-bullets-bullet-list
   '("▼" "▽" "▿" "▾"))
  (require 'eww)
  (require 'shrface)
  (require 'shr-tag-pre-highlight)
  (require 'cl-lib)

  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let ((mode (eww-buffer-auto-detect-mode)))
        (when mode
          (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region
     (point-min) (point-max) nil))

  (defun eww-buffer-auto-detect-mode ()
    (let* ((map '((ada ada-mode)
                  (awk awk-mode)
                  (c c-mode)
                  (cpp c++-mode)
                  (clojure clojure-mode lisp-mode)
                  (csharp csharp-mode java-mode)
                  (css css-mode)
                  (dart dart-mode)
                  (delphi delphi-mode)
                  (emacslisp emacs-lisp-mode)
                  (erlang erlang-mode)
                  (fortran fortran-mode)
                  (fsharp fsharp-mode)
                  (go go-mode)
                  (groovy groovy-mode)
                  (haskell haskell-mode)
                  (html html-mode)
                  (java java-mode)
                  (javascript javascript-mode)
                  (json json-mode javascript-mode)
                  (latex latex-mode)
                  (lisp lisp-mode)
                  (lua lua-mode)
                  (matlab matlab-mode octave-mode)
                  (objc objc-mode c-mode)
                  (perl perl-mode)
                  (php php-mode)
                  (prolog prolog-mode)
                  (python python-mode)
                  (r r-mode)
                  (ruby ruby-mode)
                  (rust rust-mode)
                  (scala scala-mode)
                  (shell shell-script-mode)
                  (smalltalk smalltalk-mode)
                  (sql sql-mode)
                  (swift swift-mode)
                  (visualbasic visual-basic-mode)
                  (xml sgml-mode)))
           (language (language-detection-string
                      (buffer-substring-no-properties (point-min) (point-max))))
           (modes (cdr (assoc language map)))
           (mode (cl-loop for mode in modes
                          when (fboundp mode)
                          return mode)))
      (message (format "%s" language))
      (when (fboundp mode)
        mode)))

  (add-to-list 'shr-external-rendering-functions
               '((pre . eww-tag-pre)))

  (defun eww-link-buttonize ()
    "Ensure that links in EWW buffers are properly buttonized."
    (interactive)
    (setq shr-external-rendering-functions
          (append '((a . shr-tag-a)) shr-external-rendering-functions)))

  (add-hook 'eww-after-render-hook #'eww-link-buttonize)

  (with-eval-after-load 'eww
    (keymap-set eww-mode-map "<tab>" #'forward-button)
    (keymap-set eww-mode-map "C-<tab>" #'shrface-outline-cycle)
    (keymap-set eww-mode-map "C-<backtab>" #'shrface-outline-cycle-buffer)
    (keymap-set eww-mode-map "C-t" #'shrface-toggle-bullets)
    (keymap-set eww-mode-map ">" #'shrface-next-headline)
    (keymap-set eww-mode-map "<" #'shrface-previous-headline)
    (keymap-set eww-mode-map "L" #'shrface-links-counsel)
    (keymap-set eww-mode-map "H" #'shrface-headline-counsel))

  (with-eval-after-load 'mu4e
    (defun shrface-mu4e-advice (orig-fun &rest args)
      (require 'eww)
      (require 'shr-tag-pre-highlight)
      (let ((shrface-org nil)
            (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
            (shr-table-vertical-line "|")
            (shr-width 90)
            (shr-indentation 3)
            (shr-external-rendering-functions
             (append '((title . eww-tag-title)
                       (form . eww-tag-form)
                       (input . eww-tag-input)
                       (button . eww-form-submit)
                       (textarea . eww-tag-textarea)
                       (select . eww-tag-select)
                       (link . eww-tag-link)
                       (meta . eww-tag-meta)
                       (pre . eww-tag-pre)
                       (a . shr-tag-a))
                     shrface-supported-faces-alist))
            (shrface-toggle-bullets nil)
            (shrface-href-versatile t)
            (shr-use-fonts nil))
        (apply orig-fun args)))
    (advice-add 'mu4e-shr2text :around #'shrface-mu4e-advice)
    (keymap-set mu4e-view-mode-map "o" #'shrface-outline-cycle)
    (keymap-set mu4e-view-mode-map "O" #'shrface-outline-cycle-buffer)
    (keymap-set mu4e-view-mode-map "<" #'shrface-previous-headline)
    (keymap-set mu4e-view-mode-map ">" #'shrface-next-headline)
    (keymap-set mu4e-view-mode-map "C-t" #'shrface-toggle-bullets)
    (keymap-set mu4e-view-mode-map "C-l" #'shrface-links-counsel)
    (keymap-set mu4e-view-mode-map "C-c h" #'shrface-headline-counsel)))

;; EWW Emacs Web Wowser
(use-package eww
  :ensure nil
  :custom
  (setq mm-text-html-renderer-alist)
  :config
  (require 'eww)
  (require 'shrface)
  (require 'shr-tag-pre-highlight)
  (defun cf-eww-init-hooks ()
    "Initial eww settings."
    (whitespace-mode -1)
    (display-line-numbers-mode -1)
    (shrface-mode 1)
    (auto-fill-mode 1)
    (fit-window-to-buffer))
  (add-hook 'eww-mode-hook #'cf-eww-init-hooks))

(use-package elfeed
  :ensure t
  :commands (elfeed-update elfeed)
  :custom
  (elfeed-search-feed-face ":foreground #f3f3f3 :weight bold")
  (elfeed-feeds '(("https://www.reddit.com/r/linux.rss" reddit linux)
                  ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                  ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                  ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                  ("https://www.quebec.ca/fil-de-presse.rss" quebec presseqc)
                  ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                  ("https://hackaday.com/blog/feed/" hackaday linux)
                  ("https://opensource.com/feed" opensource linux)
                  ("https://linux.softpedia.com/backend.xml" softpedia linux)
                  ("https://itsfoss.com/feed/" itsfoss linux)
                  ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                  ("https://www.phoronix.com/rss.php" phoronix linux)
                  ("http://feeds.feedburner.com/d0od" omgubuntu linux)
                  ("https://www.computerworld.com/index.rss" computerworld linux)
                  ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                  ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                  ("https://betanews.com/feed" betanews linux)
                  ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
                  ("https://distrowatch.com/news/dwd.xml" distrowatch linux)))
  :config
  (use-package elfeed-goodies
    :ensure t
    :after elfeed
    :custom
    (elfeed-goodies/entry-pane-size 0.7)
    :commands (elfeed elfeed-update)
    :init (elfeed-goodies/setup)
    :bind
    (:map elfeed-search-mode-map
          ("l" . elfeed-search-show-entry)
          ("<return>" . elfeed-search-show-entry)
          ("T" . elfeed-search-tag-all)
          ("t" . elfeed-search-untag-all)
          ("U" . elfeed-update)
          ("<" . elfeed-search-first-entry)
          (">" . elfeed-search-last-entry)
          ("F" . elfeed-search-set-filter)
          ("o" . elfeed-search-browse-url)
          ("c" . elfeed-search-clear-filter)
          ("q" . elfeed-search-quit-window)
          ("S" . elfeed-search-fetch)
          ("R" . elfeed-search-update--force)
          ("f" . elfeed-search-live-filter)
          ("a" . elfeed-search-tag-all-unread)
          ("A" . elfeed-search-untag-all-unread)
          ("y" . elfeed-search-yank)
          ("N" . elfeed-goodies/split-show-next)
          ("P" . elfeed-goodies/split-show-prev)
          ("d" . cf/scroll-up-and-recenter)
          ("u" . cf/scroll-down-and-recenter))
    :config
    (elfeed-update)))

;;;###autoload
(defun cf/elfeed-frame ()
  (interactive)
  (let ((frame (make-frame '((name . "elfeed-frame")
                             (minibuffer . t)
                             (width . 100)
                             (height . 40)))))
    (select-frame-set-input-focus frame)
    (elfeed)
    (delete-other-windows)))



;;; _
(provide 'cf-elfeed-frame)
;;; cf-elfeed-frame.el ends here

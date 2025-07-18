;;; custom.el --- User custom-file for Emacs automatic customizations. -*- lexical-binding:t -*-

;;; Commentary:
;;     Tell Emacs to write autoset configs here.
;;     cf. Info Nodes:
;;        - (info "(emacs)Easy Customization")
;;        - (info "(emacs)use-package")

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode 'filter)
 '(ansi-color-names-vector
   ["#000000" "#c73c3f" "#73c936" "#ffdd33" "#95a99f" "#9e95c7" "#96a6c8"
    "gray90"])
 '(bookmark-sort-flag nil)
 '(comment-column 42)
 '(comment-inline-offset 2)
 '(comment-padding 2)
 '(completion-pcm-leading-wildcard t)
 '(custom-safe-themes
   '("90cfee0fb29a5f9a9a22f06c7c3339058c1aa03225626426207ad1c86d67b11f"
     default))
 '(dictionary-coding-systems-for-dictionaries '(("mueller" . koi8-r) ("*" . utf-8-auto)))
 '(dictionary-read-word-prompt "littré-toé:")
 '(display-line-numbers-current-absolute t)
 '(eww-default-download-directory "~/.local/eww/")
 '(eww-form-checkbox-selected-symbol "[X]")
 '(eww-restore-desktop t)
 '(frame-background-mode 'dark)
 '(global-org-modern-mode t)
 '(gnus-max-image-proportion 0.8)
 '(gnus-single-article-buffer t)
 '(gnus-treat-body-boundary 'head)
 '(gnus-treat-emojize-symbols t)
 '(gnus-treat-strip-leading-blank-lines t)
 '(gnus-treat-strip-multiple-blank-lines t)
 '(gnus-treat-strip-trailing-blank-lines t)
 '(image-crop-cut-command
   '("magick" "-draw" "rectangle %l,%t %r,%b" "-fill" "%c" "-" "%f:-"))
 '(mc/max-cursors nil)
 '(mm-inline-large-images-proportion 0.9)
 '(mm-text-html-renderer 'gnus-w3m)
 '(package-selected-packages
   '(2048-game academic-phrases ace-mc all-the-icons-ivy-rich android-env
               auto-complete-exuberant-ctags awk-ts-mode
               awk-yasnippets browse-at-remote cider clojure-mode
               company consult counsel diminish dired-git-info
               dired-open dired-preview dired-subtree drag-stuff
               eldoc-box elfeed elfeed-goodies elpher embark emms
               eradio eshell-syntax-highlighting eshell-toggle
               exec-path-from-shell expreg flycheck go-mode
               highlight-numbers htmlize htmlize-mode hydra hyperdrive
               imenu-list ivy-prescient keycast ledger-mode lua-mode
               magit marginalia markdown-mode mct memoize mew
               multiple-cursors nano-mu4e nerd-icons-completion
               nerd-icons-dired nerd-icons-ibuffer nerd-icons-ivy-rich
               ob-async orderless ox-rfc paredit pdf-tools php-mode
               projectile rainbow-delimiters rainbow-mode rfc-mode
               scratch sed-mode shr-tag-pre-highlight shrface
               sudo-edit taxy taxy-magit-section tldr toc-org
               treesit-auto vertico vterm vterm-toggle
               yasnippet-snippets))
 '(pdf-annot-activate-created-annotations t)
 '(pdf-view-use-imagemagick t)
 '(safe-local-variable-values
   '((eval keymap-local-set "C-c C-c" #'compile) (rainbow-mode . t)
     (dired-yt-mode . t)
     (eval local-set-key (kbd "RET") #'cf/dired-linkhandler)
     (auto-fill-mode) (fill-column) (Fill-Column . 90)
     (whitespace-mode) (code . utf-8-unix)
     (eval outline-hide-sublevel: 2) (code . utf8-emacs-unix)
     (eval outline-hide-sublevels 1)
     (rainbow-identifiers-cie-l*a*b*-lightness . 70)
     (eval when (fboundp 'rainbow-mode) (rainbow-mode 1))
     (eval keymap-local-set "C-c o" #'counsel-outline)))
 '(server-window 'pop-to-buffer)
 '(sh-indent-comment t)
 '(shell-command-with-editor-mode t)
 '(shr-bullet "– ")
 '(shr-color-visible-distance-min 1)
 '(shr-cookie-policy nil)
 '(shr-discard-aria-hidden t)
 '(shr-image-zoom-levels '(fit original image fill-width fill-height))
 '(shr-max-image-proportion 0.7)
 '(shr-max-inline-image-size '(0.9 . 0.9))
 '(shr-max-width 140)
 '(shr-sliced-image-height 0.8)
 '(shr-use-colors nil)
 '(tldr-use-word-at-point t)
 '(warning-suppress-log-types '((defvaralias losing-value woman-topic-history)))
 '(warning-suppress-types '((native-compiler)))
 '(woman-default-indent 5)
 '(woman-emulation 'nroff)
 '(woman-fill-frame nil)
 '(woman-use-topic-at-point-default t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t (:inherit org-code))))
 '(ansi-color-blue ((t (:background "#96a6c8" :foreground "#96a6c8"))))
 '(ansi-color-bright-blue ((t (:background "steel blue" :foreground "steel blue"))))
 '(ansi-color-bright-cyan ((t (:background "SteelBlue1" :foreground "SteelBlue1"))))
 '(ansi-color-bright-green ((t (:background "#73c936" :foreground "#73c936"))))
 '(ansi-color-bright-magenta ((t (:background "#9e95c7" :foreground "#9e95c7"))))
 '(ansi-color-bright-red ((t (:background "#ff4f58" :foreground "#ff4f58"))))
 '(ansi-color-bright-white ((t (:background "#f4f4f4" :foreground "#f4f4f4"))))
 '(ansi-color-bright-yellow ((t (:background "#ffdd33" :foreground "#ffdd33"))))
 '(ansi-color-cyan ((t (:background "#96a6c8" :foreground "#96a6c8"))))
 '(ansi-color-green ((t (:background "#73c936" :foreground "#73c936"))))
 '(ansi-color-magenta ((t (:background "#9e95c7" :foreground "#9e95c7"))))
 '(ansi-color-red ((t (:background "#f43841" :foreground "#f43841"))))
 '(ansi-color-yellow ((t (:background "#ffdd33" :foreground "#ffdd33"))))
 '(custom-button ((t (:background "#121212" :foreground "#f2f2f2" :box (:line-width (2 . 2) :style released-button)))))
 '(custom-button-mouse ((t (:background "#191919" :foreground "#f8f8f8" :box (:line-width (2 . 2) :style released-button)))))
 '(custom-button-pressed ((t (:background "#282828" :foreground "#000000" :box (:line-width (2 . 2) :style pressed-button)))))
 '(custom-themed ((t (:foreground "#565f73"))))
 '(dictionary-reference-face ((t (:foreground "#95a99f" :slant italic))))
 '(dictionary-word-definition-face ((t (:family "Libertinus Serif"))))
 '(dired-symlink ((t (:foreground "#e2e2e2" :weight bold :inherit default :extend t :slant italic))))
 '(eww-form-checkbox ((t (:background "#202020" :foreground "#9cf33c" :box (:line-width (2 . 2) :style flat-button)))))
 '(eww-form-file ((t (:background "#242424" :foreground "white" :box (:line-width (2 . 2) :style released-button)))))
 '(eww-form-select ((t (:background "#121212" :foreground "white" :box (:line-width (2 . 2) :style released-button)))))
 '(eww-form-submit ((t (:background "#242424" :foreground "#efefef" :box (:line-width (2 . 2) :style released-button)))))
 '(eww-form-text ((t (:background "#181818" :foreground "#e4e4fe" :box (1 . 1)))))
 '(eww-form-textarea ((t (:background "#161616" :foreground "#e3e3ef" :box (1 . 1)))))
 '(eww-valid-certificate ((t (:foreground "dim gray" :weight bold))))
 '(fixed-pitch-serif ((t (:foreground "#f2f2f2" :family "CaskaydiaCove"))))
 '(gnus-header-content ((t (:inherit gnus-header :foreground "gray" :slant italic))))
 '(gnus-header-from ((t (:inherit gnus-header :foreground "#323232"))))
 '(gnus-header-name ((t (:inherit gnus-header :foreground "#888888"))))
 '(gnus-header-newsgroups ((t (:inherit gnus-header :foreground "#ffdd33" :slant italic))))
 '(gnus-header-subject ((t (:inherit gnus-header :foreground "white"))))
 '(gnus-x-face ((t (:foreground "white smoke"))))
 '(header-line ((t (:inherit mode-line :background "#181818" :foreground "grey90" :box nil))))
 '(icon-button ((t (:inherit icon :background "#282828" :foreground "#f4f4f4" :box (:line-width (3 . -2) :color "#404040" :style released-button)))))
 '(info-header-node ((t (:inherit info-node :weight normal))))
 '(info-menu-header ((t (:inherit variable-pitch :distant-foreground "black" :foreground "#efe4e3" :weight bold :height 0.8))))
 '(keycast-key ((t (:inherit fixed-pitch :background "#181818" :foreground "#696969" :box (:line-width (-1 . -1) :color "black" :style flat-button) :weight bold))))
 '(lazy-highlight ((t (:background "#484848" :foreground "#e4e4ef" :weight bold))))
 '(mc/cursor-bar-face ((t (:background "#ffffff" :height 1))))
 '(mm-uu-extract ((t (:extend t :foreground "steel blue"))))
 '(mode-line-highlight ((t (:box (:line-width (1 . 1) :color "#202020" :style released-button)))))
 '(mouse-drag-and-drop-region ((t (:inherit region :background "#484848" :distant-foreground "black" :foreground "gainsboro"))))
 '(newline-mark ((t (:inherit region))))
 '(org-column-title ((t (:underline t :weight bold))))
 '(org-ellipsis ((t (:foreground "#202020"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.4))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.3))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-6 ((t (:inherit outline-5 :extend nil :foreground "#565f73" :height 1.1))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :extend nil))))
 '(org-table ((t (:foreground "#565f73"))))
 '(org-verbatim ((t (:inherit shadow :foreground "gray" :slant italic))))
 '(persp-selected-face ((t (:foreground "#484848" :slant oblique :weight ultra-light))))
 '(pulsar-red ((t (:extend t :background "#342424"))))
 '(region ((t (:extend t :background "#323232"))))
 '(shr-code ((t (:inherit fixed-pitch :family "IosevkaTerm Nerd Font Mono"))))
 '(shr-h1 ((t (:weight bold :height 1.3 :family "Aporetic Serif"))))
 '(shr-link ((t (nil nil "#f8f8f8" :inherit :italic :foreground))))
 '(shr-mark ((t (:background "#ffdd33" :foreground "black"))))
 '(shrface-h6-face ((t (:inherit org-level-6))))
 '(shrface-href-file-face ((t (:inherit org-link :foreground "#565f73"))))
 '(tab-line ((t (:inherit mode-line :background "#181818" :foreground "#484848" :height 0.9))))
 '(tldr-code-block ((t (:background "#181818" :foreground "#eee"))))
 '(tldr-command-argument ((t (:background "#181818" :foreground "#eee" :underline t))))
 '(tldr-command-itself ((t (:background "#181818" :foreground "#f4f4f4" :weight bold))))
 '(tldr-description ((t (:foreground "#cfcfcf" :foundry "variable" :family "Iosevka Nerd Font"))))
 '(tldr-title ((t (:foreground "#cb9c3c" :weight bold :height 1.3))))
 '(tooltip ((t (:background "#52494e" :foreground "#f6f6fe" :height 1.5))))
 '(whitespace-empty ((t (:extend t :background "#202020" :foreground "#202020"))))
 '(whitespace-newline ((t (:foreground "#453d41"))))
 '(window-divider ((t (:foreground "#282828" :background "#282828"))))
 '(window-divider-first-pixel ((t (:background "#282828"))))
 '(window-divider-last-pixel ((t (:background "#282828")))))


;;; _
(provide 'custom)
;;; custom.el ends here.

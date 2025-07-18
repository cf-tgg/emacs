;;; cf-modeline.el --- Custom mode-line configuration with toggleable visibility -*- lexical-binding: t; -*-

;;; Commentary:
;;    Custom mode-line-format for my dotemacs.

;;; Code:

(defgroup cf-mode-line nil
  "Custom mode-line and header-line theming with toggle controls."
  :group 'faces
  :prefix "cf-mode-line-")

(defface cf-modeline-active
  '((t :foreground "#484848" :background "#282828" :family "Iosevka Nerd Font Mono"))
  "Face for active mode-line."
  :group 'cf-mode-line)

(defface cf-modeline-inactive
  '((t :foreground "#484848" :background "#282828" :family "Iosevka Nerd Font Mono"))
  "Face for inactive mode-line."
  :group 'cf-mode-line)

(defface cf-modeline-buffer-readonly
  '((t :foreground "#ac2c1d"))
  "Face for readonly buffer indicator."
  :group 'cf-mode-line)

(defface cf-modeline-buffer-modified
  '((t :foreground "#cb9c3c"))
  "Face for modified buffer indicator."
  :group 'cf-mode-line)

(defface cf-modeline-buffer-saved
  '((t :foreground "#484848"))
  "Face for unmodified buffer indicator."
  :group 'cf-mode-line)

(defface cf-modeline-position
  '((t :foreground "#594949"))
  "Face for line/column display."
  :group 'cf-mode-line)

(defface cf-modeline-time
  '((t :foreground "#696969"))
  "Face for clock display."
  :group 'cf-mode-line)

(defvar-local cf--mode-line-remap-cookie nil)
(defvar-local cf--header-line-remap-cookie nil)

(defvar-local cf--mode-line-visible t
  "Local cf-mode-line-visible flag")

(defvar-local cf--header-line-visible t
  "Local cf-header-line-visible flag")

;;;###autoload
(defun cf--default-mode-line-format ()
  "Apply default custom `mode-line-format'."
  (setq-default blink-cursor-blinks 2
                window-divider-default-places 'bottom-only
                window-divider-default-bottom-width 1)
  (window-divider-mode 1)
  (let* ((sep1 (propertize "" 'face '(:foreground "#282828" :background "#282828" :box (:line-width (0 . 0) :color "#181818" :style none))))
         (sep2 (propertize "" 'face '(:foreground "#282828" :background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))))
         (sep3 (propertize "" 'face '(:foreground "#181818" :background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))))
         (sep4 (propertize "" 'face '(:foreground "#282828" :background "#181818" :box (:line-width (0 . 0) :color "#181818" :style none))))
         (inner (propertize "   %b  "
                            'face '(:background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))
                            'local-map mode-line-buffer-identification-keymap)))
    (setq-default mode-line-buffer-identification (list sep1 sep2 inner sep3 sep4)))
  (setq-default mode-line-format
                `("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  mode-line-modes
                  mode-line-input-method-map
                  mode-line-format-right-align
                  mode-line-bury-buffer
                  mode-line-misc-info
                  (:eval
                   (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line mouse-1]
                       (lambda () (interactive)
                         (when (use-region-p)
                           (copy-region-as-kill (region-beginning) (region-end))
                           (message "Region copied to the kill ring."))))
                     (propertize (format " [%d:%d] " (line-number-at-pos) (current-column))
                                 'face 'cf-modeline-position
                                 'mouse-face 'mode-line-highlight
                                 'help-echo "mouse-1: Copy region"
                                 'local-map map)))
                  (:eval
                   (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line mouse-1] (lambda () (interactive) (view-mode 'toggle)))
                     (define-key map [mode-line mouse-2] (lambda () (interactive) (eval-buffer)))
                     (define-key map [mode-line mouse-3] (lambda () (interactive) (save-buffer)))
                     (propertize
                      (cond
                       (buffer-read-only " [RO] ")
                       ((buffer-modified-p) " [+] ")
                       (t " [-] "))
                      'face (cond
                             (buffer-read-only 'cf-modeline-buffer-readonly)
                             ((buffer-modified-p) 'cf-modeline-buffer-modified)
                             (t 'cf-modeline-buffer-saved))
                      'mouse-face 'mode-line-highlight
                      'help-echo "mouse-1: Toggle view-mode\nmouse-2: Eval buffer\nmouse-3: Save buffer"
                      'local-map map)))
                  (:eval (propertize (format-time-string "[%H:%M] ")
                                     'face 'cf-modeline-time
                                     'help-echo (format-time-string "%c")))
                  mode-line-end-spaces)))

(add-hook 'after-init-hook #'cf--default-mode-line-format)
(setq-default mode-line-format (cf--default-mode-line-format))

;;;###autoload
(defun cf--active-mode-line-format ()
  "Apply custom `mode-line-format'."
  (interactive)
  (setq cf--mode-line-visible t)

  (setq-local blink-cursor-blinks 2
              window-divider-default-places 'bottom-only
              window-divider-default-bottom-width 1)
  (cond
   ((eq major-mode 'eww-mode)
    (setq-local header-line-format "%t %u"))

   ((and (eq major-mode 'Info-mode) ((boundp 'Info-use-header-line) Info-use-header-line))
    (setq-local header-line-format
                '(:eval (get-text-property (point-min) 'header-line))))
   ((eq header-line-format "")
    (setq-local header-line-format nil))
   (t
    (setq cf--header-line-visible t)))

  (let* ((sep1 (propertize "" 'face '(:foreground "#282828" :background "#282828" :box (:line-width (0 . 0) :color "#181818" :style none))))
         (sep2 (propertize "" 'face '(:foreground "#282828" :background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))))
         (sep3 (propertize "" 'face '(:foreground "#181818" :background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))))
         (sep4 (propertize "" 'face '(:foreground "#282828" :background "#181818" :box (:line-width (0 . 0) :color "#181818" :style none))))
         (inner (propertize "   %b  "
                            'face '(:background "#181818" :box (:line-width (0 . -1) :color "#181818" :style none))
                            'local-map mode-line-buffer-identification-keymap)))
    (setq-local mode-line-buffer-identification (list sep1 sep2 inner sep3 sep4)))
  (setq-local mode-line-format
              `("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                mode-line-modes
                mode-line-input-method-map
                mode-line-format-right-align
                mode-line-bury-buffer
                ;;  mode-line-misc-info
                (:eval
                 (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line mouse-1]
                               (lambda () (interactive)
                                 (when (use-region-p)
                                   (copy-region-as-kill (region-beginning) (region-end))
                                   (message "Region copied to the kill ring."))))
                   (propertize (format " [%d:%d] " (line-number-at-pos) (current-column))
                               'face 'cf-modeline-position
                               'mouse-face 'mode-line-highlight
                               'help-echo "mouse-1: Copy region"
                               'local-map map)))
                (:eval
                 (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line mouse-1] (lambda () (interactive) (view-mode 'toggle)))
                   (define-key map [mode-line mouse-2] (lambda () (interactive) (eval-buffer)))
                   (define-key map [mode-line mouse-3] (lambda () (interactive) (save-buffer)))
                   (propertize
                    (cond
                     (buffer-read-only " [RO] ")
                     ((buffer-modified-p) " [+] ")
                     (t " [-] "))
                    'face (cond
                           (buffer-read-only 'cf-modeline-buffer-readonly)
                           ((buffer-modified-p) 'cf-modeline-buffer-modified)
                           (t 'cf-modeline-buffer-saved))
                    'mouse-face 'mode-line-highlight
                    'help-echo "mouse-1: Toggle view-mode\nmouse-2: Eval buffer\nmouse-3: Save buffer"
                    'local-map map)))
                (:eval (propertize (format-time-string "[%H:%M] ")
                                   'face 'cf-modeline-time
                                   'help-echo (format-time-string "%c")))
                mode-line-end-spaces)))

;;;###autoload
(defun cf--hidden-mode-line-format ()
  "Hide the mode line."
  (interactive)
  (setq cf--mode-line-visible nil)
  (setq-local header-line-format nil)
  (setq-local mode-line-format nil))

;;;###autoload
(defun cf/mode-line-toggle ()
  "Toggle local mode-line display."
  (interactive)
  (if cf--mode-line-visible
      (progn
        (window-divider-mode -1)
        (cf--hidden-mode-line-format))
    (progn
      (window-divider-mode 1)
      (cf--active-mode-line-format)))
  (force-mode-line-update)
  (redraw-display))

(keymap-global-set "C-c M-b" #'cf/mode-line-toggle)


;;; _
(provide 'cf-modeline)
;;; cf-modeline.el ends here

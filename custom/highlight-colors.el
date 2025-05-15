;;; highlight-colors.el --- Highlight color aliased names -*- lexical-binding: t; -*-

;;; Commentary:
;; Given plists of color names to color values, highlights alias names in the buffer.
;; Saves named color palettes in .hl-colors in `user-emacs-directory`.

;;; Code:

(defvar highlight-colors-history nil
  "Alist of saved color plists.
Each entry is of the form (NAME . PLIST), where PLIST is a list of (symbol . \"#hex\").")

(defconst highlight-colors-history-file
  (expand-file-name ".hl-colors" user-emacs-directory)
  "File to persist `highlight-colors-history`.")

;;;###autoload
(defun highlight-colors-save-history ()
  "Save `highlight-colors-history` to file."
  (when (listp highlight-colors-history)
    (with-temp-file highlight-colors-history-file
      (prin1 highlight-colors-history (current-buffer)))))

;;;###autoload
(defun highlight-colors-load-history ()
  "Load `highlight-colors-history` from file."
  (when (file-exists-p highlight-colors-history-file)
    (with-temp-buffer
      (insert-file-contents highlight-colors-history-file)
      (setq highlight-colors-history (read (current-buffer))))))

(defun highlight-colors--apply (palette)
    "Apply color highlighting based on the given PALETTE (a plist of color names and their hex values)."
  (save-excursion
    (dolist (entry palette)
      (let ((color-name (symbol-name (car entry)))
            (color-value (cdr entry)))
        (goto-char (point-min))
        (while (search-forward color-name nil t)
          (put-text-property
           (match-beginning 0) (match-end 0)
           'font-lock-face `(:foreground ,color-value)))))))

;;;###autoload
  (defun highlight-colors (&optional name-or-plist)
  "Highlight color aliases using a saved NAME or a new PLIST.
If called interactively without argument, prompts for known NAMEs.
If called with a raw PLIST, prompts for a name and saves it."
  (interactive)
  (highlight-colors-load-history)
  (let* ((entry
          (cond
           ((and name-or-plist (listp name-or-plist))
            (let ((name (intern (read-string "Name for this palette: "))))
              (setf (alist-get name highlight-colors-history) name-or-plist)
              (highlight-colors-save-history)
              (cons name name-or-plist)))

           ((called-interactively-p 'any)
            (let* ((names (mapcar (lambda (e) (symbol-name (car e))) highlight-colors-history))
                   (choice (completing-read "Choose palette: " names nil t)))
              (assoc (intern choice) highlight-colors-history)))

           ((symbolp name-or-plist)
            (assoc name-or-plist highlight-colors-history))

           (t
            (user-error "Invalid argument")))))
    (unless entry
      (user-error "No matching palette found"))
    (highlight-colors--apply (cdr entry))))

;;;###autoload
(defun highlight-face-properties-with-palette ()
  "Highlight face properties in the buffer, using the Gruber Darker color palette."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "(\\([a-zA-Z0-9-]+\\)\\s-+((t ,(list :foreground \\([a-zA-Z0-9-]+\\) :background \\([a-zA-Z0-9-]+\\))))"
            nil t)
      (let* ((face-name (match-string 1))
             (fg-name (match-string 3))
             (bg-name (match-string 4))
             (fg-color (cdr (assoc (intern fg-name) gruber-darker-palette)))
             (bg-color (cdr (assoc (intern bg-name) gruber-darker-palette))))

        ;; Highlight the face name with both foreground and background colors
        (when (and fg-color bg-color)
          (put-text-property (match-beginning 1) (match-end 1)
                             'font-lock-face `(:foreground ,fg-color :background ,bg-color)))

        ;; Highlight the foreground color text
        (when fg-color
          (put-text-property (match-beginning 3) (match-end 3)
                             'font-lock-face `(:foreground ,fg-color)))

        ;; Highlight the background color text
        (when bg-color
          (put-text-property (match-beginning 4) (match-end 4)
                             'font-lock-face `(:background ,bg-color)))))))

;;;###autoload
(defun highlight-colors-with-palette (plist)
  "Apply color highlighting from PLIST of ([:foreground|:background] SYMBOL . COLOR) using Gruber palette."
  (save-excursion
    (dolist (entry plist)
      (let* ((spec entry)
             (prop (if (memq (car-safe spec) '(:foreground :background))
                       (pop spec)
                     :foreground))
             (color-name (symbol-name (car spec)))
             (color-value (cdr spec)))
        (goto-char (point-min))
        (while (search-forward color-name nil t)
          (put-text-property
           (match-beginning 0) (match-end 0)
           'font-lock-face `((,prop ,color-value))))))))

(defgroup gruber-darker-theme nil
  "Gruber Darker theme customization group."
  :group 'faces)

(defcustom gruber-darker-palette
  '((gruber-darker-fg        . "#e2e2ef")
    (gruber-darker-fg+1      . "#f4f4f3")
    (gruber-darker-fg+2      . "#f5f5f5")
    (gruber-darker-white     . "#f6f6fe")
    (gruber-darker-black     . "#000000")
    (gruber-darker-bg-1      . "#101010")
    (gruber-darker-bg        . "#181818")
    (gruber-darker-bg+1      . "#282828")
    (gruber-darker-bg+2      . "#453d41")
    (gruber-darker-bg+3      . "#484848")
    (gruber-darker-bg+4      . "#52494e")
    (gruber-darker-red-1     . "#c73c3f")
    (gruber-darker-red       . "#f43841")
    (gruber-darker-red+1     . "#ff4f58")
    (gruber-darker-green     . "#73c936")
    (gruber-darker-yellow    . "#ffdd33")
    (gruber-darker-brown     . "#cb9c3c")
    (gruber-darker-quartz    . "#95a99f")
    (gruber-darker-niagara-2 . "#303540")
    (gruber-darker-niagara-1 . "#565f73")
    (gruber-darker-niagara   . "#96a6c8")
    (gruber-darker-wisteria  . "#9e95c7"))
  "Gruber Darker color palette used for UI and syntax highlighting."
  :type '(alist :key-type symbol :value-type color)
  :group 'gruber-darker-theme)

(add-to-list 'highlight-colors-history
             (cons 'gruber gruber-darker-palette))

;; Auto-load/save on session
(add-hook 'emacs-startup-hook #'highlight-colors-load-history)
(add-hook 'kill-emacs-hook #'highlight-colors-save-history)

(provide 'highlight-colors)
;;; highlight-colors.el ends here

;;; hls-playlist-mode.el --- Major mode for editing HLS playlists -*- lexical-binding:t; -*-

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
;;  This mode aims to make viewing and editing HLS playlists for use
;;  as an extension to `yt-dired-mode'.
;;  It sets up font-lock and checks for mandatory basic component.
;;  This work is in progress and may not be fully compliant with the
;;  standard, but from my experience, playlists do play fine in `mpv'.
;;  Provides editing, pretty-printing, and sortable list viewing for HLS playlists.
;;  Integrates with mpv for launching playlist starting from any entry in current sort order.

;;; Code:
;;; Commentary:
;; Tabulated-list derived mode to view, sort, and launch HLS playlists (.m3u/.m3u8).
;; Ignores #EXTM3U and raw #EXTINF lines in display.
;; Reconstructs valid playlists respecting sort order, looping from selection.
;; Title is buttonized, sortable by Title, Duration, Date.

;;; Code:

(eval-when-compile
  (require 'tabulated-list)
  (require 'cl-lib))

;;;; Mode Definition

(define-derived-mode hls-playlist-mode tabulated-list-mode "HLS-Playlist"
  "Major mode for viewing, sorting, and launching HLS playlists."
  (setq tabulated-list-format [("Date" 12 t)
                                ("Title" 132 t)
                                ("Duration" 12 t)
                                ("URI" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Date" . nil))
  (add-hook 'tabulated-list-revert-hook #'hls-playlist--populate nil t)
  (add-hook 'hls-playlist-mode-hook #'hl-line-mode)
  (tabulated-list-init-header))

;;;; Parsing

(defun hls-playlist--parse-buffer ()
  "Parse buffer into list of (Date Title Duration URI), ignoring #EXTM3U."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      ;; Skip #EXTM3U if present
      (when (looking-at "^#EXTM3U\\s-*$")
        (forward-line 1))
      (while (re-search-forward "^#EXTINF:\\([0-9]+\\),\\(.*\\),\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\n\\(.*\\)" nil t)
        (push (list (match-string 3) ; Date
                    (match-string 2) ; Title
                    (string-to-number (match-string 1)) ; Duration
                    (match-string 4)) ; URI
              entries))
      (nreverse entries))))

;;;; Populate Tabulated List

(defun hls-playlist--populate ()
  "Populate tabulated list from buffer contents."
  (let* ((entries (hls-playlist--parse-buffer))
         (rows (mapcar (lambda (e)
                         (list e
                               (vector (nth 0 e) ; Date
                                       (nth 1 e)
                                       (format "+%02d:%02d:%02d"
                                               (/ (nth 2 e) 3600)
                                               (% (/ (nth 2 e) 60) 60)
                                               (% (nth 2 e) 60))
                                       (nth 3 e))))
                       entries)))
    (setq tabulated-list-entries rows)))

(defun hls-playlist-toggle-sort ()
  "Toggle sorting by column at point."
  (interactive)
  (let ((name (tabulated-list--column-name-at (point))))
    (when name
      (setq tabulated-list-sort-key
            (if (and tabulated-list-sort-key
                     (string= (car tabulated-list-sort-key) name)
                     (not (cdr tabulated-list-sort-key)))
                (cons name t)
              (cons name nil)))
      (tabulated-list-print t))))

;;;; Playlist Launch

(defun hls-playlist-launch-from-selection ()
  "Launch playlist in mpv starting from selected entry, looping."
  (interactive)
  (let* ((all (mapcar #'car tabulated-list-entries))
         (sel (tabulated-list-get-id))
         (split (member sel all))
         (ordered (append split (cl-subseq all 0 (- (length all) (length split)))))
         (playlist (concat "#EXTM3U\n"
                           (mapconcat (lambda (e)
                                        (format "#EXTINF:%d,%s,%s\n%s"
                                                (nth 2 e) (nth 1 e) (nth 0 e) (nth 3 e)))
                                      ordered
                                      "\n"))))
    (when playlist
      (with-temp-buffer
        (insert playlist)
        (let ((file (make-temp-file "hls-playlist-" nil ".m3u8")))
          (write-region (point-min) (point-max) file)
          (start-process-shell-command "mpv" nil
                                       (format "mpv %s" (shell-quote-argument file))))))))

;;;; Entry Point

;;;###autoload
(defun hls-playlist-open ()
  "Open current buffer as interactive HLS playlist tabulated list."
  (interactive)
  (hls-playlist-mode)
  (hls-playlist--populate)
  (tabulated-list-print))

;;;; Keybindings

(defvar hls-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key hls-playlist-mode-map (kbd "RET") #'hls-playlist-launch-from-selection)
    (define-key hls-playlist-mode-map [mouse-1] #'hls-playlist-launch-from-selection)
    map)
  "Keymap for `hls-playlist-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m3u8\\'" . hls-playlist-mode))



(provide 'hls-playlist-mode)
;;; hls-playlist-mode.el ends here

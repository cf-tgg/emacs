;;; cf-yt-dired.el --- Dired-based YouTube playlist manager with thumbnails -*- lexical-binding: t; -*-

(require 'dired)
(require 'tabulated-list)
(require 'cl-lib)
(require 'image)

(defvar yt-dired-playlist-buffer "*YT Dired Playlist*")

(defvar yt-dired-playlist-entries nil
  "Internal list of playlist entries for yt-dired-playlist-mode.")

(define-derived-mode yt-dired-mode dired-mode "YT-Dired"
  "Dired mode extension for generating YouTube playlist tables with thumbnails."
  (easy-menu-add-item nil '("Operate")
                      ["Generate YT Playlist Table" yt-dired-generate-playlist-table t]))

(defun yt-dired--parse-table-file (tbl-file)
  "Parse the generated .tbl file into tabulated list entries."
  (with-temp-buffer
    (insert-file-contents tbl-file)
    (let ((lines (split-string (buffer-string) "\n" t))
          (entries '()))
      (dolist (line (cdr lines)) ; skip header
        (let* ((fields (split-string line "\t"))
               (date (or (nth 0 fields) ""))
               (title (or (nth 1 fields) ""))
               (duration (or (nth 2 fields) ""))
               (uri (or (nth 3 fields) ""))
               (thumb (or (nth 4 fields) "")))
          (when (and uri (not (string-empty-p uri)))
            (let ((image (if (and (file-exists-p thumb) (not (string-empty-p thumb)))
                             (propertize " " 'display (create-image thumb :width 80 :height 45))
                           "")))
              (push (list uri (vector date title duration uri image)) entries)))))
      (nreverse entries))))

(defun yt-dired-generate-playlist-table ()
  "Generate tabulated playlist buffer from marked files."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (tbl-file (make-temp-file "yt-dired" nil ".tbl")))
    (dolist (f files)
      (call-process-shell-command (format "exifjpg2tbl %s >> %s"
                                          (shell-quote-argument f)
                                          (shell-quote-argument tbl-file))))
    (yt-dired-playlist-mode)
    (setq yt-dired-playlist-entries (yt-dired--parse-table-file tbl-file))
    (setq tabulated-list-entries yt-dired-playlist-entries)
    (tabulated-list-print)))

(define-derived-mode yt-dired-playlist-mode tabulated-list-mode "YT-Playlist"
  "Tabulated list mode for YouTube playlist entries with thumbnails."
  (setq tabulated-list-format [("Date" 12 t)
                               ("Title" 40 t)
                               ("Duration" 8 t)
                               ("Uri" 50 t)
                               ("Thumbnail" 10 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(defun yt-dired-playlist-generate-m3u ()
  "Generate a valid .m3u playlist from current tabulated list."
  (interactive)
  (let ((m3u "#EXTM3U\n"))
    (dolist (entry yt-dired-playlist-entries)
      (let* ((vec (cadr entry))
             (duration (string-to-number (aref vec 2)))
             (title (aref vec 1))
             (date (aref vec 0))
             (uri (aref vec 3)))
        (setq m3u (concat m3u (format "#EXTINF:%d,%s,%s\n%s\n" duration title date uri)))))
    (with-temp-buffer
      (insert m3u)
      (let ((file (read-file-name "Write playlist to: " nil nil nil "playlist.m3u8")))
        (write-region (point-min) (point-max) file)
        (message "Playlist written to %s" file)))))

(defun yt-dired-playlist-launch-mpv-from-selection ()
  "Launch playlist in mpv starting from selected entry, looping."
  (interactive)
  (let* ((all (mapcar #'car yt-dired-playlist-entries))
         (sel (tabulated-list-get-id))
         (split (member sel all))
         (ordered (append split (cl-subseq all 0 (- (length all) (length split)))))
         (m3u "#EXTM3U\n"))
    (dolist (uri ordered)
      (let* ((entry (assoc uri yt-dired-playlist-entries))
             (vec (cadr entry))
             (duration (string-to-number (aref vec 2)))
             (title (aref vec 1))
             (date (aref vec 0))
             (url (aref vec 3)))
        (setq m3u (concat m3u (format "#EXTINF:%d,%s,%s\n%s\n" duration title date url)))))
    (with-temp-buffer
      (insert m3u)
      (let ((file (make-temp-file "yt-dired-playlist-" nil ".m3u8")))
        (write-region (point-min) (point-max) file)
        (start-process-shell-command "mpv" nil (format "mpv %s" (shell-quote-argument file)))))))

(defun yt-dired-playlist-delete-entry ()
  "Delete entry at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (setq yt-dired-playlist-entries (assq-delete-all id yt-dired-playlist-entries))
    (setq tabulated-list-entries yt-dired-playlist-entries)
    (tabulated-list-print)))

(defun yt-dired-playlist-move-entry-up ()
  "Move entry at point up."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (pos (cl-position id (mapcar #'car yt-dired-playlist-entries) :test #'equal)))
    (when (and pos (> pos 0))
      (cl-rotatef (nth pos yt-dired-playlist-entries) (nth (1- pos) yt-dired-playlist-entries))
      (setq tabulated-list-entries yt-dired-playlist-entries)
      (tabulated-list-print)
      (goto-char (point-min))
      (forward-line (1- pos)))))

(defun yt-dired-playlist-move-entry-down ()
  "Move entry at point down."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (len (length yt-dired-playlist-entries))
         (pos (cl-position id (mapcar #'car yt-dired-playlist-entries) :test #'equal)))
    (when (and pos (< pos (1- len)))
      (cl-rotatef (nth pos yt-dired-playlist-entries) (nth (1+ pos) yt-dired-playlist-entries))
      (setq tabulated-list-entries yt-dired-playlist-entries)
      (tabulated-list-print)
      (goto-char (point-min))
      (forward-line (1+ pos)))))

(define-key yt-dired-playlist-mode-map (kbd "RET") #'yt-dired-playlist-launch-mpv-from-selection)
(define-key yt-dired-playlist-mode-map (kbd "d") #'yt-dired-playlist-delete-entry)
(define-key yt-dired-playlist-mode-map (kbd "M-<up>") #'yt-dired-playlist-move-entry-up)
(define-key yt-dired-playlist-mode-map (kbd "M-<down>") #'yt-dired-playlist-move-entry-down)
(define-key yt-dired-playlist-mode-map (kbd "C-c C-s") #'yt-dired-playlist-generate-m3u)

(provide 'cf-yt-dired)

;;; cf-yt-dired.el ends here

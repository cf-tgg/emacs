;;; music.el --- music el -*- lexical-binding: t -*-

;;; Commentary:
;;       music packs.

;;; Code:

;;; E-Radio
(use-package eradio
  :ensure t
  :custom
  (eradio-player '("mpv" "-quiet" "--no-audio-display" "--no-load-scripts" "--no-video" "--no-terminal"))
  :commands (eradio-toggle eradio-play)
  :config
  (setq eradio-channels '(("Totally 80s FM" . "https://zeno.fm/radio/totally-80s-fm/")
                          ("Oldies Radio 50s-60s" . "https://zeno.fm/radio/oldies-radio-50s-60s/")
                          ("Oldies Radio 70s" . "https://zeno.fm/radio/oldies-radio-70s/")
                          ("Unlimited 80s" . "https://zeno.fm/radio/unlimited80s/")
                          ("80s Hits" . "https://zeno.fm/radio/80shits/")
                          ("90s Hits" . "https://zeno.fm/radio/90s_HITS/")
                          ("2000s Pop" . "https://zeno.fm/radio/2000s-pop/")
                          ("The 2000s" . "https://zeno.fm/radio/the-2000s/")
                          ("Hits 2010s" . "https://zeno.fm/radio/helia-hits-2010/")
                          ("Classical Radio" . "https://zeno.fm/radio/classical-radio/")
                          ("Classical Relaxation" . "https://zeno.fm/radio/radio-christmas-non-stop-classical/")
                          ("Classic Rock" . "https://zeno.fm/radio/classic-rockdnb2sav8qs8uv/")
                          ("Gangsta49" . "https://zeno.fm/radio/gangsta49/")
                          ("HipHop49" . "https://zeno.fm/radio/hiphop49/")
                          ("Madhouse Country Radio" . "https://zeno.fm/radio/madhouse-country-radio/")
                          ("PopMusic" . "https://zeno.fm/radio/popmusic74vyurvmug0uv/")
                          ("PopStars" . "https://zeno.fm/radio/popstars/")
                          ("RadioMetal" . "https://zeno.fm/radio/radio-metal/")
                          ("RocknRoll Radio" . "https://zeno.fm/radio/rocknroll-radio994c7517qs8uv/"))))

;;; MPC
(use-package mpc
  :ensure nil
  :bind
  (:map mpc-mode-map
        ("RET" . mpc-select)
        ("SPC" . scroll-up-command)
        ("DEL" . scroll-down-command)
        ("<" . mpc-prev)
        (">" . mpc-next)
        ("N" . mpc-prev)
        ("n" . next-line)
        ("p" . previous-line)
        ("D" . mpc-describe-song)
        ("d" . cf/scroll-up-and-recenter)
        ("u" . cf/scroll-down-and-recenter)
        ("]" . mpc-seek-current)
        ("g" . mpc-goto-playing-song)
        ("p" . mpc-play)
        ("q" . mpc-quit)
        ("s" . mpc-toggle-play)
        ("<mouse-2>" . mpc-select-toggle)
        ("<down-mouse-3>" . mpc-select-extend)
        ("S-<return>"  .	mpc-select-toggle)
        ("C-<down-mouse-1>" . mpc-drag-n-drop)
        ("<mouse-1>" . mpc-select)))

(use-package bongo
  :ensure t
  :config
  (setq bongo-default-directory "~/Music")
  (setq bongo-prefer-library-buffers nil )
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-inline-playback-progress t)
  (setq bongo-join-inserted-tracks t)
  (setq bongo-field-separator (propertize " . " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-enabled-backends '(mpv vlc))
  (setq bongo-vlc-program-name "cvlc")

  (defvar cf/bongo-playlist-delimiter
    "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"
    "Delimiter for inserted entries in `bongo playlist buffers'.")

  (defun cf/bongo-playlist-section ()
    (bongo-insert-comment-text
     cf/bongo-playlist-delimiter))

  (defun cf/bongo-paylist-section-next ()
    "Move to next `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-forward section nil t))
          (progn
            (goto-char (pos-eol))
            (re-search-forward section nil t))
        (goto-char (point-max)))))

  (defun cf/bongo-paylist-section-previous ()
    "Move to previous `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (goto-char (point-at-bol))
            (re-search-backward section nil t))
        (goto-char (point-min)))))

  (defun cf/bongo-playlist-mark-section ()
    "Mark `bongo' playlist section, delimited by custom markers.
The marker is `cf/bongo-playlist-delimiter'."
    (interactive)
    (let ((section "^\\*+$"))
      (search-forward-regexp section nil t)
      (push-mark nil t)
      (forward-line -1)
      ;; REVIEW any predicate to replace this `save-excursion'?
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (search-backward-regexp section nil t)
            (forward-line 1))
        (goto-char (point-min)))
      (activate-mark)))

  (defun cf/bongo-playlist-kill-section ()
    "Kill `bongo' playlist-section at point.
This operates on a custom delimited section of the buffer.  See
`cf/bongo-playlist-kill-section'."
    (interactive)
    (cf/bongo-playlist-mark-section)
    (bongo-kill))

  (defun cf/bongo-playlist-play-random ()
    "Play random `bongo' track and determine further conditions."
    (interactive)
    (unless (bongo-playlist-buffer)
      (bongo-playlist-buffer))
    (when (or (bongo-playlist-buffer-p)
              (bongo-library-buffer-p))
      (unless (bongo-playing-p)
        (with-current-buffer (bongo-playlist-buffer)
          (bongo-play-random)
          (bongo-random-playback-mode 1)
          (bongo-recenter)))))

  (defun cf/bongo-playlist-random-toggle ()
    "Toggle `bongo-random-playback-mode' in playlist buffers."
    (interactive)
    (if (eq bongo-next-action 'bongo-play-random-or-stop)
        (bongo-progressive-playback-mode)
      (bongo-random-playback-mode)))

  (defun cf/bongo-playlist-reset ()
    "Stop playback and reset `bongo' playlist marks.
To reset the playlist is to undo the marks produced by non-nil
`bongo-mark-played-tracks'."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-reset-playlist)))

  (defun cf/bongo-playlist-terminate ()
    "Stop playback and clear the entire `bongo' playlist buffer.
Contrary to the standard `bongo-erase-buffer', this also removes
the currently-playing track."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-erase-buffer)))

  (defun cf/bongo-playlist-insert-playlist-file ()
    "Insert contents of playlist file to a `bongo' playlist.
Upon insertion, playback starts immediately, in accordance with
`cf/bongo-play-random'.

The available options at the completion prompt point to files
that hold filesystem paths of media items.  Think of them as
'directories of directories' that mix manually selected media
items.

Also see `cf/bongo-dired-make-playlist-file'."
    (interactive)
    (let* ((path "~/Music/playlists/")
           (dotless directory-files-no-dot-files-regexp)
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files path nil dotless)))
           (choice (completing-read "Insert playlist: " playlists nil t)))
      (if (bongo-playlist-buffer-p)
          (progn
            (save-excursion
              (goto-char (point-max))
              (bongo-insert-playlist-contents
               (format "%s%s" path choice))
              (cf/bongo-playlist-section))
            (cf/bongo-playlist-play-random))
        (user-error "Not in a `bongo' playlist buffer"))))

;;; Bongo + Dired (bongo library buffer)
  (defmacro cf/bongo-dired-library (name doc val)
    "Create `bongo' library function NAME with DOC and VAL."
    `(defun ,name ()
       ,doc
       (when (string-match-p "\\`~/Music/" default-directory)
         (bongo-dired-library-mode ,val))))

  (cf/bongo-dired-library
   cf/bongo-dired-library-enable
   "Set `bongo-dired-library-mode' when accessing ~/Music.

Add this to `dired-mode-hook'.  Upon activation, the directory
and all its sub-directories become a valid library buffer for
Bongo, from where we can, among others, add tracks to playlists.
The added benefit is that Dired will continue to behave as
normal, making this a superior alternative to a purpose-specific
library buffer.

Note, though, that this will interfere with `wdired-mode'.  See
`cf/bongo-dired-library-disable'."
   1)

  ;; NOTE `cf/bongo-dired-library-enable' does not get reactivated
  ;; upon exiting `wdired-mode'.
  ;;
  ;; TODO reactivate bongo dired library upon wdired exit
  (cf/bongo-dired-library
   cf/bongo-dired-library-disable
   "Unset `bongo-dired-library-mode' when accessing ~/Music.
This should be added `wdired-mode-hook'.  For more, refer to
`cf/bongo-dired-library-enable'."
   -1)

  (defun cf/bongo-dired-insert-files ()
    "Add files in a `dired' buffer to the `bongo' playlist."
    (let ((media (dired-get-marked-files)))
      (with-current-buffer (bongo-playlist-buffer)
        (goto-char (point-max))
        (mapc 'bongo-insert-file media)
        (cf/bongo-playlist-section))
      (with-current-buffer (bongo-library-buffer)
        (dired-next-line 1))))

  (defun cf/bongo-dired-insert ()
    "Add `dired' item at point or marks to `bongo' playlist.

The playlist is created, if necessary, while some other tweaks
are introduced.  See `cf/bongo-dired-insert-files' as well as
`cf/bongo-playlist-play-random'.

Meant to work while inside a `dired' buffer that doubles as a
library buffer (see `cf/bongo-dired-library')."
    (interactive)
    (when (bongo-library-buffer-p)
      (unless (bongo-playlist-buffer-p)
        (bongo-playlist-buffer))
      (cf/bongo-dired-insert-files)
      (cf/bongo-playlist-play-random)))

  (defun cf/bongo-dired-make-playlist-file ()
    "Add `dired' marked items to playlist file using completion.

These files are meant to reference filesystem paths.  They ease
the task of playing media from closely related directory trees,
without having to interfere with the user's directory
structure (e.g. a playlist file 'rock' can include the paths of
~/Music/Scorpions and ~/Music/Queen).

This works by appending the absolute filesystem path of each item
to the selected playlist file.  If no marks are available, the
item at point will be used instead.

Selecting a non-existent file at the prompt will create a new
entry whose name matches user input.  Depending on the completion
framework, such as with `icomplete-mode', this may require a
forced exit (e.g. \\[exit-minibuffer] to parse the input without
further questions).

Also see `cf/bongo-playlist-insert-playlist-file'."
    (interactive)
    (let* ((dotless directory-files-no-dot-files-regexp)
           (pldir "~/Music/playlists")
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files pldir nil dotless)))
           (plname (completing-read "Select playlist: " playlists nil nil))
           (plfile (format "%s/%s" pldir plname))
           (media-paths
            (if (derived-mode-p 'dired-mode)
                ;; TODO more efficient way to do ensure newline ending?
                ;;
                ;; The issue is that we need to have a newline at the
                ;; end of the file, so that when we append again we
                ;; start on an empty line.
                (concat
                 (mapconcat #'identity
                            (dired-get-marked-files)
                            "\n")
                 "\n")
              (user-error "Not in a `dired' buffer"))))
      ;; The following `when' just checks for an empty string.  If we
      ;; wanted to make this more robust we should also check for names
      ;; that contain only spaces and/or invalid characters…  This is
      ;; good enough for me.
      (when (string-empty-p plname)
        (user-error "No playlist file has been specified"))
      (unless (file-directory-p pldir)
        (make-directory pldir))
      (unless (and (file-exists-p plfile)
                   (file-readable-p plfile)
                   (not (file-directory-p plfile)))
        (make-empty-file plfile))
      (append-to-file media-paths nil plfile)
      (with-current-buffer (find-file-noselect plfile)
        (delete-duplicate-lines (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (save-buffer)
        (kill-buffer))))

  :hook ((dired-mode-hook . cf/bongo-dired-library-enable)
         (wdired-mode-hook . cf/bongo-dired-library-disable))
  :bind (("<C-XF86AudioPlay>" . bongo-pause/resume)
         ("<C-XF86AudioNext>" . bongo-next)
         ("<C-XF86AudioPrev>" . bongo-previous)
         ("<M-XF86AudioPlay>" . bongo-show)
         ("<S-XF86AudioNext>" . bongo-seek-forward-10)
         ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
         :map bongo-playlist-mode-map
         ("n" . bongo-next-object)
         ("p" . bongo-previous-object)
         ("M-n" . cf/bongo-playlist-section-next)
         ("M-p" . cf/bongo-paylist-section-previous)
         ("M-h" . cf/bongo-playlist-mark-section)
         ("M-d" . cf/bongo-playlist-kill-section)
         ("g" . cf/bongo-playlist-reset)
         ("D" . cf/bongo-playlist-terminate)
         ("r" . cf/bongo-playlist-random-toggle)
         ("R" . bongo-rename-line)
         ("j" . bongo-dired-line)       ; Jump to dir of file at point
         ("J" . dired-jump)             ; Jump to library buffer
         ("i" . cf/bongo-playlist-insert-playlist-file)
         ("I" . bongo-insert-special)
         :map bongo-dired-library-mode-map
         ("<C-return>" . cf/bongo-dired-insert)
         ("C-c SPC" . cf/bongo-dired-insert)
         ("C-c +" . cf/bongo-dired-make-playlist-file)))


(provide 'music.el)
;;; music.el ends here.

;;; cf-custom-functions.el --- Custom Elisp fun for day-to-day editing -*- lexical-binding: t; -*-

;;; Commentary:
;;     Took from Protesilaos' codelog: <https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/>

;; Copyright © 2025 cf dot gg

;; Author: cf <cf.gg.tty@protonmail.com>
;; Github: <https://github.com/cf-tgg/dotemacs/>.
;; Gitlab: <https://github.com/cf-gg/dotemacs/>.
;; Codeberg: <https://codeberg.org/cfggtty/dotemacs/>.
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;; Scratch buffers on demand
;; Package by Ian Eure (ieure on GitHub)
(use-package scratch
  :ensure
  :config
  (defun cf/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly."
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t)))
  :hook (scratch-create-buffer-hook . cf/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

;;; Maximise window + kill buffer (and close window)

;; `cf/window-single-toggle' is based on `windower' by Pierre
;; Neidhardt (ambrevar on GitLab)
(use-package emacs
  :config
  (defvar cf/window-configuration nil
    "Current window configuration.
Intended for use by `cf/window-monocle'.")

  (define-minor-mode cf/window-single-toggle
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    :lighter " [M]"
    :global nil
    (if (one-window-p)
        (when cf/window-configuration
          (set-window-configuration cf/window-configuration))
      (setq cf/window-configuration (current-window-configuration))
      (delete-other-windows)))

  (defun cf/kill-buffer-current (&optional arg)
    "Kill current buffer or abort recursion when in minibuffer."
    (interactive "P")
    (if (minibufferp)
        (abort-recursive-edit)
      (kill-buffer (current-buffer)))
    (when (and arg
               (not (one-window-p)))
      (delete-window)))
  :bind (("C-c w" . cf/window-single-toggle)
         ("C-x k" . cf/kill-buffer-current)))

;;; Marking constructs

(use-package emacs
  :commands (cf/mark-symbol
             cf/mark-sexp-backward)
  :config
  (defmacro cf/mark (name object &optional docstring)
    "Produce function for marking small syntactic constructs.
NAME is how the function should be called.  OBJECT is its scope.
Optional DOCSTRING describes the resulting function.

This is a slightly modified version of the built-in `mark-word'."
    `(defun ,name (&optional arg allow-extend)
       ,docstring
       (interactive "P\np")
       (let ((x (format "%s-%s" "forward" ,object)))
         (cond ((and allow-extend
                     (or (and (eq last-command this-command) (mark t))
                         (region-active-p)))
                (setq arg (if arg (prefix-numeric-value arg)
                            (if (< (mark) (point)) -1 1)))
                (set-mark
                 (save-excursion
                   (goto-char (mark))
                   (funcall (intern x) arg)
                   (point))))
               (t
                (let ((bounds (bounds-of-thing-at-point (intern ,object))))
                  (unless (consp bounds)
                    (user-error "No %s at point" ,object))
                  (if (>= (prefix-numeric-value arg) 0)
                      (goto-char (car bounds))
                    (goto-char (cdr bounds)))
                  (push-mark
                   (save-excursion
                     (funcall (intern x) (prefix-numeric-value arg))
                     (point)))
                  (activate-mark)))))))

  (cf/mark
   cf/mark-word
   "word"
   "Mark the whole word at point.
This function is a slightly modified version of the built-in
`mark-word', that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required.  For a general purpose utility, use `cf/mark-symbol'
instead.")

  (cf/mark
   cf/mark-symbol
   "symbol"
   "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining
ARGth symbols away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next
symbol in the direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

  (defun cf/mark-sexp-backward (&optional arg)
    "Mark previous or ARGth balanced expression[s].
Just a convenient backward-looking `mark-sexp'."
    (interactive "P")
    (if arg
        (mark-sexp (- arg) t)
      (mark-sexp (- 1) t)))

  (defun cf/mark-construct-dwim (&optional arg)
    "Mark symbol or balanced expression at point.
A do-what-I-mean wrapper for `cf/mark-sexp-backward',
`mark-sexp', and `cf/mark-symbol'.

When point is over a symbol, mark the entirety of it.  Regular
words are interpreted as symbols when an actual symbol is not
present.

For balanced expressions, a backward match will happen when point
is to the right of the closing delimiter.  A forward match is the
fallback condition and should work when point is before a
balanced expression, with or without whitespace in between it an
the opening delimiter.

Optional ARG will mark a total of ARGth objects while counting
the current one (so 3 would be 1+2 more).  A negative count moves
the mark backward (though that would invert the backward-moving
sexp matching of `cf/mark-sexp-backward', so be mindful of
where the point is).  Repeated invocations of this command
incrementally mark objects in the direction originally
specified."
    (interactive "P")
    (cond
     ((symbol-at-point)
      (cf/mark-symbol arg t))
     ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (cf/mark-sexp-backward arg))
     (t
      (mark-sexp arg t))))

  :bind (("M-@" . cf/mark-word)       ; replaces `mark-word'
         ("C-M-SPC" . cf/mark-construct-dwim)))

;;; Workspaces (Emacs 27 "tabs")

(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)

  (tab-bar-mode -1)
  (tab-bar-history-mode -1)

  (defun cf/tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (icomplete-vertical-do ()
               (tab-bar-switch-to-tab
                (completing-read "Select tab: " tabs nil t)))))))

  :bind (("C-x t t" . cf/tab-bar-select-tab-dwim)
         ("s-t" . cf/tab-bar-select-tab-dwim)
         ("C-x t s" . tab-switcher)))

;;; Git log with VC (built-in Version Control framework)

(use-package vc
  :config
  (setq vc-find-revision-no-save t)

  (use-package log-view
    :config
    (defun cf/vc-print-log (&optional arg)
      "Like `vc-print-log' but for a custom fileset.

With optional prefix ARG (\\[universal-argument]), query for a
number to limit the log to.  Then prompt the user for matching
files in the `default-directory'.  A literal space delimits
multiple files (inserting a space will renew the prompt, asking
for another file match).

In a `dired-mode' buffer, print log for the file at point, or any
marked files, except for when a double prefix argument is passed.
A single prefix arg still provides for a limit to the log.

If a double prefix ARG is passed, prompt for a limit and produce
a log that covers all files in the present directory."
      (interactive "P")
      (let* ((lim (if arg
                      (read-number "Limit log to N entries: " 5)
                    20))
             (dir default-directory)
             (dotless directory-files-no-dot-files-regexp)
             (files (directory-files dir nil dotless t))
             (crm-separator " ")
             (set (cond
                   ((equal arg '(16))
                    files)
                   ((eq major-mode 'dired-mode)
                    (dired-get-marked-files t nil))
                   (t
                    (icomplete-vertical-do ()
                      (completing-read-multiple
                       "Select files in current dir: " files nil t)))))
             (backend (vc-backend set)))
        (vc-print-log-internal backend set nil nil lim 'with-diff)))

    (defun cf/log-view-extract-commit ()
      "Kill commit from around point in `vc-print-log'."
      (interactive)
      (let ((commit (cadr (log-view-current-entry (point) t))))
        (kill-new (format "%s" commit))
        (message "Copied: %s" commit)))

    :bind (("C-x v SPC" . cf/vc-print-log)
           :map log-view-mode-map
           ("<tab>" . log-view-toggle-entry-display)
           ("<return>" . log-view-find-revision)
           ("w" . cf/log-view-extract-commit)
           ("s" . vc-log-search)
           ("o" . vc-log-outgoing)
           ("f" . vc-log-incoming)
           ("F" . vc-update)
           ("P" . vc-push)))

  :bind (("C-x v b" . vc-retrieve-tag)  ; "branch" switch
         ("C-x v t" . vc-create-tag)
         ("C-x v f" . vc-log-incoming)  ; the actual git fetch
         ("C-x v F" . vc-update)        ; "F" because "P" is push
         ("C-x v d" . vc-diff)))

;;; Diff functions

(use-package diff
  :commands cf/diff-refine-buffer
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)                ; I do it on demand
  (setq diff-font-lock-prettify nil)    ; better for patches
  (setq diff-font-lock-syntax nil)      ; good for accessibility

  (defun cf/diff-buffer-with-file (&optional arg)
    "Compare buffer to its file, else run `vc-diff'.
With \\[universal-argument] also enable highlighting of word-wise
changes, local to the current buffer."
    (interactive "P")
    (let ((buf nil))     ; this method will "fail" if multi diff buffers
      (if (buffer-modified-p)
          (progn
            (diff-buffer-with-file (current-buffer))
            (setq buf "*Diff*"))
        (vc-diff)
        (setq buf "*vc-diff*"))
      (when arg
        (with-current-buffer (get-buffer buf)
          (setq-local diff-refine 'font-lock)))))

  (defun cf/diff-refine-buffer ()
    "Produce word-wise, 'refined' diffs in `diff-mode' buffer.
Also see `cf/diff-refine-hunk-or-buf' that is a wrapper for the
current command."
    (interactive)
    (let ((position (point)))
      (when (derived-mode-p 'diff-mode)
        (setq-local diff-refine 'font-lock)
        (font-lock-flush (point-min) (point-max))
        (goto-char position))))

  (defun cf/diff-refine-hunk-or-buf (&optional arg)
    "Apply word-wise, 'refined' diffs to hunk or buffer.
With prefix ARG (\\[universal-argument]), refine the entire
buffer, else the diff hunk at point.

This is a wrapper around `cf/diff-refine-buffer' and
`diff-refine-hunk', meant to economise on key bindings."
    (interactive "P")
    (if arg
        (cf/diff-refine-buffer)
      (diff-refine-hunk)))

  (defun cf/diff-restrict-view-dwim (&optional arg)
    "Use `diff-restrict-view', or widen when already narrowed.
By default the narrowing effect applies to the focused diff hunk.
With \\[universal-argument] do it for the current file instead."
    (interactive "P")
    (when (derived-mode-p 'diff-mode)
      (if (buffer-narrowed-p)
          (progn
            (widen)
            (message "Widened the view"))
        (if arg
            (progn
              (diff-restrict-view arg)
              (message "Narrowed to file"))
          (diff-restrict-view)
          (message "Narrowed to diff hunk")))))

  ;; `cf/diff-buffer-with-file' replaces the default for `vc-diff'
  ;; (which I bind to another key---see VC section).
  :bind (("C-x v =" . cf/diff-buffer-with-file)
         :map diff-mode-map
         ("C-c C-b" . cf/diff-refine-hunk-or-buf) ; replace `diff-refine-hunk'
         ("C-c C-n" . cf/diff-restrict-view-dwim)))

;;; Insert delimiters and text headings

(use-package emacs
  :config
  ;; Got those numbers from `string-to-char'
  (defconst cf/insert-pair-alist
    '(("' Single quote" . (39 39))           ; ' '
      ("\" Double quotes" . (34 34))         ; " "
      ("` Elisp quote" . (96 39))            ; ` '
      ("‘ Single apostrophe" . (8216 8217))  ; ‘ ’
      ("“ Double apostrophes" . (8220 8221)) ; “ ”
      ("( Parentheses" . (40 41))            ; ( )
      ("{ Curly brackets" . (123 125))       ; { }
      ("[ Square brackets" . (91 93))        ; [ ]
      ("< Angled brackets" . (60 62))        ; < >
      ("« Εισαγωγικά Gr quote" . (171 187))) ; « »
    "Alist of pairs for use with `cf/insert-pair-completion'.")

  (defun cf/insert-pair-completion (&optional arg)
    "Insert pair from `cf/insert-pair-alist'."
    (interactive "P")
    (let* ((data cf/insert-pair-alist)
           (chars (mapcar #'car data))
           (choice (completing-read "Select character: " chars nil t))
           (left (cadr (assoc choice data)))
           (right (caddr (assoc choice data))))
      (insert-pair arg left right)))

  ;; Based on `org--line-empty-p'.
  (defmacro cf/line-p (name regexp)
    "Make NAME function to match REGEXP on line n from point."
    `(defun ,name (n)
       (save-excursion
         (and (not (bobp))
	          (or (beginning-of-line n) t)
	          (save-match-data
	            (looking-at ,regexp))))))

  (cf/line-p
   cf/empty-line-p
   "[\s\t]*$")

  (cf/line-p
   cf/indent-line-p
   "^[\s\t]+")

  (cf/line-p
   cf/non-empty-line-p
   "^.*$")

  (cf/line-p
   cf/text-list-line-p
   "^\\([\s\t#*+]+\\|[0-9]+[).]+\\)")

  (cf/line-p
   cf/text-heading-line-p
   "^[=-]+")

  (defun cf/text-mode-heading (&optional arg)
    "Insert equal length heading delimiter below current line.

A heading delimiter is drawn as a series of dashes (-).  With
optional ARG, i.e. by prefixing \\[universal-argument], draw the
heading delimiter with equals signs (=).  The latter is
considered a heading level 1, while the former is level 2.

A heading delimiter is inserted only when that would not mess up
with existing headings or lists.  In such cases, point will move
to the next line.  For the purposes of this command, text that
starts with a number and no further delimiter is not consider a
list element.

This command is meant to be used in `text-mode' buffers and
derivatives, such as `markdown-mode', though not in `org-mode'."
    (interactive "P")
    (cond
     ((eq major-mode 'org-mode)
      (user-error "Do not use `cf/text-mode-heading' in `org-mode'!"))
     ((derived-mode-p 'text-mode)
      (let* ((count (- (point-at-eol) (point-at-bol)))
             (char (string-to-char (if arg "=" "-"))))
          (cond
           ((and (eobp)
                 (or (cf/text-list-line-p 1)
                     (cf/text-heading-line-p 1)
                     (cf/empty-line-p 1)
                     (cf/indent-line-p 1)))
            (newline 1))
           ((or (cf/empty-line-p 1)
                (cf/indent-line-p 1))
            (cf/new-line-below))
           ((or (cf/text-list-line-p 1)
                (cf/text-heading-line-p 2))
            (if (cf/empty-line-p 3)
                (beginning-of-line 3)
              (cf/new-line-below)))
           (t
            (cf/new-line-below)
            (insert-char char count nil)
            (newline 2)))))))

  :bind (("C-'" . cf/insert-pair-completion)
         ("M-'" . cf/insert-pair-completion)
         ("<C-M-backspace>" . backward-kill-sexp)
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)        ; "lower" case
         ("M-u" . upcase-dwim)
         :map text-mode-map
         ("<M-return>" . cf/text-mode-heading)))

;;; Search for files, directories, buffers

(use-package dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)

  (defmacro cf/dired-fd (name doc prompt &rest flags)
    "Make commands for selecting 'fd' results with completion.
NAME is how the function should be named.  DOC is the function's
documentation string.  PROMPT describes the scope of the query.
FLAGS are the command-line arguments passed to the 'fd'
executable, each of which is a string."
    `(defun ,name (&optional arg)
       ,doc
       (interactive "P")
       (let* ((vc (vc-root-dir))
              (dir (expand-file-name (if vc vc default-directory)))
              (regexp (read-regexp
                       (format "%s matching REGEXP in %s: " ,prompt
                               (propertize dir 'face 'bold))))
              (names (process-lines "fd" ,@flags regexp dir))
              (buf "*FD Dired*"))
         (if names
             (if arg
                 (dired (cons (generate-new-buffer-name buf) names))
               (icomplete-vertical-do ()
                 (find-file
                  (completing-read (format "Items matching %s (%s): "
                                           (propertize regexp 'face 'success)
                                           (length names))
                                   names nil t)))))
         (user-error (format "No matches for « %s » in %s" regexp dir)))))

  (cf/dired-fd
   cf/dired-fd-dirs
   "Search for directories in VC root or PWD.
With \\[universal-argument] put the results in a `dired' buffer.
This relies on the external 'fd' executable."
   "Subdirectories"
   "-i" "-H" "-a" "-t" "d" "-c" "never")

  (cf/dired-fd
   cf/dired-fd-files-and-dirs
   "Search for files and directories in VC root or PWD.
With \\[universal-argument] put the results in a `dired' buffer.
This relies on the external 'fd' executable."
   "Files and dirs"
    "-i" "-H" "-a" "-t" "d" "-t" "f" "-c" "never")

  :bind (("M-s d" .  cf/dired-fd-dirs)
         ("M-s z" . cf/dired-fd-files-and-dirs)
         :map dired-mode-map
         ("C-+" . dired-create-empty-file)
         ("M-s f" . nil)))

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48)

  (defun cf/buffers-major-mode (&optional arg)
    "Select buffers that match the current buffer's major mode.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion."
    (interactive "P")
    (let* ((major major-mode)
           (prompt "Buffers for ")
           (mode-string (format "%s" major))
           (mode-string-pretty (propertize mode-string 'face 'success)))
      (if arg
          (ibuffer t (concat "*" prompt mode-string "*")
                   (list (cons 'used-mode major)))
        (switch-to-buffer
         (read-buffer
          (concat prompt mode-string-pretty ": ") nil t
          (lambda (pair) ; pair is (name-string . buffer-object)
            (with-current-buffer (cdr pair) (derived-mode-p major))))))))

  (defun cf/buffers-vc-root (&optional arg)
    "Select buffers that match the present `vc-root-dir'.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion.

When no VC root is available, use standard `switch-to-buffer'."
    (interactive "P")
    (let* ((root (vc-root-dir))
           (prompt "Buffers for VC ")
           (vc-string (format "%s" root))
           (vc-string-pretty (propertize vc-string 'face 'success)))
      (if root
          (if arg
              (ibuffer t (concat "*" prompt vc-string "*")
                       (list (cons 'filename (expand-file-name root))))
            (switch-to-buffer
             (read-buffer
              (concat prompt vc-string-pretty ": ") nil t
              (lambda (pair) ; pair is (name-string . buffer-object)
                (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
        (call-interactively 'switch-to-buffer))))

  :hook (ibuffer-mode-hook . hl-line-mode)
  :bind (("M-s b" . cf/buffers-major-mode)
         ("M-s v" . cf/buffers-vc-root)
         ("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("* f" . ibuffer-mark-by-file-name-regexp)
         ("* g" . ibuffer-mark-by-content-regexp) ; "g" is for "grep"
         ("* n" . ibuffer-mark-by-name-regexp)
         ("s n" . ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
         ("/ g" . ibuffer-filter-by-content)))



;;; _
(provide 'cf-custom-functions)
;;; cf-custom-functions.el ends here

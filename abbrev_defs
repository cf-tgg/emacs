;;-*-coding: utf-8;-*-
(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("elhd" "" abrv/insert-elisp-header :count 0)
   ))

(define-abbrev-table 'global-abbrev-table
  '(
    ("CC" "" abrv/c-compile-command-var :count 0)
    ("LV" "" abrv/local-variables :count 2)
    ("MM" "" abrv/major-mode-prop-line :count 0)
    ("cfv" "" abrv/c-file-local-variables :count 0)
    ("elhd" "" abrv/elisp-header :count 5)
    ("fdv" "" abrv/file-local-variables :count 1)
    ("flv" "" abrv/file-local-variables :count 3)
    ("lxb" "" abrv/lexical-binding :count 0)
    ("m3u" "" abrv/hls-playlist-header :count 0)
    ("mdt" "" abrv/current-date-time :count 0)
    ("mtt" "" abrv/current-time :count 0)
    ("orghd" "" abrv/insert-org-header :count 0)
   ))


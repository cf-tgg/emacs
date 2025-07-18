;;; sixel.el --- minor mode for processing sixel graphics

;; Copyright (C) 2019 Tim Vaughan

;; Author: Tim Vaughan <plugd@thelambdalab.xyz>
;; Created: 19 May 2019
;; Version: 1.0.0
;; Keywords:
;; Homepage: gopher://thelambdalab.xyz/1/projects/sixel
;; Package-Requires: ((emacs "26"))

;;; Commentary:

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar test-string
  (concat "Pq"
          "#0;2;0;0;0#1;2;100;100;0#2;2;0;100;0"
          "#1~~@@vv@@~~@@~~$"
          "#2??}}GG}}??}}??-"
          "#1!14@\\"))

(defun sixel-get-params (string)
  "Retrieve the sixel parameters."
  (car (split-string (substring string 2) "q")))

(defun sixel-get-data (string)
  "Retrieve data string."
  (substring string (1+ (string-match "q" string))))

(defun sixel-tag-bits (sixel n tag)
  "Set bits of SIXEL corresponding to N with to the value TAG."
  (dotimes (i 6)
    (if (= (% n 2) 1)
        (aset sixel i tag))
    (setq n (/ n 2))))

(defun sixel-tag-sixel-in-row (row index char tag)
  "Tag the bits of the sixel at INDEX in the list identified by
the variable ROW-VARIABLE corresponding to input character CHAR
with TAG."
  (while (not (< index (length row)))
    (push (make-vector 6 nil) row))
  (let ((sixel (elt row (- (length row) 1 index))))
    (sixel-tag-bits sixel (- char 63) tag))
  row)

(defun sixel-process-data (string)
  "Convert STRING into a list of lists representing individual sixels.
Returns a sixel image object."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((idx-out 0)
          this-row rows
          current-colour colour-map
          finished)
      (while  (not finished)
        (cond
         ;; Define colour:
         ((looking-at "#\\([0-9]+\\);\\([0-9]+\\);\\([0-9]+\\);\\([0-9]+\\);\\([0-9]+\\)")
          (let ((tag (format "%02x" (string-to-number (match-string 1))))
                (mode (match-string 2))
                (r (string-to-number (match-string 3)))
                (g (string-to-number (match-string 4)))
                (b (string-to-number (match-string 5))))
            (push (list tag r g b) colour-map)))
         ;; Set current colour:
         ((looking-at "#\\([0-9]+\\)")
          (let ((tag (format "%02x" (string-to-number (match-string 1)))))
            (setq current-colour tag)))
         ;; Carriage return:
         ((looking-at "\\$")
          (setq idx-out 0))
         ;; New line:
         ((looking-at "-")
          (push (reverse this-row) rows)
          (setq this-row nil)
          (setq idx-out 0))
         ;; RLE sixel char sequence:
         ((looking-at "!\\([0-9]+\\)\\([?-~]\\)")
          (let ((repeat-count (string-to-number (match-string 1)))
                (char (elt (match-string 2) 0)))
            (dotimes (i repeat-count)
              (setq this-row
                    (sixel-tag-sixel-in-row this-row idx-out char current-colour))
              (setq idx-out (1+ idx-out)))))
         ;; Sixel char:
         ((looking-at "\\([?-~]\\)") ; Sixel char
          (let ((char (elt (match-string 1) 0)))
            (setq this-row
                  (sixel-tag-sixel-in-row this-row idx-out char current-colour))
            (setq idx-out (1+ idx-out))))
         ;; Termination sequence:
         ((looking-at "\\\\")
          (setq finished t))
         ;; Skip other char:
         ((looking-at "[[:ascii:]]")))
        (goto-char (match-end 0)))
      (push (reverse this-row) rows)
      (cons colour-map
            (reverse rows)))))

(defun sixel-pad-rows (sixel-image)
  "Pad out contents of rows in SIXEL-IMAGE so that all rows are the same length."
  (let ((width (car (sixel-image-dims sixel-image)))
        (rows (cdr sixel-image)))
    (dotimes (row-idx (length rows))
      (let* ((row-cdr (nthcdr row-idx rows))
             (row-width (length (car row-cdr))))
        (if (< row-width width)
            (setcar row-cdr (append (car row-cdr)
                                    (make-list (- width row-width)
                                               [nil nil nil nil nil nil])))))))
  sixel-image)

(defun sixel-image-colour-map (sixel-image)
  "Extract colour map from SIXEL-IMAGE."
  (car sixel-image))

(defun sixel-image-sixels (sixel-image)
  "Extract sixels from SIXEL-IMAGE."
  (cdr sixel-image))

(defun sixel-image-dims (sixel-image)
  "Compute image width from SIXEL-IMAGE.  Return pair (width . height)."
  (let ((sixels (sixel-image-sixels sixel-image)))
    (cons
     (apply #'max (mapcar (lambda (row) (length row)) sixels))
     (* 6 (length sixels)))))

(defun sixel-image-to-xpm-values (sixel-image)
  "Produce parameter values component of XPM representation of SIXEL-IMAGE."
  (let* ((dims (sixel-image-dims sixel-image))
         (colour-map (sixel-image-colour-map sixel-image))
         (n-colours (1+ (length colour-map))))
    (concat "\""
            (number-to-string (car dims)) " "
            (number-to-string (cdr dims)) " "
            (number-to-string n-colours) " 2\"")))

(defun sixel-image-to-xpm-colours (sixel-image)
  "Produce colour definitions component of XPM representation of SIXEL-IMAGE."
  (let ((colour-map (sixel-image-colour-map sixel-image)))
    (concat
     (string-join
      (mapcar (lambda (colour)
                (concat
                 "\""
                 (elt colour 0) " "
                 "c #"
                 (format "%02x%02x%02x"
                         (/ (* 255 (elt colour 1)) 100)
                         (/ (* 255 (elt colour 2)) 100)
                         (/ (* 255 (elt colour 3)) 100))
                 "\""))
              colour-map)
      ",\n")
     ",\n"
     "\"-- c #000000\"")))

(defun sixel-image-to-xpm-pixels (sixel-image)
  "Produce pixels component of XPM representation of SIXEL-IMAGE."
  (concat
   "\""
   (string-join
    (mapcar (lambda (sixel-row)
              (string-join
               (mapcar (lambda (i)
                         (string-join
                          (mapcar (lambda (sixel)
                                    (let ((pixel (elt sixel i)))
                                      (if pixel
                                          pixel
                                        "--")))
                                  sixel-row)))
                       (number-sequence 0 5))
               "\",\n\""))
            (sixel-image-sixels sixel-image))
    "\",\n\"")
   "\""))

(defun sixel-to-xpm (string)
  "Return an XPM image representation of the SIXEL graphic encoded in STRING."
  (let* ((param-string (sixel-get-params string))
         (data-string (sixel-get-data string))
         (sixel-image (sixel-pad-rows (sixel-process-data data-string))))
    (if (string-prefix-p "P" string)
        (concat
         "/* XPM */"
         "static char * pixmap[] = {"
         (sixel-image-to-xpm-values sixel-image) ",\n"
         (sixel-image-to-xpm-colours sixel-image) ",\n"
         (sixel-image-to-xpm-pixels sixel-image) "};")
      (error "Incorrecly formatted sixel string"))))

(defun sixel-render-images-in-buffer ()
  "Find and render any sixel images in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "P[[:ascii:]]*\\\\" nil t)
      (let ((sixel-string (match-string 0))
            (inhibit-read-only t))
        (delete-region (match-beginning 0)
                       (match-end 0))
        (insert-image
         (create-image (sixel-to-xpm sixel-string) 'xpm t))
        (insert "\n")))))

(defgroup sixel nil
  "Render sixel images."
  :group 'multimedia)

(define-minor-mode sixel-mode
  "A minor mode which renders sixel graphics." nil "sixel" nil
  (add-hook 'after-change-functions
            (lambda (start end size)
              (sixel-render-images-in-buffer)
              (message "Render complete."))
            nil t))


;;; _
(provide 'sixel)
;;; sixel.el ends here

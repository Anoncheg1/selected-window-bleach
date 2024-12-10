;;; selected-window-bleach.el --- Accurate reduce contrast of non selected windows.   -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg

;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords:  comparing, merging, patching, vc, tools, unix
;; URL: https://codeberg.org/Anoncheg/diffnw
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Usage:
;; (add-hook 'buffer-list-update-hook 'selected-window-bleach-highlight-selected-window)
;; To increase contrast of selected modeline:
;; (selected-window-bleach-change-modeline 0.7 0.7)
;; Customize:
;;; Code:

;; - configurable:
(defcustom selected-window-bleach-selected-magnitude-text 1
  "Change in contrast for selected window.
Higher value decrease contrast between text and background.
This value change contrast of text regarding to background.")
(defcustom selected-window-bleach-selected-magnitude-background 1
  ".")
(defcustom selected-window-bleach-not-sel-magnitude-text 1.2
  "Change in contrast for not selected window.")
(defcustom selected-window-bleach-not-sel-magnitude-background 1.2
  "This value change contrast of background regarding to text.")


(defun selected-window-bleach--get-current-colors ()
  "Get the current text and background colors."
  (let ((text-color (face-attribute 'default :foreground (selected-frame)))
        (background-color (face-attribute 'default :background (selected-frame))))
    (list text-color background-color)))

(defun selected-window-bleach--hex-to-hsl (hex)
  "Convert hex to RGB. Generated with AI."
  (let ((rgb (color-name-to-rgb hex)))
    (color-rgb-to-hsl (/ (nth 0 rgb) 65535.0)
                      (/ (nth 1 rgb) 65535.0)
                      (/ (nth 2 rgb) 65535.0))))

(defun selected-window-bleach--adjust-brightness (text-color background-color magnitude-text magnitude-back)
  "Adjust the brightness of the text and background colors.
To be closer By the magnitude.
Return (foreground , background)."
  (let ((text-hsl (selected-window-bleach--hex-to-hsl text-color))
        (background-hsl (selected-window-bleach--hex-to-hsl background-color)))
    (let ((text-brightness (nth 2 text-hsl))
          (background-brightness (nth 2 background-hsl)))
      ;; new-text-brightness = average + (average - background) / magnitude-text
      ;; new-back-brightness = average + (average - text) / magnitude-back
      (let ((average-brightness (/ (+ text-brightness background-brightness) 2)))
        (let ((new-text-brightness (+ average-brightness (/ (- average-brightness background-brightness)
                                                            magnitude-text)))
              (new-background-brightness (- average-brightness (/ (- text-brightness average-brightness)
                                                                  magnitude-back))))
          (list (color-hsl-to-rgb (nth 0 text-hsl)
                                  (nth 1 text-hsl)
                                  new-text-brightness)
                (color-hsl-to-rgb (nth 0 background-hsl)
                                  (nth 1 background-hsl)
                                  new-background-brightness)))))))

(defun selected-window-bleach--rgb-to-hex (red green blue)
  "Convert RGB to hex."
  (color-rgb-to-hex (* red 65535) (* green 65535) (* blue 65535)))

(defun selected-window-bleach--apply-new-colors (text-color background-color)
  "Apply the new text and background colors."
  (let ((fg (apply 'selected-window-bleach--rgb-to-hex text-color))
        (bg (apply 'selected-window-bleach--rgb-to-hex background-color)))
    (buffer-face-set (list :background bg :foreground fg))
    ))

(defun selected-window-bleach-change (magnitude-text magnitude-back)
  "Adjust the text and background colors to be closer in brightness."
  (let* ((current-colors (selected-window-bleach--get-current-colors))
        (new-colors (selected-window-bleach--adjust-brightness (nth 0 current-colors)
                                                               (nth 1 current-colors)
                                                               magnitude-text
                                                               magnitude-back)))
    (selected-window-bleach--apply-new-colors (nth 0 new-colors)
                                              (nth 1 new-colors))))

(defun selected-window-bleach-change-modeline (magnitude-text magnitude-back)
  "Adjust modeline brightness of text and background."
  (let* ((backgound (face-attribute 'mode-line :background))
         (foreground (face-attribute 'mode-line :foreground))
         (new-colors (selected-window-bleach--adjust-brightness foreground
                                                                backgound
                                                                magnitude-text
                                                                magnitude-back))
         (new-foreground (apply 'selected-window-bleach--rgb-to-hex (nth 0 new-colors)))
         (new-background (apply 'selected-window-bleach--rgb-to-hex (nth 1 new-colors))))
    (set-face-attribute 'mode-line-active nil
                        :foreground new-foreground
                        :background new-background)))


(defun selected-window-bleach-highlight-selected-window ()
  "Highlight not selected windows with a different background color."
  (let ((cbn (buffer-name (current-buffer)))
        (sw (selected-window)))
    (when (/= (aref cbn 0) ?\s) ; ignore system buffers
      ;; - not selected:
      (walk-windows (lambda (w)
                      (unless (or (eq sw w)
                                  (eq cbn  (buffer-name (window-buffer w))))
                        (with-selected-window w
                          (if (not (and (= selected-window-bleach-selected-magnitude-text 1)
                                        (= selected-window-bleach-selected-magnitude-background 1)))
                              (selected-window-bleach-change
                               selected-window-bleach-not-sel-magnitude-text
                               selected-window-bleach-not-sel-magnitude-background)
                            ;; else
                            (buffer-face-set 'default))))))

      ;; - selected:
      (if (not (and (= selected-window-bleach-selected-magnitude-text 1)
                    (= selected-window-bleach-selected-magnitude-background 1)))
          (progn
            (selected-window-bleach-change
             selected-window-bleach-selected-magnitude-text
             selected-window-bleach-selected-magnitude-background)

            (set-face-attribute 'mode-line-active nil :background "cyan4"))
        ;; else
        (buffer-face-set 'default)
        ))))

(provide 'selected-window-bleach)
;;; selected-window-bleach.el ends here

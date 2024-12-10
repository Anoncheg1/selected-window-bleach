;;; selected-window-bleach.el --- Not selected windows shadowed by reducing difference between foreground and background.   -*- lexical-binding: t -*-

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
;; Recommended:
;; (set-face-attribute 'mode-line-active nil :background "cyan4")
;;; Code:

(defun selected-window-bleach--get-current-colors ()
  "Get the current text and background colors."
  (let ((text-color (face-attribute 'default :foreground (selected-frame)))
        (background-color (face-attribute 'default :background (selected-frame))))
    (list text-color background-color)))

(defun selected-window-bleach--hex-to-rgb (hex)
  "Convert hex to RGB. Generated with AI."
  (let ((rgb (color-name-to-rgb hex)))
    (list (/ (nth 0 rgb) 65535.0) (/ (nth 1 rgb) 65535.0) (/ (nth 2 rgb) 65535.0))))

(defvar selected-window-bleach-change-magnitude-text 1.2)
(defvar selected-window-bleach-change-magnitude-background 1.2)

(defun selected-window-bleach--adjust-brightness (text-color background-color)
  "Adjust the brightness of the text and background colors to be closer.
Generated with AI."
  (let ((text-hsl (color-rgb-to-hsl
                   (nth 0 (selected-window-bleach--hex-to-rgb text-color))
                   (nth 1 (selected-window-bleach--hex-to-rgb text-color))
                   (nth 2 (selected-window-bleach--hex-to-rgb text-color))))
        (background-hsl (color-rgb-to-hsl
                         (nth 0 (selected-window-bleach--hex-to-rgb background-color))
                         (nth 1 (selected-window-bleach--hex-to-rgb background-color))
                         (nth 2 (selected-window-bleach--hex-to-rgb background-color)))))
    (let ((text-brightness (nth 2 text-hsl))
          (background-brightness (nth 2 background-hsl)))
      (let ((average-brightness (/ (+ text-brightness background-brightness) 2)))
        (let ((new-text-brightness (+ average-brightness (/ (- average-brightness background-brightness) selected-window-bleach-change-magnitude-text)))
              (new-background-brightness (- average-brightness (/ (- text-brightness average-brightness) selected-window-bleach-change-magnitude-background))))
          (list (color-hsl-to-rgb (nth 0 text-hsl) (nth 1 text-hsl) new-text-brightness)
                (color-hsl-to-rgb (nth 0 background-hsl) (nth 1 background-hsl) new-background-brightness)))))))

(defun selected-window-bleach--rgb-to-hex (red green blue)
  "Convert RGB to hex."
  (color-rgb-to-hex (* red 65535) (* green 65535) (* blue 65535)))

(defun selected-window-bleach--apply-new-colors (text-color background-color)
  "Apply the new text and background colors."
  (let ((fg (selected-window-bleach--rgb-to-hex (nth 0 text-color) (nth 1 text-color) (nth 2 text-color)))
        (bg (selected-window-bleach--rgb-to-hex (nth 0 background-color) (nth 1 background-color) (nth 2 background-color))))
    (buffer-face-set (list :background bg :foreground fg))
    ))

(defun selected-window-bleach-adjust-colors ()
  "Adjust the text and background colors to be closer in brightness."
  (let* ((current-colors (selected-window-bleach--get-current-colors))
        (new-colors (selected-window-bleach--adjust-brightness (nth 0 current-colors) (nth 1 current-colors))))
    (selected-window-bleach--apply-new-colors (nth 0 new-colors) (nth 1 new-colors))))


(defun selected-window-bleach-highlight-selected-window ()
  "Highlight selected window with a different background color."
  (let ((cbn (buffer-name (current-buffer)))
        (sw (selected-window)))
    (when (/= (aref cbn 0) ?\s) ; ignore system buffers
      (walk-windows (lambda (w)
                      (unless (or (eq sw w)
                                  (eq cbn  (buffer-name (window-buffer w))))
                        (with-selected-window w
                          ;; (print cbn)
                          (selected-window-bleach-adjust-colors)))))
      (buffer-face-set 'default))))
(provide 'selected-window-bleach)
;;; selected-window-bleach.el ends here

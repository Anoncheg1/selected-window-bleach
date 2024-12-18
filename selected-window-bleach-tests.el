;;; selected-window-bleach-tests.el --- Accurate reduce contrast of non selected windows.   -*- lexical-binding: t -*-

(require 'ert)
(require 'selected-window-bleach)

(ert-deftest selected-window-bleach-tests--a1 ()
  (with-temp-buffer
    (should (listp (selected-window-bleach--get-current-colors)))
    (should (equal (length (selected-window-bleach--get-current-colors)) 2))
    )
)



(ert-deftest selected-window-bleach-tests--a2 ()
  (with-temp-buffer
    (let* ((text-color "white")
          (background-color "black")
          (magnitude-text 1.2)
          (magnitude-back 1.2)
          (text-hsl (selected-window-bleach--hex-to-hsl text-color))
          (background-hsl (selected-window-bleach--hex-to-hsl background-color))
          (text-brightness (nth 2 text-hsl))
          (background-brightness (nth 2 background-hsl))
          (average-brightness (/ (+ text-brightness background-brightness) 2))
          (new-text-brightness (+ average-brightness (/ (- average-brightness background-brightness)
                                                        magnitude-text)))
          (new-background-brightness (- average-brightness (/ (- text-brightness average-brightness)
                                                              magnitude-back))))
      (should (<= 0 new-text-brightness 1))
      (should (<= 0 new-background-brightness 1))
      (should (<= 0 (nth 0 (color-hsl-to-rgb (nth 0 text-hsl) (nth 1 text-hsl) new-text-brightness)) 1))
      (should (<= 0 (nth 1 (color-hsl-to-rgb (nth 0 text-hsl) (nth 1 text-hsl) new-text-brightness)) 1))
      (should (<= 0 (nth 2 (color-hsl-to-rgb (nth 0 text-hsl) (nth 1 text-hsl) new-text-brightness)) 1))
      (should (<= 0 (nth 0 (color-hsl-to-rgb (nth 0 background-hsl) (nth 1 background-hsl) new-background-brightness)) 1))
      (should (<= 0 (nth 1 (color-hsl-to-rgb (nth 0 background-hsl) (nth 1 background-hsl) new-background-brightness)) 1))
      (should (<= 0 (nth 2 (color-hsl-to-rgb (nth 0 background-hsl) (nth 1 background-hsl) new-background-brightness)) 1)))
    )
)

(ert-deftest selected-window-bleach-tests--a3 ()
  (with-temp-buffer
    (should (listp (selected-window-bleach--adjust-brightness "white" "black" 1.2 1.2)))
    (should (equal (length (selected-window-bleach--adjust-brightness "white" "black" 1.2 1.2)) 2))

    (should (stringp (selected-window-bleach--rgb-to-hex 1.0 1.0 1.0)))

    (let ((red 0.5)
          (green 0.5)
          (blue 0.5))
      (should (stringp (selected-window-bleach--rgb-to-hex red green blue))))

    (should (equal t (selected-window-bleach-change 1.2 1.2)))
    )
)

;; (ert-deftest selected-window-bleach-tests--a4 ()
;;   (with-temp-buffer

;;     (let ((text-color "white")
;;           (background-color "black"))
;;       (print (selected-window-bleach--adjust-brightness text-color background-color))
;;       (print (selected-window-bleach--adjust-brightness-2 text-color background-color))
;;       ;; (should (equal (selected-window-bleach--adjust-brightness text-color background-color)
;;       ;;                (selected-window-bleach--adjust-brightness-2 text-color background-color))))
;;     )
;; )

(provide 'selected-window-bleach-tests)
;;; selected-window-bleach-tests.el ends here

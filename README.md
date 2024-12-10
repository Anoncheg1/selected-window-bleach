# selected-window-bleach
Change background of not selected windows by shadowing them by calculating difference between foreground text and background brightness.

Allow to adjust brightness of text and background for selected and not selected window.

Allow to increase constrast between text and background.

# Usage
```Elisp
(add-hook 'buffer-list-update-hook 'selected-window-bleach-highlight-selected-window)
```

Recommended: ```(set-face-attribute 'mode-line-active nil :background "cyan4")```

# Original idea:
```Elisp
(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "#111"))))))
  (buffer-face-set 'default))
(add-hook 'buffer-list-update-hook 'highlight-selected-window)
```
from https://emacs.stackexchange.com/questions/24630/is-there-a-way-to-change-color-of-active-windows-fringe

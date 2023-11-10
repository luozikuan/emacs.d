;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

;; Navigate window layouts with "C-c <left>" and "C-c <right>"

(add-hook 'after-init-hook 'winner-mode)


;; Make "C-x o" prompt for a target window when there are more than 2
(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'qwerty)
(setq-default switch-window-qwerty-shortcuts '("a" "r" "s" "t" "n" "e" "i" "o" "d" "h" "q" "w" "f" "p" "g" "j" "l" "u" "y" ";" "z" "x" "c" "v" "b" "k" "m")) ;; change to cokemak layout
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)



;; When splitting window, show (other-buffer) in the new window

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))


;; use shift + arrow keys to switch between visible buffers
(require-package 'windmove)
(windmove-default-keybindings 'control)


(provide 'init-windows)
;;; init-windows.el ends here

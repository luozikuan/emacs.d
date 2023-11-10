;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'flymake "1.2.1")

(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'text-mode-hook 'flymake-mode)

(with-eval-after-load 'flymake
  ;; Provide some flycheck-like bindings in flymake mode to ease transition
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! c") 'flymake-start))

(setq eldoc-documentation-function #'eldoc-documentation-compose)
(add-hook 'flymake-mode-hook
          (lambda ()
            (add-hook 'eldoc-documentation-functions 'flymake-eldoc-function nil t)))


(provide 'init-flymake)
;;; init-flymake.el ends here

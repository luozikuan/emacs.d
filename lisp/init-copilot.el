;;; init-copilot.el --- Enable github copilot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; github copilot https://github.com/zerolfx/copilot.el
(when (and (executable-find "node")
           (maybe-require-package 'dash)
           (maybe-require-package 's)
           (maybe-require-package 'editorconfig)
           (require 'copilot nil t))
  (dolist (mode '(c++-mode python-mode gfm-mode))
    (add-hook (derived-mode-hook-name mode) 'copilot-mode))
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-[") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "M-]") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "M-f") 'copilot-accept-completion-by-word))


(provide 'init-copilot)
;;; init-copilot.el ends here

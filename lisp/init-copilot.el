;;; init-copilot.el --- Enable github copilot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; github copilot https://github.com/copilot-emacs/copilot.el
(when (and (executable-find "node")
           (maybe-require-package 'dash)
           (maybe-require-package 'f)
           (maybe-require-package 's)
           (maybe-require-package 'editorconfig)
           (maybe-require-package 'jsonrpc "1.0.23")
           (require 'copilot nil t))
  (dolist (mode '(c++-mode python-mode gfm-mode sh-mode))
    (add-hook (derived-mode-hook-name mode) 'copilot-mode))
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-[") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "M-]") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "M-f") 'copilot-accept-completion-by-word)

  (setq warning-suppress-log-types '((copilot copilot-exceeds-max-char))))


(provide 'init-copilot)
;;; init-copilot.el ends here

;;; init-copilot.el --- Enable github copilot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; only use copilot when node is installed
(when (executable-find "node")
  (use-package copilot
    :ensure t
    :hook (prog-mode . copilot-mode)
    :config
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "M-[") 'copilot-previous-completion)
    (define-key copilot-completion-map (kbd "M-]") 'copilot-next-completion)
    (define-key copilot-completion-map (kbd "M-f") 'copilot-accept-completion-by-word)

    (setq copilot-max-char-warning-disable t
          copilot-indent-offset-warning-disable t)
    ))


(provide 'init-copilot)
;;; init-copilot.el ends here

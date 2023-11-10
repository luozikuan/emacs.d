;;; init-prog-common.el --- Configures for programming -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'derived)

;; breadcrumb for project
(when (maybe-require-package 'breadcrumb)
  (dolist (mode '(c++-ts-mode python-ts-mode org-mode markdown-mode))
    (add-hook (derived-mode-hook-name mode) #'breadcrumb-local-mode)))

(add-hook 'prog-mode-hook #'hl-line-mode)


(provide 'init-prog-common)
;;; init-prog-common.el ends here

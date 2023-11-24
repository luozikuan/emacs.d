;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'gfm-mode)))

(when (executable-find "code")
  (defun markdown-preview-in-vscode ()
    "Open the current file in Visual Studio Code."
    (interactive)
    (shell-command (concat "code \"" buffer-file-name "\"")))

  (eval-after-load 'markdown-mode
    '(define-key gfm-mode-map (kbd "C-S-v") 'markdown-preview-in-vscode)))

(when (maybe-require-package 'markdown-toc)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (markdown-toc-mode +1))))

(provide 'init-markdown)
;;; init-markdown.el ends here

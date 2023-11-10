;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'gfm-mode)))

(defun markdown-preview-in-vscode ()
  "Open the current file in Visual Studio Code."
  (interactive)
  ;; (message (concat "code \"" buffer-file-name "\""))
  (shell-command (concat "code \"" buffer-file-name "\"")))

(eval-after-load 'markdown-mode
  '(define-key gfm-mode-map (kbd "C-S-v") 'markdown-preview-in-vscode))


(provide 'init-markdown)
;;; init-markdown.el ends here

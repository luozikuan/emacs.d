;;; init-claude-code.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup) ; Optionally enable Emacs MCP tools
  (setq claude-code-ide-terminal-backend 'eat) ; Use eat instead of vterm
  )

(provide 'init-claude-code)
;;; init-claude-code.el ends here

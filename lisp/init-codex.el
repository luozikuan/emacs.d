;;; init-codex.el --- OpenAI codex config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package codex-ide
  :vc (:url "https://github.com/dgillis/emacs-codex-ide" :rev :newest)
  :bind ("C-c C-;" . codex-ide-menu))

(provide 'init-codex)
;;; init-codex.el ends here

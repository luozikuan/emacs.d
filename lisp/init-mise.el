;;; init-mise.el --- Integrate with mise -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (and (executable-find "mise")
		   (maybe-require-package 'mise))
  (add-hook 'after-init-hook #'global-mise-mode))

(provide 'init-mise)
;;; init-mise.el ends here

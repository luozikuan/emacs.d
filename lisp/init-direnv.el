;;; init-direnv.el --- Integrate with direnv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (and (executable-find "direnv")
           (maybe-require-package 'envrc))

  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))

  (setq envrc-error-lighter '(" " (:propertize "envrc" face envrc-mode-line-error-face))
        envrc-none-lighter nil
        envrc-on-lighter '(" " (:propertize "envrc" face envrc-mode-line-on-face)))

  (add-hook 'after-init-hook 'envrc-global-mode))

(provide 'init-direnv)

;;; init-direnv.el ends here

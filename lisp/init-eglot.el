;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)

  (with-eval-after-load 'eglot
    ;; (setq eglot-prefer-plaintext t)
    (add-to-list 'eglot-server-programs
                 '((c-mode c-ts-mode c++-mode c++-ts-mode)
                   . ("clangd"
                      "--header-insertion=never")))))


(provide 'init-eglot)
;;; init-eglot.el ends here

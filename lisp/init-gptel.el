;;; init-gptel.el --- ai assistant -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'gptel)
  ;; OPTIONAL configuration
  (setq gptel-model 'claude-3.7-sonnet
        gptel-backend (gptel-make-gh-copilot "Copilot")))

(provide 'init-gptel)
;;; init-gptel.el ends here

;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(when (maybe-require-package 'orderless)
  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic))))
(setq completion-category-defaults nil
      completion-category-overrides nil)
(setq completion-cycle-threshold 4)

(when (maybe-require-package 'corfu)
  (setq-default corfu-auto t)
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (setq-default corfu-quit-no-match 'separator)
  (add-hook 'after-init-hook 'global-corfu-mode)



  (with-eval-after-load 'corfu
    (corfu-popupinfo-mode))

  ;; Make Corfu also work in terminals, without disturbing usual behaviour in GUI
  (when (and (not (display-graphic-p))
             (maybe-require-package 'corfu-terminal))
    (with-eval-after-load 'corfu
      (corfu-terminal-mode)))

  (when (maybe-require-package 'kind-icon)
    (with-eval-after-load 'corfu
      (setq-default kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))
  )


(provide 'init-corfu)
;;; init-corfu.el ends here

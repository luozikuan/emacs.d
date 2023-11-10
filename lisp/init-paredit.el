;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'paredit)

(with-eval-after-load 'paredit
  (diminish 'paredit-mode " Par")
  ;; Suppress certain paredit keybindings to avoid clashes, including
  ;; my global binding of M-?
  (dolist (binding '("RET" "C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil))
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-splice-sexp-killing-backward))


(provide 'init-paredit)
;;; init-paredit.el ends here

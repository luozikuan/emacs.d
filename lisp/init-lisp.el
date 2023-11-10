;;; init-lisp.el --- Emacs lisp settings, and common config for other lisps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default debugger-bury-or-kill 'kill)

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))

(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(defun headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert "(provide '" (file-name-sans-extension fname) ")\n")
      (insert ";;; " fname " ends here\n"))))


;; Load .el if newer than corresponding .elc

(setq load-prefer-newer t)


(defun enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar lispy-modes-hook
  '(enable-paredit-mode
    enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(require 'derived)

(dolist (mode '(emacs-lisp-mode lisp-mode lisp-interaction-mode))
  (add-hook (derived-mode-hook-name mode) (lambda () (run-hooks 'lispy-modes-hook))))


(provide 'init-lisp)
;;; init-lisp.el ends here

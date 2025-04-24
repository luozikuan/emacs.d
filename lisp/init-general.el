;;; init-general.el --- leader key settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package general
  :ensure t)

(general-create-definer my/leader-def
  :prefix "M-m"
  :keymaps 'override)

(general-create-definer my/local-leader-def
  :prefix "M-RET"
  :keymaps 'override)

;; Global bindings (M-m)
(my/leader-def
  "f"  '(:ignore t :wk "files")
  "f f" 'find-file
  "f s" 'save-buffer
  "f r" 'revert-buffer
  "f e" '((lambda ()
			(interactive)
			(find-file (expand-file-name "init.el" user-emacs-directory)))
		  :wk "emacs config")

  "b"  '(:ignore t :wk "buffers")
  "b b" 'switch-to-buffer
  "b B" 'ibuffer
  "b k" 'kill-buffer
  "b s" 'scratch-buffer
  "b d" 'dashboard-open
  "b r" 'revert-buffer

  "g"  '(:ignore t :wk "git")
  "g g" 'magit-status
  "g s" 'magit-status
  "g l" 'magit-list-repositories
  "g f" 'magit-file-dispatch

  "j" '(:ignore t :wk "jump")
  "j j" 'avy-goto-char-timer
  "j l" 'consult-goto-line

  "p" '(:keymap projectile-command-map :wk "projectile" :package projectile)

  "s" '(:ignore t :wk "search")
  "s s" 'isearch-forward
  "s S" 'isearch-forward-regexp
  "s h" 'anzu-query-replace

  "t"  '(:ignore t :wk "tools")
  "t t" 'eat-other-window

  "w"  '(:ignore t :wk "windows")
  "w m" 'toggle-frame-maximized
  "w s" 'window-swap-states
  "w k" 'kill-buffer-and-window
  "w o" 'switch-window
  "w /" 'split-window-horizontally-instead
  "w -" 'split-window-vertically-instead

  "u" 'universal-argument
  "M-m" 'back-to-indentation)


(provide 'init-general)
;;; init-general.el ends here

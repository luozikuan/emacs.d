;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

(when (maybe-require-package 'diff-hl)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-init-hook 'global-diff-hl-mode)

  (with-eval-after-load 'diff-hl
    (define-key diff-hl-mode-map
                (kbd "<left-fringe> <mouse-1>")
                'diff-hl-diff-goto-hunk)))


(require-package 'git-modes)
(when (maybe-require-package 'git-timemachine)
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))

(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk 'all)
  (setq-default magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 12))
  (add-hook 'git-commit-setup-hook 'turn-off-auto-fill t)

  ;; Hint: customize `magit-repository-directories' so that you can use C-u C-x g to
  ;; quickly open magit on any one of your projects.
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch)
  )

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up))

(require-package 'fullframe)
(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))


(when sys/macp
  (with-eval-after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))


(provide 'init-git)
;;; init-git.el ends here

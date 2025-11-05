;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)


  (when (maybe-require-package 'consult)
	(defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
        (interactive (list prefix-arg
						   (if (use-region-p)
							   (buffer-substring-no-properties
								(region-beginning) (region-end))
							 (if-let ((s (symbol-at-point)))
                                 (symbol-name s)))))
        (consult-ripgrep dir initial))

    (when (executable-find "rg")
      (global-set-key (kbd "M-?") 'sanityinc/consult-ripgrep-at-point))

    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)
    )
  )

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; Misc config - yet to be placed in separate files
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))


(defun calc-eval-line-or-region ()
  "Evaluate math expr in line or the selected region."
  (interactive)
  (let ((expr (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'line t))))
    (message "Result: %s" (calc-eval expr))))


(provide 'init-misc)
;;; init-misc.el ends here

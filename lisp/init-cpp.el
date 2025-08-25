;;; init-cpp.el --- Settings for c++ -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'c-ts-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))

;; Use tabs, not spaces
(setq-default indent-tabs-mode t)

;; Visual width of a tab character
(setq-default tab-width 4)

;; Treesit C/C++ indent offset (logical indent level)
(setq c-ts-mode-indent-offset 4)
(setq c++-ts-mode-indent-offset 4)

;; Optional: tweak style if you dislike default brace/arg alignment
(defun my-cpp-indent-style ()
  "Base on BSD style, but keep tabs at width 4."
  `( ;; Align closing paren/braces with opening
    ;; ((node-is ")") parent-bol 0)
    ;; Function args: indent one level from opening paren
	((parent-is "argument_list") parent-bol c-ts-mode-indent-offset)
    ;; Parameters in decls
	((parent-is "parameter_list") parent-bol c-ts-mode-indent-offset)

	((node-is "field_initializer_list") standalone-parent c-ts-mode-indent-offset)
	((match "," "field_initializer_list") prev-line 0)

	((match "compound_statement" "for_range_loop") standalone-parent 0)
	((parent-is "for_range_loop") standalone-parent c-ts-mode-indent-offset)

	((parent-is "binary_expression") parent-bol c-ts-mode-indent-offset)
	((node-is "case") parent-bol 0)

	((parent-is "lambda_expression") parent-bol 0)

    ;; Append base BSD style
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(setq c-ts-mode-indent-style #'my-cpp-indent-style)
;; (setq treesit--indent-verbose t) ; debug treesit indent, hit <tab> to see matched rule

(defun my-c++-ts-mode-hook ()
  "Common setting for c++-ts-mode."
  (remove-hook 'flymake-mode-hook 'sanityinc/enable-flymake-flycheck)
  (display-fill-column-indicator-mode -1)
  (hs-minor-mode)
  ;; (origami-mode)
  ;; (which-function-mode)
  (local-set-key [mouse-8] 'xref-go-back)
  (local-set-key [mouse-9] 'xref-go-forward)
  (local-set-key (kbd "M-?") 'xref-find-references)
  (local-set-key (kbd "M-o") 'projectile-find-other-file)
  (local-set-key (kbd "C-c f") 'hs-toggle-hiding)
  )
(add-hook 'c++-ts-mode-hook #'my-c++-ts-mode-hook)

(provide 'init-cpp)
;;; init-cpp.el ends here

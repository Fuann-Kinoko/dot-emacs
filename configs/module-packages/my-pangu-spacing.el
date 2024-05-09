;;; configs/module-packages/my-pangu-spacing.el -*- lexical-binding: t; -*-

(use-package! pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  (add-hook 'org-mode-hook
            '(lambda ()
             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
    )

(provide 'my-pangu-spacing)

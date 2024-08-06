(defun auto-dark-banner-update (img-path)
  (progn
    ;; update banner
    (setq fancy-splash-image img-path)
    ;; reload doom dashboard if it is
    (when (eq major-mode '+doom-dashboard-mode)
        (+doom-dashboard/open (selected-frame)))))

(use-package! auto-dark
  :config
  (after! doom-ui
    ;; set your favorite themes
    (setq! auto-dark-dark-theme 'my-mountain
          auto-dark-light-theme 'doom-flatwhite)
    (auto-dark-mode 1)
    (add-hook 'auto-dark-dark-mode-hook
      (apply-partially #'auto-dark-banner-update "~/.config/doom/banner3.pbm"))
    (add-hook 'auto-dark-light-mode-hook
      (apply-partially #'auto-dark-banner-update "~/.config/doom/banner3_inv.pbm"))))

(provide 'my-auto-dark)

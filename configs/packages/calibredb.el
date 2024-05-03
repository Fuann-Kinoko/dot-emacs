;;; configs/packages/calibredb.el -*- lexical-binding: t; -*-

(defun calibredb-font-setup ()
  (setq buffer-face-mode-face '(:family "Sarasa Term SC" :height 200))
  (buffer-face-mode))



(use-package! calibredb
  :defer t
  :config
  (evil-set-initial-state 'calibredb-search-mode 'emacs)
  (evil-set-initial-state 'calibredb-show-mode 'emacs)

  (add-hook 'calibredb-search-mode-hook 'calibredb-font-setup)

  (setq calibredb-author-width 15)
  (setq calibredb-title-width 50)
  (setq calibredb-root-dir "~/.local/Calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Documents/book")
                                  ("~/Documents/Landing-Page"))))

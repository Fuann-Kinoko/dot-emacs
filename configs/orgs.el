;;; configs/orgs.el -*- lexical-binding: t; -*-


(setq org-directory "~/org/")

(setq org-link-search-must-match-exact-headline nil) ;; to enable linking words & bookmarks

(require 'org-download)
(require 'org-ros)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq org-image-actual-width 600)

(use-package deft
  :ensure t
  :defer t
  :config
  (progn
    (setq deft-extensions '("md" "markdown" "org" "txt"))
    (setq deft-directory "~/org/")
    (setq deft-file-naming-rules '((noslash . "-")
                                   (nospace . "-")
                                   (case-fn . downcase)))
    (setq deft-recursive t)
    (evil-define-key 'insert deft-mode-map (kbd "C-<backspace>") #'deft-filter-decrement-word)
    (evil-define-key 'insert deft-mode-map (kbd "C-w") #'deft-filter-decrement-word)
    (evil-define-key 'insert deft-mode-map (kbd "C-k") #'deft-filter-clear)))

(use-package! org-appear
  :init
  (setq org-hide-emphasis-markers t)
  :config
  (add-hook 'org-mode-hook 'org-appear-mode)
  ;; (setq org-appear-autolinks t)
  )

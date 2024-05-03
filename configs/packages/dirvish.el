;;; configs/packages/dirvish.el -*- lexical-binding: t; -*-
(use-package! dirvish
  :init
  (dirvish-override-dired-mode)
  (add-hook 'dired-mode-hook
    'auto-revert-mode)
  :config
  (setq dired-do-revert-buffer t) ;; refresh buffer automatically after do commands
  (setq dired-listing-switches "-ahl -v --group-directories-first --almost-all")

  (setq dirvish-attributes '(collapse file-size file-time))

  (setq dirvish-default-layout '(1 0.11 0.55))

  (setq dirvish-side-auto-close t)

  (setq dirvish-preview-dispatchers
      (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers))

  (dirvish-define-preview exa (file)
    "Use `exa' to generate directory preview."
    :require ("exa") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("exa" "-al" "--color=always" "--icons"
          "--group-directories-first" ,file))))

  (add-to-list 'dirvish-preview-dispatchers 'exa))

; set
(setq! dirvish-quick-access-entries
      '(("c" "~/.config" "Config")
        ("m" "~" "Home")
        ("d" "~/Downloads" "Download")))

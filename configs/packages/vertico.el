(use-package! vertico
  :init
  (vertico-multiform-mode)
  :config
  ;; flat stands for one-line mode
  (setq vertico-multiform-commands
        '(
          (consult-line
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
           (vertico-posframe-border-width . 10)
           (vertico-posframe-fallback-mode . vertico-buffer-mode)
          )
          (t posframe)
          (consult-imenu buffer indexed)
          (execute-extended-command indexed)
          ;; (+vertico/switch-workspace-buffer flat)
          ;; (query-replace flat)
          ;; (moon/query-replace-region flat)
          ;; (moon/query-replace-point flat)
          (query-replace buffer indexed)
          ;; (find-file flat)
          ))
  (setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
  )

  ;; (setq vertico-multiform-categories
  ;;       '((file grid))))

(use-package! vertico
  :init
  (vertico-multiform-mode)
  :config
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (execute-extended-command indexed)
          (+vertico/switch-workspace-buffer flat)
          (query-replace flat)
          (moon/query-replace-region flat)
          (moon/query-replace-point flat)
          (query-replace buffer indexed)
          (find-file flat))))

  ;; (setq vertico-multiform-categories
  ;;       '((file grid))))

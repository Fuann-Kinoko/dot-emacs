(defun my/vertico-truncate-candidates (args)
  (if-let ((arg (car args))
           (type (get-text-property 0 'multi-category arg))
           ((or (eq (car-safe type) 'file) (eq (car-safe type) 'buffer)))
           (w (max 30 (- (window-width) (consult--display-width (nth 2 args)))))
           (l (length arg))
           ((> l w)))
      (setcar args (concat "…" (truncate-string-to-width arg l (- l w)))))
  args)

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
          (consult-imenu (:not posframe) buffer indexed)
          (execute-extended-command indexed)
          (+vertico/switch-workspace-buffer (:not posframe) buffer)
          (my/switch-workspace-buffer-no-dired (:not posframe) flat)
          (my/switch-workspace-buffer-only-dired (:not posframe) flat)
          ;; (query-replace flat)
          (+lookup/references (:not posframe) buffer)
          (moon/query-replace-region (:not posframe) flat)
          (moon/query-replace-point (:not posframe) flat)
          (query-replace buffer indexed)
          (t posframe)
          ;; (find-file flat)
          ))
  (setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)
        (alpha . 50)))
  (advice-add #'vertico--format-candidate :filter-args #'my/vertico-truncate-candidates)
)

(use-package! orderless
  :custom
  ;; enable flex to fuzzy matching, partically to better select buffer for me
  (orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp))
  )

  ;; (setq vertico-multiform-categories
  ;;       '((file grid))))

(provide 'my-vertico)

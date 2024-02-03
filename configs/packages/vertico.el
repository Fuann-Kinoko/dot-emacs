(vertico-multiform-mode)

;; Configure the display per command.
;; Use a buffer with indices for imenu
;; and a flat (Ido-like) menu for M-x.
(setq vertico-multiform-commands
      '((consult-imenu buffer indexed)
        (execute-extended-command indexed)
        (+vertico/switch-workspace-buffer flat)
        (query-replace flat)
        (moon/query-replace-region flat)
        (moon/query-replace-point flat)
        (query-replace buffer indexed)
        (find-file flat)))

;; Configure the display per completion category.
;; Use the grid display for files and a buffer
;; for the consult-grep commands.
(setq vertico-multiform-categories
      '((file grid)
        (consult-grep buffer)))

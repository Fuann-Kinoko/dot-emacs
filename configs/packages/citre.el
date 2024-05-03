;;; configs/packages/citre.el -*- lexical-binding: t; -*-

(use-package! citre
  :config
  (setq citre-default-create-tags-file-location 'global-cache
        citre-tags-file-global-cache-dir "~/.cache/gtags/"))

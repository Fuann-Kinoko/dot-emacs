;;; configs/init-system.el -*- lexical-binding: t; -*-

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/")

(setq gc-cons-threshold (* 50 1000 1000))

(provide 'init-system)

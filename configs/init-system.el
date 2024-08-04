;;; configs/init-system.el -*- lexical-binding: t; -*-

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/")
;; set trash can

(setq gc-cons-threshold (* 50 1000 1000))
;; enlarge gc cons

(provide 'init-system)

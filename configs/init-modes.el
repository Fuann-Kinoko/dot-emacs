;;; configs/init-mode.el -*- lexical-binding: t; -*-

;; adga / agda 2
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))

;; lisp for kmonad
(add-to-list 'auto-mode-alist '("\\.kbd\\'" . lisp-mode))

;; kdl mode (inherited from sdlang mode)
;; (add-to-list 'auto-mode-alist '("\\.kdl\\'" . sdlang-mode))


(provide 'init-modes)

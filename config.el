;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

; misc configs
(add-to-list 'load-path "~/.config/doom/configs")
(require 'init-system)
(require 'init-keybinding)
(require 'init-modes)
(require 'init-orgs)
(require 'init-ui)
(require 'init-debug)

; settings for each installed package
(add-to-list 'load-path "~/.config/doom/configs/module-packages")
(use-package! evil-nerd-commenter)
(use-package! rainbow-mode)
(require 'my-dirvish)
(require 'my-vertico)
(require 'my-auto-dark)
(require 'my-treesitter)
(require 'my-fcitx)
(require 'my-pangu-spacing)
;; (require 'my-calibredb)
;; (require 'my-better-jumper)
(require 'my-dimmer)

; misc utils
(add-to-list 'load-path "~/.config/doom/utils")
(require 'my-dash-board)
;; (require 'my-exwm)

; custom modes
(add-to-list 'load-path "~/.config/doom/modes")
(require 'kdlang-mode)
(require 'kdl-ts-mode)

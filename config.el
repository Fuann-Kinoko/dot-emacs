; misc configs
(add-to-list 'load-path "~/.doom.d/configs")
(require 'init-system)
(require 'init-keybinding)
(require 'init-modes)
(require 'init-orgs)
(require 'init-ui)
(require 'init-debug)

(setq delete-by-moving-to-trash t
      trash-directory "~/trash/")

; settings for each installed package
(add-to-list 'load-path "~/.doom.d/configs/module-packages")
(use-package! evil-nerd-commenter)
(use-package! rainbow-mode)
(require 'my-dirvish)
(require 'my-vertico)
(require 'my-auto-dark)
(require 'my-treesitter)
(require 'my-pangu-spacing)
(require 'my-better-jumper)
(require 'my-dimmer)
;; (require 'my-calibredb)
;; (require 'my-fcitx)

; misc utils
(add-to-list 'load-path "~/.doom.d/utils")
(require 'my-dash-board)

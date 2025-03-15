; misc configs
(add-to-list 'load-path "~/.doom.d/configs")
(require 'init-system)
(require 'init-modes)
(require 'init-ui)
(require 'init-debug)

; settings for each installed package
(add-to-list 'load-path "~/.doom.d/configs/module-packages")
(use-package! evil-nerd-commenter)
(use-package! rainbow-mode)
(use-package! embrace)
(require 'my-dirvish)
(require 'my-vertico)
(require 'my-auto-dark)
(require 'my-treesitter)
(require 'my-pangu-spacing)
;; (require 'my-better-jumper)
(require 'my-dimmer)
;; (require 'my-calibredb)
;; (require 'my-fcitx)


(use-package! meow
  :init
  (meow-global-mode 1)
  :custom
  (meow-use-cursor-position-hack t)
  (meow-keypad-capital-letter-add-ctrl t)
  :config
  (meow-setup-line-number)
  (setopt meow-char-thing-table
        (mapcar (lambda (entry)
                  (cond
                   ((eq (cdr entry) 'string)
                    (cons ?q 'string))       ;; 113 = q
                   ((eq (cdr entry) 'round)
                    (cons ?w 'round))        ;; 119 = w
                   ((eq (cdr entry) 'window)
                    (cons ?j 'window))       ;; 114 = r
                   (t entry)))
                meow-char-thing-table))
  )

; misc utils
(add-to-list 'load-path "~/.doom.d/utils")
(require 'my-dash-board)

(require 'init-keybinding)
(require 'init-orgs)

(require 'server)
(unless (server-running-p)
  (server-start))

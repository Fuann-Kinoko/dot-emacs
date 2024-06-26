;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face


;; (setq doom-font (font-spec :family "Intel One Mono" :size 33 :weight 'regular))
(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 37 :weight 'regular :spacing 100))
(setq doom-variable-pitch-font (font-spec :family "Vollkorn"))
;; (setq doom-font (font-spec :family "Spleen 16x32" :size 41 :weight 'regular))
;; (setq doom-font (font-spec :family "FTT-Chiaro B + FandolSong" :size 38 :weight 'regular))

;; (setq my-cjk-font-name "Fusion Pixel 12px Proportional zh_hant")
;; (setq my-cjk-font-name "FTT-Chiaro B + FandolSong")
;; (setq my-cjk-font-name "Sarasa Gothic SC")
(setq my-cjk-font-name "Sarasa Term SC")

;; 测试中文输入
(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family my-cjk-font-name :height 150))))

(add-hook 'after-setting-font-hook #'my-cjk-font)


;; (setq doom-font (:size 25)
;;       doom-variable-pitch-font (:size 20)
;;       doom-big-font (:size 24))

;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:


(setq doom-gruvbox-material-background  "medium"  ; or hard (defaults to soft)
     doom-gruvbox-material-palette     "material") ; or original (defaults to material)
(setq doom-theme 'doom-gruvbox-material)
;; (setq doom-theme 'doom-tomorrow-day)
;; (setq doom-theme 'doom-flatwhite)
;; (setq doom-theme 'doom-earl-grey)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq delete-by-moving-to-trash t
      trash-directory "~/trash/")

; settings for each installed package
(add-to-list 'load-path "~/.config/doom/configs/module-packages")
(use-package! evil-nerd-commenter)
(use-package! rainbow-mode)
(require 'my-dirvish)
(require 'my-vertico)
(require 'my-auto-dark)
(require 'my-treesitter)
(require 'my-fcitx)
(require 'my-calibredb)
(require 'my-pangu-spacing)
(require 'my-better-jumper)

; misc configs
(add-to-list 'load-path "~/.config/doom/configs")
(require 'init-keybinding)
(require 'init-modes)
(require 'init-orgs)
(require 'init-ui)
(require 'init-debug)

; misc utils
(add-to-list 'load-path "~/.config/doom/utils")
(require 'my-dash-board)

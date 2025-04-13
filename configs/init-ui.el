;;; configs/init-ui.el -*- lexical-binding: t; -*-

;; =========================== Doom Font ===========================
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face


;; (setq doom-font (font-spec :family "Intel One Mono" :size 24 :weight 'regular))
;; (setq doom-font (font-spec :family "Iosevka NFM" :size 37 :weight 'regular :spacing 100))
(setopt doom-font (font-spec :family "M+1Code Nerd Font Mono" :size 35 :weight 'medium))
;; (setq doom-font (font-spec :family "Dank Mono" :size 26))
;; (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 28 :weight 'regular :spacing 100))

;; (setq doom-variable-pitch-font (font-spec :family "Vollkorn"))
;; (setq doom-font (font-spec :family "Spleen 16x32" :size 41 :weight 'regular))
;; (setq doom-font (font-spec :family "FTT-Chiaro B + FandolSong" :size 38 :weight 'regular))

;; (setq my-cjk-font-name "Fusion Pixel 12px Proportional zh_hant")
;; (setq my-cjk-font-name "FTT-Chiaro B + FandolSong")
;; (setq my-cjk-font-name "Sarasa Gothic HC")
(setopt my-cjk-font-name "霞鹜文楷")

;; 测试中文输入
(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family my-cjk-font-name :height 150))))

(add-hook 'after-setting-font-hook #'my-cjk-font)
;; =========================== Doom Font ===========================

;; =========================== Doom Theme ==========================
(setopt doom-gruvbox-material-background  "medium"  ; or hard (defaults to soft)
        doom-gruvbox-material-palette     "material") ; or original (defaults to material)
(setopt doom-theme 'doom-gruvbox-material) ;; 'doom-tomorrow-day', 'doom-flatwhite', 'doom-earl-grey', 'my-mountain', 'doom-gruvbox-material'

(setopt display-line-numbers-type 'relative) ;; `nil', `relative'
;; =========================== Doom Theme ==========================


(after! rustic
  (set-popup-rule! "^\\*cargo-run" :height 0.5)
  (set-popup-rule! "^\\*rustic-compilation" :height 0.5))
;; make rust cargo run window takes half screen

(use-package! ace-window
  :disabled t)
;; (after! ace-window
;;   (customize-set-variable aw-char-position 'top-left)
;;   (set-face-attribute 'aw-leading-char-face nil
;;                       :height 1.5))
;; change the size and the font of indicator triggered by ace-window(C-w C-w)

(setopt lsp-signature-doc-lines 10)
;; change lsp-signature to overlay, instead of jumping a bunch of stuff from bottom buffer
;; which is f***ing annoying

(setopt compilation-scroll-output 'first-error)
(setopt compilation-skip-threshold 2)
;; automatically scroll compilation, if no error

(use-package hl-line
  :config
  (setopt hl-line-sticky-flag nil))
(defun adjust-global-hl-line-mode-based-on-theme ()
  "Adjust `global-hl-line-mode` based on the loaded THEME."
  (let ((theme doom-theme))
    (if (eq theme 'doom-flatwhite)
        (progn (setopt global-hl-line-modes nil)
               (global-hl-line-mode -1))
      (progn (setopt global-hl-line-modes '(prog-mode text-mode conf-mode special-mode org-agenda-mode dired-mode))
             (global-hl-line-mode 1)))))
;; (setq global-hl-line-modes nil)
(add-hook 'after-init-hook 'adjust-global-hl-line-mode-based-on-theme)
(add-hook 'doom-load-theme-hook 'adjust-global-hl-line-mode-based-on-theme)
;; disable background highlight for current line specifally in flatwhite theme
;; so that i can use this theme correctly

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 46)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face))
;; add more blank in margin area, aesthentical setting

(setopt pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode)
(use-package! ultra-scroll
  :config
  (ultra-scroll-mode t))
;; enhance scroll mode

;; (setq evil-shift-width 2)
;; shift only 2 char pressing > or < in visual mode

(custom-set-faces!
  '(org-document-title :height 1.5 :weight extrabold))
;; enlarge org mode title


(setopt scroll-preserve-screen-position t
        scroll-margin 0
        ;; set conservatively to 1, to auto-recenter after c-i c-o jump
        scroll-conservatively 1)
(setopt display-line-numbers-width-start t)
;; let scroll slow down a little bit

;; (use-package doom-modeline
;;  ;; :custom-face
;;  ;; (mode-line ((t (:height 0.85))))
;;  ;; (mode-line-inactive ((t (:height 0.85))))
;;  :custom
;;  (doom-modeline-window-width-limit nil) ;; i have no idea what does that mean, but it fixes the wrong display of right align items
;;  (doom-modeline-height 45)
;;  (doom-modeline-bar-width 6)
;;  (doom-modeline-lsp t)
;;  (doom-modeline-github nil)
;;  (doom-modeline-mu4e nil)
;;  (doom-modeline-irc nil)
;;  (doom-modeline-minor-modes nil)
;;  (doom-modeline-persp-name nil)
;;  (doom-modeline-buffer-file-name-style 'truncate-except-project)
;;  (doom-modeline-major-mode-icon nil))
;; doom modeline customes

;; (global-subword-mode 1)
;; treat CamelCase as a splitted words just like snake_case

;; let which-key jump out quicker
(setopt which-key-idle-delay 0.5)

;;@@IBUFFER 高级 buffer 列表
(use-package ibuffer
  :bind ("C-x C-b" . yy/ibuffer)
  :config
  (defun yy/ibuffer ()
    (interactive)
    (if (string= (buffer-name) "*Ibuffer*")
        (ibuffer-update nil t)
      (ibuffer)))
  ;; 不显示临时 BUFFER
  ;; 还是显示吧
  ;;(setopt ibuffer-never-show-predicates '("^\\*"))
  ;; 不显示为空的分组
  (setopt ibuffer-show-empty-filter-groups nil)
  ;; 不显示汇总信息
  (setopt ibuffer-display-summary nil)
  ;; 显式人类可读的文件大小（Emacs 31 开始支持）
  (setopt ibuffer-human-readable-size t)
  ;; 默认的 filter-group
  (setopt ibuffer-saved-filter-groups
          '(("default"
             ("PROJECT"
              (name . "\\*<p>.+\\*"))
             ("emacs-src-el"
              (and (file-extension . "el")
                   (directory . "share/emacs/.*/lisp")))
             ("emacs-lisp"
              (or (file-extension . "el")
                  (mode . emacs-lisp-mode)))
             ("common-lisp"
              (or (file-extension . "lisp")
                  (mode . lisp-mode)))
             ("scheme/racket"
              (or (mode . scheme-mode)
                  (file-extension . "scm")))
             ("C/C++"
              (or (mode . c-mode)
                  (mode . c++-mode)
                  (filename . ".+\\.\\(c\\|cc\\|cpp\\|h\\|hpp\\)$")))
             ("Python"
              (or (mode . python-mode)
                  (mode . python-ts-mode)
                  (file-extension . "py")))
             ("js/css/html"
              (or (mode . js-mode)
                  (mode . js-ts-mode)
                  (mode . json-ts-mode)
                  (filename . ".+\\.\\(cjs\\|mjs\\|js\\|json\\|ts\\)")
                  (mode . html-mode)
                  (mode . css-mode)
                  (filename . ".+\\.wgsl")
                  (filename . ".+\\.html?")
                  (filename . ".+\\.css")))
             ("Rust"
              (or (mode . rust-ts-mode)
                  (file-extension . "rs")))
             ("rescript"
              (or (mode . rescript-mode)
                  (filename . ".+\\.resi?")))
             ("ORG"
              (or (mode . org-mode)
                  (file-extension . "org")))
             ("DIRED"
              (mode . dired-mode))
             ("IMAGES"
              (or (mode . image-mode)
                  (filename . ".+\\.\\(jpe?g\\|png\\|gif\\|webp\\|ppm\\|pgm\\|pbm\\)")))
             ("TEXT"
              (or (mode . text-mode)
                  (filename . ".+\\.txt")))
             ("CONFIG"
              (or (mode . conf-mode)
                  (filename . ".+\\.toml")
                  (filename . ".+\\.yaml")))
             ("LOG"
              (or (filename . "[cC][hH][aA][nN][gG][eE][lL][oO][gG]")
                  (mode . change-log-mode)))
	     ("SHELL"
	      (mode . shell-mode))
	     ("HELP"
	      (or (mode . help-mode)
		  (mode . Info-mode)
		  (mode . apropos-mode)))
	     ("MAGIT"
	      (or (mode . magit-status-mode)
		  (mode . magit-diff-mode)
		  (mode . magit-log-mode)))
             ("PROCESS"
              (process))
             ("TEMP"
              (name . "\\*.*\\*")))))
  (defun yy/ibuffer-use-default-group ()
    (and (not ibuffer-filter-groups) ;; not use group
         (assoc "default" ibuffer-saved-filter-groups)
         (ibuffer-switch-to-saved-filter-groups "default")))
  (add-hook 'ibuffer-hook 'yy/ibuffer-use-default-group))

;; this is annoying and slow as fuck
(turn-off-flyspell)

(provide 'init-ui)

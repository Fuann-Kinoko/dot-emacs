;; =========================== Doom Font ===========================
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face


;; (setq doom-font (font-spec :family "Intel One Mono" :size 33 :weight 'regular))
(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 26 :weight 'regular :spacing 100))
(setq doom-variable-pitch-font (font-spec :family "Vollkorn"))
;; (setq doom-font (font-spec :family "Spleen 16x32" :size 41 :weight 'regular))
;; (setq doom-font (font-spec :family "FTT-Chiaro B + FandolSong" :size 38 :weight 'regular))

;; (setq my-cjk-font-name "Fusion Pixel 12px Proportional zh_hant")
;; (setq my-cjk-font-name "FTT-Chiaro B + FandolSong")
;; (setq my-cjk-font-name "Sarasa Gothic SC")
(setq my-cjk-font-name "Sarasa Term SC")

(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family my-cjk-font-name :height 150))))
;; 测试中文输入
;; =========================== Doom Font ===========================
;;
;; =========================== Doom Theme ==========================
(setq doom-gruvbox-material-background  "medium"  ; or hard (defaults to soft)
     doom-gruvbox-material-palette     "material") ; or original (defaults to material)
(setq doom-theme 'doom-gruvbox-material) ;; `doom-tomorrow-day', `doom-flatwhite, `doom-earl-grey''

(setq display-line-numbers-type 'relative) ;; `nil', `relative'

;; =========================== Doom Theme ==========================
;;
(add-hook 'after-setting-font-hook #'my-cjk-font)
(after! rustic
  (set-popup-rule! "^\\*cargo-run" :height 0.5)
  (set-popup-rule! "^\\*rustic-compilation" :height 0.5))
;; make rust cargo run window takes half screen

(setq aw-char-position 'left)
(set-face-attribute 'aw-leading-char-face nil
                    :height 2.0)
;; change the size and the font of indicator triggered by ace-window(C-w C-w)

(setq lsp-signature-function 'lsp-signature-posframe)
(setq lsp-signature-doc-lines 10)
;; change lsp-signature to overlay, instead of jumping a bunch of stuff from bottom buffer
;; which is f***ing annoying

(setq compilation-scroll-output 'first-error)
(setq compilation-skip-threshold 2)
;; automatically scroll compilation, if no error

(setq global-hl-line-modes nil)
;; disable background highlight for current line
;; so that i can use flatwhite theme correctly

(modify-all-frames-parameters
'((right-divider-width . 10)
(internal-border-width . 16)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
(face-spec-reset-face face))
;; add more blank in margin area, aesthentical setting

(pixel-scroll-precision-mode)
;; enhance scroll mode

(setq evil-shift-width 2)
;; shift only 2 char pressing > or < in visual mode

(custom-set-faces!
  '(org-document-title :height 1.5 :weight extrabold))
;; enlarge org mode title

(setq scroll-preserve-screen-position t
      scroll-margin 0)
(setq display-line-numbers-width-start t)
;; let scroll slow down a little bit

(setq scroll-conservatively 1)
;; auto recenter after C-i C-o jump

(use-package doom-modeline
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))
;; doom modeline customes

(provide 'init-ui)

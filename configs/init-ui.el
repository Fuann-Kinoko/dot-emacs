(after! rustic
  (set-popup-rule! "^\\*cargo-run" :height 0.5)
  (set-popup-rule! "^\\*rustic-compilation" :height 0.5))
;; make rust cargo run window takes half screen

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
'((right-divider-width . 40)
(internal-border-width . 46)))
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
      scroll-margin 0
      scroll-conservatively 97)
(setq display-line-numbers-width-start t)
;; let scroll slow down a little bit

(provide 'init-ui)

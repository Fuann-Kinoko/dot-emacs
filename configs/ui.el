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

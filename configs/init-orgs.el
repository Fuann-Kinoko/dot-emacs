;; ;;; configs/orgs.el -*- lexical-binding: t; -*-


(setq org-directory "~/org/")

(setq org-link-search-must-match-exact-headline nil) ;; to enable linking words & bookmarks
(setq +zen-text-scale 1)

(require 'org-download)
(require 'org-ros)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq org-image-actual-width 300)

(setq line-spacing 0.1)
(setq org-startup-with-inline-images t)


;; (use-package deft
;;   :defer t
;;   :config
;;   (progn
;;     (setq deft-extensions '("md" "markdown" "org" "txt"))
;;     (setq deft-directory "~/org/")
;;     (setq deft-file-naming-rules '((noslash . "-")
;;                                    (nospace . "-")
;;                                    (case-fn . downcase)))
;;     (setq deft-recursive t)
;;     (evil-define-key 'insert deft-mode-map (kbd "C-<backspace>") #'deft-filter-decrement-word)
;;     (evil-define-key 'insert deft-mode-map (kbd "C-w") #'deft-filter-decrement-word)
;;     (evil-define-key 'insert deft-mode-map (kbd "C-k") #'deft-filter-clear)))

(use-package! org-appear
  :defer t
  :custom
  (org-hide-emphasis-markers t)
  (org-appear-inside-latex t)
  :hook
  (org-mode . org-appear-mode)
  )

;; (use-package! org-roam
;;   :defer t
;;   :ensure t
;;   :bind
;;   (:map org-mode-map
;;         ("C-SPC" . completion-at-point))
;;   :config
;;   (org-roam-db-autosync-enable))

(use-package! org-latex-preview :defer t
  :init
  (setq org-startup-with-latex-preview 't)
  (setq org-latex-preview-live nil)
  (setq org-pretty-entities 't)
  :config
  ;; Increase preview width
  (setq org-latex-preview-appearance-options
        '(
          :foreground auto
          :background "Transparent"
          :scale 1.04
          :zoom 1.04
          :page-width 0.6))

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n and C-p from opening up previews when using auto-mode
  ;; (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  ;; (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))
;;
;; ;; Use CDLaTeX to improve editing experiences
(use-package! cdlatex
  :defer t
  :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

(after! org
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :arrow_right   "->"
    :arrow_left    "<-"
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :property      "#+property:"
    :options       "#+options:"
    :startup       "#+startup:"
    :macro         "#+macro:"
    :html_head     "#+html_head:"
    :html          "#+html:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :latex         "#+latex:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_html:"
    :attr_org      "#+attr_org:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"
    :roam_tags     "#+roam_tags:"
    :filetags      "#+filetags:")
)

(after! org
    (setq org-src-fontify-natively t
    org-fontify-whole-heading-line t
    org-agenda-block-separator ""
    org-fontify-done-headline t
    org-startup-indented t
    org-startup-folded 'fold
    org-fontify-quote-and-verse-blocks t))


;; (add-hook 'org-mode-hook #'org-modern-mode)
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
;; (use-package! org-modern
;;   :custom
;;   (org-modern-block-name nil)
;;   (org-auto-align-tags nil)
;;   (org-tags-column 0)
;;   (org-fold-catch-invisible-edits 'show-and-error)
;;   (org-special-ctrl-a/e t)
;;   (org-insert-heading-respect-content t)
;;   (org-ellipsis "…")
;;   (org-agenda-tags-column 0)
;;   (org-agenda-time-grid '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000) " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
;;   (org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────")
;;   (org-modern-star 'replace)

;;   :config
;;   (global-org-modern-mode)
;;   (defun my-org-mode-no-line-number-hook () (setq display-line-numbers nil))
;;   (add-hook 'org-mode-hook '(lambda ()
;;                             (setq display-line-numbers nil))))

;; (use-package! org-margin
;;   :config
;;   )

;; (use-package! org-modern-indent
;;   :config
;;   (setq
;;    org-modern-indent-begin " "
;;    org-modern-indent-guide " "
;;    org-modern-indent-end " ")
;;   (add-hook 'org-mode-hook #'org-modern-indent-mode 100)) ;; 90 is the depth

(provide 'init-orgs)

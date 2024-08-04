;; ;;; configs/orgs.el -*- lexical-binding: t; -*-


(setq org-directory "~/org/")

(setq org-link-search-must-match-exact-headline nil) ;; to enable linking words & bookmarks
(setq +zen-text-scale 1)

(require 'org-download)
(require 'org-ros)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq org-image-actual-width 600)

(setq line-spacing 0.1)
(setq org-startup-with-inline-images t)

;; prettify symbol
(setq-default prettify-symbols-alist
  '(("[ ]"           . "")
  ("[-]"             . "")
  ("[X]"             . "")
  ("#+BEGIN_SRC"     . "✎")
  ("#+END_SRC"       . "⇤")
  ("#+begin_src"     . "✎")
  ("#+end_src"       . "⇤")
  ("#+RESULTS:"      . "⟾")
  ("#+begin_quote"   . "»")
  ("#+end_quote"     . "⇤")
  ("#+begin_verse"   . "ζ")
  ("#+end_verse"     . "⇤")
  ("#+begin_example" . "⟝")
  ("#+end_example"   . "⇤")
  ("#+begin_export"  . "🙵")
  ("#+end_export"    . "⇤")
  ("#+BEGIN_QUOTE"   . "»")
  ("#+END_QUOTE"     . "⇤")
  ("#+BEGIN_VERSE"   . "ζ")
  ("#+END_VERSE"     . "⇤")
  ("#+BEGIN_EXAMPLE" . "⟝")
  ("#+END_EXAMPLE"   . "⇤")
  ("#+BEGIN_EXPORT"  . "🙵")
  ("#+END_EXPORT"    . "⇤")
  ("#+END:"          . "⇤")
  ("#+BEGIN:"        . "✎")
  ("#+CAPTION:"      . "✑")
  ("#+ATTR_LATEX"    . "🄛")))

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
  :hook
  (org-mode . org-appear-mode)
  (org-mode . prettify-symbols-mode)
  :custom
  (org-hide-emphasis-markers t)
  (org-appear-inside-latex t)
  )

;; (use-package! org-roam
;;   :defer t
;;   :ensure t
;;   :bind
;;   (:map org-mode-map
;;         ("C-SPC" . completion-at-point))
;;   :config
;;   (org-roam-db-autosync-enable)
;;   (setq org-roam-node-display-template
;;       (concat "${doom-hierarchy:*}" (propertize "${tags:10}" 'face 'org-tag))))
(use-package! denote
  :custom
  (denote-directory "/home/akerue/org/denote")
  (denote-known-keywords '("book" "main" "reference"))
  (denote-rename-buffer-format "[D] %t")
  (denote-excluded-files-regexp "^.*(png|jpg|jpeg)$")
  (denote-backlinks-show-context 't)
  :config
  (denote-rename-buffer-mode)
  )
(use-package! consult-notes
  :commands
  (consult-notes
   consult-notes-search-in-all-notes)
  :config
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  )

(use-package! org-latex-preview
  :init
  (setq org-startup-with-latex-preview 't)
  (setq org-latex-preview-live nil)
  (setq org-pretty-entities 't)
  :hook
  (org-mode . org-latex-preview-auto-mode)
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

  ;; Block C-n and C-p from opening up previews when using auto-mode
  ;; (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  ;; (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered nil)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))
;;
;; ;; Use CDLaTeX to improve editing experiences
(use-package! cdlatex
  :hook (org-mode . cdlatex-mode))

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

;;; configs/module-packages/my-dimmer.el -*- lexical-binding: t; -*-

;; Dim inactive windows
;; this is a bit of problematic, so it is disabled in default
;; (use-package! dimmer
;;   :ensure nil
;;   :hook (after-init . dimmer-mode)
;;   :config
;;   (setopt dimmer-fraction 0.5)
;;   (setopt dimmer-adjustment-mode :foreground)
;;   (setopt dimmer-use-colorspace :rgb)
;;   (setopt dimmer-watch-frame-focus-events nil)
;;   (dimmer-configure-which-key)
;;   (dimmer-configure-magit)
;;   (dimmer-configure-org)
;;   (dimmer-configure-posframe))

;; TODO: after update, 'auto-dim-other-buffers' seems to result in unexpected behavior
;; as so, it is disabled until i have figured some debugging
;; (defadvice! auto-dim-set-faces(&rest _)
;;   :after #'enable-theme
;;   (set-face-attribute 'auto-dim-other-buffers-face nil
;;        :background (doom-darken
;;                     (doom-color 'bg)
;;                     0.2)
;;        :foreground (doom-darken
;;                     (doom-color 'fg)
;;                     0.4)))
;; (defun my/never-dim-speical-buffers (buffer)
;;   "Return non-nil if the name of BUFFER starts with * and ends with *.
;; Example: '*Minibuf-1*' or '*info*'"
;;   (string-match-p ".*\\*.*\\*$" (buffer-name buffer)))
;; (defun my/adob--remap-faces-advice (buffer object)
;;   "Advice to modify `wants` in `adob--remap-faces`.
;; Diasble dim for other buffers if current one is minibuffer."
;;   (let ((wants (and (not (adob--never-dim-p buffer))
;;                      (not (window-minibuffer-p))))
;;          (has (buffer-local-value 'adob--face-mode-remapping buffer)))
;;     (when (eq wants (not has))
;;       (set-buffer buffer)
;;       (if wants
;;           (adob--remap-add-relative)
;;         (adob--remap-remove-relative))
;;       (force-window-update object)
;;       wants)))

;; (use-package! auto-dim-other-buffers
;;   :hook
;;   ((after-init . auto-dim-other-buffers-mode))
;;   :custom
;;   (auto-dim-other-buffers-dim-on-focus-out nil)
;;   (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
;;   (add-hook 'auto-dim-other-buffers-never-dim-buffer-functions 'my/never-dim-speical-buffers)
;;   (advice-add 'adob--remap-faces :override #'my/adob--remap-faces-advice)
;;  )

;; ;; add a slight pulse when changing focus window
;; ;; usually combined with 'auto-dim-other-buffers'
(use-package! pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . +recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . +recenter-and-pulse-line))
  :init
  (setopt pulse-delay 0.1
        pulse-iterations 2)

  (defun +pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun +pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (+pulse-momentary-line)))

  (defun +recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+pulse-momentary))

  (defun +recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (+pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window switch-to-buffer
                 aw-select toggle-window-split
                 windmove-do-window-select
                 pager-page-down pager-page-up
                 treemacs-select-window
                 tab-bar-select-tab))
    (advice-add cmd :after #'+pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'+recenter-and-pulse))

  (dolist (cmd '(symbol-overlay-basic-jump
                 compile-goto-error))
    (advice-add cmd :after #'+recenter-and-pulse-line))
  )


;; (use-package! evil-goggles
;;   :config
;;   (evil-goggles-mode)
;;   (evil-goggles-use-diff-faces)
;;   )

(provide 'my-dimmer)

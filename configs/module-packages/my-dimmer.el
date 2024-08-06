;;; configs/module-packages/my-dimmer.el -*- lexical-binding: t; -*-

;; Dim inactive windows
;; this is a bit of problematic, so it is disabled in default
(use-package! dimmer
  :disabled
  :ensure nil
  :hook (after-init . dimmer-mode)
  :config
  (setq dimmer-fraction 0.5)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)
  (setq dimmer-watch-frame-focus-events nil)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-posframe))

(defadvice! auto-dim-set-faces(&rest _)
  :after #'enable-theme
  (set-face-attribute 'auto-dim-other-buffers-face nil
       :background (doom-darken
                    (doom-color 'bg)
                    0.2)
       :foreground (doom-darken
                    (doom-color 'fg)
                    0.4)))

(defun my/never-dim-speical-buffers (buffer)
  "Return non-nil if the name of BUFFER starts with * and ends with *.
Example: '*Minibuf-1*' or '*info*'"
  (string-match-p ".*\\*.*\\*$" (buffer-name buffer)))

(defun my/adob--remap-faces-advice (buffer object)
  "Advice to modify `wants` in `adob--remap-faces`.
Diasble dim for other buffers if current one is minibuffer."
  (let ((wants (and (not (adob--never-dim-p buffer))
                     (not (window-minibuffer-p))))
         (has (buffer-local-value 'adob--face-mode-remapping buffer)))
    (when (eq wants (not has))
      (set-buffer buffer)
      (if wants
          (adob--remap-add-relative)
        (adob--remap-remove-relative))
      (force-window-update object)
      wants)))

(use-package! auto-dim-other-buffers
  :hook
  ((after-init . auto-dim-other-buffers-mode)
   (auto-dim-other-buffers-mode . auto-dim-set-faces))
  :custom
  (auto-dim-other-buffers-dim-on-focus-out nil)
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  :config
  (add-hook 'auto-dim-other-buffers-never-dim-buffer-functions 'my/never-dim-speical-buffers)
  (advice-add 'adob--remap-faces :override #'my/adob--remap-faces-advice)
  )

(provide 'my-dimmer)

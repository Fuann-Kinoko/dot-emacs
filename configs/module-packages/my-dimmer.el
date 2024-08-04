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
  "Return non-nil if BUFFER's name starts with * and ends with *.
   Example: '*Minibuf-1*'"
  (string-match-p ".*\\*.*\\*$" (buffer-name buffer)))

(use-package! auto-dim-other-buffers
  :hook
  ((after-init . auto-dim-other-buffers-mode)
   (auto-dim-other-buffers-mode . auto-dim-set-faces))
  :custom
  (auto-dim-other-buffers-dim-on-focus-out nil)
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  :config
  (add-hook 'auto-dim-other-buffers-never-dim-buffer-functions 'my/never-dim-speical-buffers))

;; below are for debugging
;; (mapcar (lambda (buffer)
;;         ;; It’s tempting to read the value of the variable and not bother
;;         ;; with the buffer if the value is nil since in that case the
;;         ;; buffer is presumably never-dim and thus we won’t remap any
;;         ;; faces in it.  There is one corner case when this is not true
;;         ;; however.  If at one point user set list of faces to affect to
;;         ;; nil the list of remapping will be nil as well and when user
;;         ;; changes the variable we’ll need to add remappings.
;;         (message
;;          (buffer-name buffer)
;;          ;; (my/never-dim-which-key-buffer buffer)
;;          ;; (run-hook-with-args-until-success
;;          ;;  'auto-dim-other-buffers-never-dim-buffer-functions buffer)
;;          ))
;;       (buffer-list))

(provide 'my-dimmer)

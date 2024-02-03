;;; configs/packages/telega.el -*- lexical-binding: t; -*-

;; telegram client for emacs
(use-package! telega
  :commands (telega)
  :defer t
  :bind ("C-c t" . #'telega)
  :init
  (unless (display-graphic-p) (setq telega-use-images nil))
  ;; :hook
  ;; ('telega-root-mode . #'evil-emacs-state)
  ;; ('telega-chat-mode . #'evil-emacs-state)
  ;; ('telega-chat-mode . #'yas-minor-mode)
  ;; ('telega-chat-mode . (lambda ()
  ;;                        (set-company-backend! 'telega-chat-mode
  ;;                          (append '(telega-company-emoji
  ;;                                    telega-company-username
  ;;                                    telega-company-hashtag)
  ;;                                  (when (telega-chat-bot-p telega-chatbuf--chat)
  ;;                                    '(telega-company-botcmd))))
  ;;                        (company-mode 1)))
  ;; ('telega-chat-pre-message . #'telega-msg-ignore-blocked-sender)
  :config
  (setq telega-proxies
        (list '(:server "127.0.0.1" :port 10809 :enable t
                        :type (:@type "proxyTypeSocks5"))))
  (set-popup-rule! "^\\*Telega Root"
    :side 'right :size 100 :quit nil :modeline t)
  (set-popup-rule! "^◀\\(\\[\\|<\\|{\\).*\\(\\]\\|>\\|}\\)"
    :side 'right :size 100 :quit nil :modeline t)
  (telega-mode-line-mode 1))

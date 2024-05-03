;;; configs/packages/fcitx.el -*- lexical-binding: t; -*-

(use-package! fcitx
  :ensure t
  :config
  (setq fcitx-use-dbus nil
      fcitx-remote-command "fcitx5-remote")
  (fcitx-aggressive-setup))

(provide 'my-fcitx)

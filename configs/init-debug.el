;;; configs/init-debug.el -*- lexical-binding: t; -*-

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(provide 'init-debug)

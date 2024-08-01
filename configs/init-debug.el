;;; configs/init-debug.el -*- lexical-binding: t; -*-

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

; dap settings
(require 'dap-lldb)
(require 'dap-cpptools)

(provide 'init-debug)

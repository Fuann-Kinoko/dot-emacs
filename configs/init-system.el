;;; configs/init-system.el -*- lexical-binding: t; -*-

(setopt delete-by-moving-to-trash t
      trash-directory "shell:RecycleBinFolder")
;; set trash can

(setopt gc-cons-threshold (* 50 1000 1000))
;; enlarge gc cons

;; switch magit git to quicker one
(setopt magit-git-executable "C:\\Program Files\\Git\\mingw64\\bin\\git.exe")

;; add path for pwsh so doom can reload properly in windows
(push "C:\\Program Files\\PowerShell\\7" exec-path)
;; (use-package! powershell
;;     :config
;;     ;; Change default compile command for powershell
;;     (add-hook 'powershell-mode-hook
;;     (lambda ()
;;         (set (make-local-variable 'compile-command)
;;              (format "powershell.exe -NoLogo -NonInteractive -Command \"& '%s'\""
;;                      (buffer-file-name))))))

;; 避免中文乱码
(set-language-environment "Chinese-GB")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setopt selection-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  ;; powershell default shell
  (let ((xlist
         '(
           "C:\\Program Files\\PowerShell\\7\\pwsh.exe"
           ))
        xfound)
    (setq xfound (seq-some (lambda (x) (if (file-exists-p x) x nil)) xlist))
    (when xfound (setq explicit-shell-file-name xfound)))
  ;; 默认的4kb管道小了一点, 给到64 kb
  (setopt w32-pipe-buffer-size (* 64 1024))
  (setopt auto-mode-case-fold nil)
  (setopt ispell-program-name "aspell")
  (tooltip-mode -1)
  )

;; add hippie (simple auto-complete support)
(setopt hippie-expand-try-functions-list
      '(try-expand-list
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-all-abbrevs
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev-from-kill
        try-expand-whole-kill
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; make custom themes safe
(setopt custom-safe-themes t)

(provide 'init-system)

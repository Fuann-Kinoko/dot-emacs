;;; configs/keybinding.el -*- lexical-binding: t; -*-

;; i don't need esc prefix, so that's it
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; remap yes or no -> or or no :)
(define-key y-or-n-p-map      "o" 'act)
(define-key query-replace-map "o" 'act)

(defun my-yank-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let* ((file-path (+default/yank-buffer-path))
            (file-name (file-name-nondirectory file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun moon/query-replace-region ()
  "Query replace selected region."
  (interactive)
  (let ((sel (buffer-substring-no-properties
                  (region-beginning)
                  (region-end))))
  (query-replace sel
                   (completing-read (format "Replace \"%s\" to: " sel) ())
                   nil (beginning-of-line))))
(defun moon/query-replace-point ()
  "Query replace thing at point."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (query-replace word
                   (completing-read (format "Replace \"%s\" to: " word) ())
                   nil (beginning-of-line))))

(general-evil-setup)
(global-set-key [escape] 'keyboard-quit)
(setq avy-timeout-seconds 0.2)


;; normal keybindings:
(evil-define-key 'normal 'global
  "J"   'back-to-indentation
  "K"   'evil-end-of-line
  "gh" '+lookup/documentation
  "gb"  'eval-defun
  "-"   'evilnc-comment-or-uncomment-lines
  "s"   'avy-goto-char-2
  (kbd "C-J")    '("jump to below"   . evilem-motion-next-line)
  (kbd "C-K")    '("jump to above"   . evilem-motion-previous-line)
  (kbd "C-B")    '("replace word"   . moon/query-replace-point)
  (kbd "C-L")    '("multi next"     . evil-multiedit-match-and-next)
  (kbd "C-S-L")  '("multi all"      . evil-multiedit-match-all)
  (kbd "M-L")    '("smart enlarge"  . er/expand-region)
  (kbd "M-H")    '("smart shrink"   . er/contract-region)
  (kbd "SPC fn") '("yank file name" . my-yank-file-name)
  (kbd "<f8>")   '("next error"     . next-error))

(map! :leader
       :desc "join line      " "j" #'evil-join
       :desc "buffer-vertico " "," #'+vertico/switch-workspace-buffer)

(evil-define-key 'visual 'global
  (kbd "C-L")    '("multi next"     . evil-multiedit-match-and-next)
  (kbd "C-S-L")  '("multi all"      . evil-multiedit-match-all)
  (kbd "C-B")    '("replace region" . moon/query-replace-region)
  "J"   'back-to-indentation
  "K"   'evil-end-of-line
  "-"   'evilnc-comment-or-uncomment-lines)

(evil-define-key 'insert 'global
  (kbd "C-SPC")  '("complete filename"     . comint-dynamic-complete-filename))

(general-nmap "RET" (general-simulate-key "cio"))
(general-nmap "f"   (general-simulate-key "gs SPC"))

(map! :leader
      (:prefix ("l" . "lsp")
       :desc "list symbols" "s"    #'consult-lsp-symbols
       :desc "code action" "a"     #'lsp-execute-code-action
       :desc "clicl code lens" "l" #'lsp-avy-lens
       :desc "check refer" "r"     #'+lookup/references))


;; window control keybindings
(map! :map evil-window-map
      "o" `delete-other-windows)

;; dired keybindings:
(map! :leader
      (:prefix ("d" . "dired")
       :desc "open dired in current file" "d" #'dirvish
       :desc "jump history" "j" #'dirvish-history-jump))
(evil-define-key 'normal dired-mode-map
  "h"         'dired-up-directory
  "l"         'dired-find-file
  "-"         `dired-do-kill-lines
  "w"         `dirvish-layout-toggle
  "o"         `dirvish-quick-access
  "i"         `dirvish-file-info-menu
  "y"         `dirvish-yank-menu
  "f"         `dirvish-narrow
  "s"         `dirvish-quicksort
  (kbd "TAB") `other-window
  (kbd "M-l") `dirvish-ls-switches-menu
  (kbd "M-m") `dirvish-mark-menu
  (kbd "M-t") `dirvish-layout-toggle
  (kbd "M-s") `dirvish-setup-menu
  (kbd "M-e") `dirvish-emerge-menu
  (kbd "M-j") `dirvish-fd-jump)

; haskell repl(interactive) mode
(evil-define-key 'insert haskell-interactive-mode-map
  (kbd "C-l")   `haskell-interactive-mode-clear
  (kbd "<up>")  `haskell-interactive-mode-history-previous
  (kbd "<down>")`haskell-interactive-mode-history-next)

(evil-define-key 'normal haskell-mode-map
  (kbd "gk")     '("check info" . haskell-process-do-info)
  (kbd "SPC lc") '("load the repl" . haskell-process-load-file))

(evil-define-key 'normal vterm-mode-map
  (kbd "M-m w")  '("new frame" . make-frame-command))

(evil-define-key 'insert vterm-mode-map
  (kbd "C-S-c")  '("paste" . vterm-yank))

;; (define-key ido-file-dir-completion-map
;;   (kbd "M-w")    '("copy file name" . ido-copy-current-file-name))

; Alt+m key bindings
(bind-keys
 :prefix "M-m"
 :prefix-map launchpad-keys
 ("h" . +doom-dashboard/open)
 ("f" . consult-recent-file)
 ("s" . doom/switch-to-scratch-buffer)
 ("i" . ibuffer)
 ("w" . make-frame-command)
 ("t" . todo-show))

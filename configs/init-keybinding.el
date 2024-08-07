;;; configs/keybinding.el -*- lexical-binding: t; -*-

;; i don't need esc prefix, so that's it
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Embark settings
(setq embark-quit-after-action '((kill-buffer . nil) (t . t)))
(map! :map 'minibuffer-mode-map
  "M-e"    #'embark-act
  "C-o"    #'my/delete-word-backward
  "C-<backspace>" #'my/delete-word-backward)

;; remap yes or no -> or or no :)
(define-key y-or-n-p-map      "o" 'act)
(define-key query-replace-map "o" 'act)

; ================== custom functions ==================

(defun my-ace-sneak ()
  "Simulate gs SPC for sneak-like cursor jump."
  (interactive)
  (let ((current-prefix-arg t))
    (evil-avy-goto-char-timer)))

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
        ;; (cl-letf (((symbol-function 'vr--set-regexp-string) ;;
        ;;         (lambda () (setq vr--regexp-string sel))))  ;;
        ;; (call-interactively 'vr/query-replace))))           ;;
  (query-replace sel
                   (completing-read (format "Replace \"%s\" to: " sel) ())
                   nil (beginning-of-line))))
(defun moon/query-replace-point ()
  "Query replace thing at point."
  (interactive)
  (let ((word (thing-at-point 'word t)))
        ;; (cl-letf (((symbol-function 'vr--set-regexp-string) ;;
        ;;         (lambda () (setq vr--regexp-string word)))) ;;
        ;; (call-interactively 'vr/query-replace))))           ;;
    (query-replace word
                   (completing-read (format "Replace \"%s\" to: " word) ())
                   nil (beginning-of-line))))

(defun revert-buffer-fine-no-confirm ()
  "revert buffer fine without confirm"
  (interactive)
  (revert-buffer-with-fine-grain nil t))

(defun repeat-last-complex-command ()
  "Basically 'repeat-complex-command' but without confirm."
  (lol)
  (repeat-complex-command 1))

(defun reload-current-dired-buffer ()
  "Reload current `dired-mode' buffer."
  (let* ((dir (dired-current-directory)))
    (progn (kill-buffer (current-buffer))
           (dired dir))))

;; delete not kill it into kill-ring
;; _based on_ http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my/delete-word-backward (arg)
  "Delete backward, not triggering `kill-ring', dont bother with 'arg' tho."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word (- arg))
     (point))))

(defun my/switch-workspace-buffer-state-preview ()
  "Copy from '+vertico--workspace-buffer-state'."
  (let ((preview
         ;; Only preview in current window and other window.
         ;; Preview in frames and tabs is not possible since these don't get cleaned up.
         (if (memq consult--buffer-display
                   '(switch-to-buffer switch-to-buffer-other-window))
             (let ((orig-buf (current-buffer))
                   other-win
                   cleanup-buffers)
               (lambda (action cand)
                 (when (eq action 'preview)
                   (when (and (eq consult--buffer-display #'switch-to-buffer-other-window)
                              (not other-win))
                     (switch-to-buffer-other-window orig-buf)
                     (setq other-win (selected-window)))
                   (let ((win (or other-win (selected-window))))
                     (when (window-live-p win)
                       (with-selected-window win
                         (cond
                          ((and cand (get-buffer cand))
                           (unless (+workspace-contains-buffer-p cand)
                             (cl-pushnew cand cleanup-buffers))
                           (switch-to-buffer cand 'norecord))
                          ((buffer-live-p orig-buf)
                           (switch-to-buffer orig-buf 'norecord)
                           (mapc #'persp-remove-buffer cleanup-buffers)))))))))
           #'ignore)))
    (lambda (action cand)
      (funcall preview action cand))))
(defun my/switch-workspace-buffer-with-extra-predicate (extra-predicate)
  "the arg 'extra-predicate' is to filter the buffer list in it
   it is describe as a lambda function containing arg 'buf'
   please refer to
   '+vertico--workspace-generate-sources' and
   '+vertico/switch-workspace-buffer'
   for more information.
  "
  (require 'consult)
  (when-let
      (buffer
       (consult--multi
        (let* ((active-workspace (+workspace-current-name))
               (workspaces (+workspace-list-names))
               (key-range (append (cl-loop for i from ?1 to ?9 collect i)
                                  (cl-loop for i from ?a to ?z collect i)
                                  (cl-loop for i from ?A to ?Z collect i)))
               (last-i (length workspaces))
               (i 0))
          (mapcar (lambda (name)
                    (cl-incf i)
                    `(:name     ,name
                      :hidden   ,(not (string= active-workspace name))
                      :narrow   ,(nth (1- i) key-range)
                      :category buffer
                      :state    my/switch-workspace-buffer-state-preview
                      :items    ,(lambda ()
                                   (consult--buffer-query
                                    :sort 'visibility
                                    :as #'buffer-name
                                    :predicate
                                    (lambda (buf)
                                      (when-let (workspace (+workspace-get name t))
                                        (and (+workspace-contains-buffer-p buf workspace)
                                             (funcall extra-predicate buf)
                                             )))))))
                  (+workspace-list-names)))
        :require-match
        (confirm-nonexistent-file-or-buffer)
        :prompt (format "Switch to buffer (%s): "
                        (+workspace-current-name))
        :history 'consult--buffer-history
        :sort nil))
    (if-let (window (get-buffer-window (car buffer)))
        (select-window window)
      (funcall consult--buffer-display (car buffer)))))
(defun my/switch-workspace-buffer-no-dired ()
  (interactive)
  (my/switch-workspace-buffer-with-extra-predicate
   (lambda (buf)
     (not (eq (buffer-local-value 'major-mode (get-buffer buf))
                                                      'dired-mode)))))
(defun my/switch-workspace-buffer-only-dired ()
  (interactive)
  (my/switch-workspace-buffer-with-extra-predicate
   (lambda (buf)
     (eq (buffer-local-value 'major-mode (get-buffer buf))
                                                      'dired-mode))))

; ================== bindings ==================
;
(general-evil-setup)
;; (global-set-key [escape] 'keyboard-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq avy-timeout-seconds 0.2)

(defun jump-backward-center ()
  "jump backward then recenter"
  (interactive)
  (better-jumper-jump-backward))
(defun jump-forward-center ()
  "jump forward then recenter"
  (interactive)
  (better-jumper-jump-forward))

;; normal keybindings:
(evil-define-key 'normal 'global
  "J"   'back-to-indentation
  "K"   'evil-last-non-blank
  "gh" '+lookup/documentation
  "gb"  'eval-defun
  "-"   'evilnc-comment-or-uncomment-lines
  ;; (kbd "<C-o>")  '("jump to back"            . jump-backward-center)
  ;; (kbd "<C-i>")  '("jump to fore"            . jump-forward-center)
  (kbd "C-s")    '("save buffer"                . save-buffer)
  (kbd "s")      '("sneak"                      . my-ace-sneak)
  (kbd "C-j")    '("jump to below"              . sp-next-sexp)
  (kbd "C-k")    '("jump to above"              . backward-up-list)
  (kbd "C-B")    '("replace word"               . moon/query-replace-point)
  (kbd "C-L")    '("multi next"                 . evil-multiedit-match-and-next)
  (kbd "C-S-L")  '("multi all"                  . evil-multiedit-match-all)
  (kbd "M-L")    '("smart enlarge"              . er/expand-region)
  (kbd "M-e")    '("embark"                     . embark-act)
  (kbd "M-n")    '("consult notes"              . consult-notes)
  ;; (kbd "M-p")    '("paste previous"          . evil-paste-pop) ;; this is replaced by C-p
  (kbd "M-b")    '("buffers"                    . my/switch-workspace-buffer-no-dired)
  (kbd "M-d")    '("direds"                     . my/switch-workspace-buffer-only-dired)
  (kbd "M-H")    '("smart shrink"               . er/contract-region)
  (kbd "M-w")    '("alt workspace"              . +workspace/switch-to)
  (kbd "SPC fn") '("yank file name"             . my-yank-file-name)
  (kbd "SPC e")  '("dirvish side"               . dirvish-side)
  (kbd "<f8>")   '("next error"                 . next-error)
  (kbd "C-<backspace>") '("delete without copy" . my/delete-word-backward))

(global-unset-key (kbd "C-;"))

(map! :leader
       :desc "join line      " "j" #'evil-join
       :desc "buffer-vertico " "," #'my/switch-workspace-buffer-no-dired)

(evil-define-key 'visual 'global
  (kbd "C-L")    '("multi next"     . evil-multiedit-match-and-next)
  (kbd "C-S-L")  '("multi all"      . evil-multiedit-match-all)
  (kbd "C-B")    '("replace region" . moon/query-replace-region)
  "J"   'back-to-indentation
  "K"   'evil-last-non-blank
  "-"   'evilnc-comment-or-uncomment-lines)

(after! info
  (evil-define-key 'normal Info-mode-map
    "J"   'back-to-indentation
    "K"   'evil-last-non-blank))

(evil-define-key 'insert 'global
  (kbd "C-SPC")  '("complete filename" . comint-dynamic-complete-filename)
  (kbd "M-e")    '("embark"            . embark-act)
  (kbd "C-o")    '("delete word"       . my/delete-word-backward)
  (kbd "M-y")    '("yasnippet expand"  . yas-expand) ;
  (kbd "C-s")    '("save buffer"       . save-buffer)
  (kbd "C-S-V")  '("paste"             . evil-paste-after))

;; (general-nmap "RET" (general-simulate-key "cio"))
;; (general-nmap "f"   (general-simulate-key "gs SPC"))
;; (general-nmap "s"   (general-simulate-key "gs SPC"))

(map! :leader
      (:prefix ("l" . "lsp")
       :desc "list symbols"        "s"   #'consult-lsp-symbols
       :desc "code action"         "a"   #'lsp-execute-code-action
       :desc "clicl code lens"     "l"   #'lsp-avy-lens
       :desc "check refer"         "r"   #'+lookup/references
       :desc "diagnostics"         "d"   #'+default/diagnostics
       :desc "format"              "f"   #'lsp-format-buffer))

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "revert"              "r"   #'revert-buffer-fine-no-confirm))

;; window control keybindings
(map! :map evil-window-map
      "o" `delete-other-windows)

;; dired keybindings:
(map! :leader
      (:prefix ("d" . "dired")
       :desc "open dired in current file" "d" #'dired-jump
       :desc "jump history" "j" #'dirvish-history-jump))
(after! dired
  (evil-define-key 'normal dired-mode-map
    "h"         'dired-up-directory
    "l"         'dired-find-file
    "-"         `dired-do-kill-lines
    "w"         `dirvish-layout-toggle
    "r"         `revert-buffer
    "R"         `wdired-change-to-wdired-mode
    "X"         `dired-do-rename
    "o"         `dirvish-quick-access
    "i"         `dirvish-file-info-menu
    "y"         `dirvish-yank-menu
    "f"         `dirvish-narrow
    ","         `dirvish-quicksort
    "."         `dired-omit-mode
    (kbd "s")   `dirvish-fd
    (kbd "TAB") `other-window
    (kbd "M-l") `dirvish-ls-switches-menu
    (kbd "M-m") `dirvish-mark-menu
    (kbd "M-t") `dirvish-layout-toggle
    (kbd "M-s") `dirvish-setup-menu
    (kbd "M-e") `dirvish-emerge-menu
    (kbd "M-j") `dirvish-fd-jump
    (kbd "TAB") `dirvish-toggle-subtree))

; haskell repl(lol) mode
(evil-define-key 'normal haskell-mode-map
  (kbd "gk")     '("check info" . haskell-process-do-info)
  (kbd "SPC lc") '("load the repl" . haskell-process-load-file))
(evil-define-key 'insert haskell-lol-mode-map
  (kbd "C-l")   `haskell-interactive-mode-clear
  (kbd "<up>")  `haskell-interactive-mode-history-previous
  (kbd "<down>")`haskell-interactive-mode-history-next)

; idris repl(interactive) mode
(evil-define-key 'insert idris-repl-mode-map
  (kbd "C-l")   `idris-repl-clear-buffer
  (kbd "<up>")  `idris-repl-backward-history
  (kbd "<down>")`idris-repl-forward-history)

(evil-define-key 'normal pdf-view-mode-map
  (kbd "j")  (lambda() (interactive) (pdf-view-next-line-or-next-page 2))
  (kbd "k")  (lambda() (interactive) (pdf-view-previous-line-or-previous-page 2))
  (kbd "d")  (lambda() (interactive) (pdf-view-next-line-or-next-page 8))
  (kbd "u")  (lambda() (interactive) (pdf-view-previous-line-or-previous-page 8)))

(evil-define-key 'normal vterm-mode-map
  (kbd "M-m w")  '("new frame" . make-frame-command))

(evil-define-key 'normal org-mode-map
  (kbd "C-J")    '("jump to below heading"   . org-next-visible-heading)
  (kbd "C-K")    '("jump to above heading"   . org-previous-visible-heading))

(evil-define-key 'insert vterm-mode-map
  (kbd "C-S-c")  '("copy" . vterm-yank)
  (kbd "C-S-v")  '("paste" . vterm-xterm-paste))

(define-key evil-command-line-map
  (kbd "C-S-v")  '("paste" . evil-paste-after))

; Alt+m key bindings
(bind-keys
 :prefix "M-m"
 :prefix-map launchpad-keys
 ("h" . +doom-dashboard/open)
 ("f" . consult-recent-file)
 ("s" . doom/switch-to-scratch-buffer)
 ("i" . ibuffer)
 ("w" . make-frame-command)
 ("t" . todo-show)
 ("e" . eval-defun)
 ("r" . revert-buffer-fine-no-confirm))


(dotimes (i 5)
  (let ((arg (1+ i)))
    (defalias (intern (format "dashboard-open-recent-file-by-arg-%d" arg))
      (lambda () (interactive) (dashboard-open-recent-file-by-arg arg nil))
      (format "open recent file #%d" arg))))
(evil-define-key `normal +doom-dashboard-mode-map
  (kbd "1") 'dashboard-open-recent-file-by-arg-1
  (kbd "2") 'dashboard-open-recent-file-by-arg-2
  (kbd "3") 'dashboard-open-recent-file-by-arg-3
  (kbd "4") 'dashboard-open-recent-file-by-arg-4
  (kbd "5") 'dashboard-open-recent-file-by-arg-5)

(provide 'init-keybinding)

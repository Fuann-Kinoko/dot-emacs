;;; configs/keybinding.el -*- lexical-binding: t; -*-
;;; Code:

;; i don't need esc prefix, so that's it
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Embark settings
(setopt embark-quit-after-action nil)
(map! :map 'minibuffer-mode-map
      "M-e"           #'embark-act
      "C-o"           #'my/delete-word-backward
      "C-w"           #'my/delete-word-backward
      "C-<backspace>" #'my/delete-word-backward)

;; remap yes or no -> or or no :)
(define-key y-or-n-p-map      "o" 'act)
(define-key query-replace-map "o" 'act)

                                        ; ================== custom functions ==================

;; (defun my-ace-sneak ()
;;   "Simulate gs SPC for sneak-like cursor jump."
;;   (interactive)
;;   (let ((current-prefix-arg t))
;;     (evil-avy-goto-char-timer)))

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
    ;;         (lambda () (setopt vr--regexp-string sel))))  ;;
    ;; (call-interactively 'vr/query-replace))))           ;;
    (query-replace sel
                   (completing-read (format "Replace \"%s\" to: " sel) ())
                   nil (beginning-of-line))))
(defun moon/query-replace-point ()
  "Query replace thing at point."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    ;; (cl-letf (((symbol-function 'vr--set-regexp-string) ;;
    ;;         (lambda () (setopt vr--regexp-string word)))) ;;
    ;; (call-interactively 'vr/query-replace))))           ;;
    (query-replace word
                   (completing-read (format "Replace \"%s\" to: " word) ())
                   nil (beginning-of-line))))

(defun revert-buffer-quick-no-confirm ()
  "Revert buffer fine without confirm."
  (interactive)
  ;; (revert-buffer-with-fine-grain nil t))
  (revert-buffer t t nil))

(defun repeat-last-complex-command ()
  "Basically `repeat-complex-command' but without confirm."
  (lol)
  (repeat-complex-command 1))

(defun intelligent-close ()
  "Quit a frame the same way no matter what kind of frame you are on."
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (delete-frame (selected-frame))
        ;;close a parent with no children present
        (save-buffers-kill-emacs))
    ;;close a child frame
    (delete-frame (selected-frame))))

(defun reload-current-dired-buffer ()
  "Reload current `dired-mode' buffer."
  (let* ((dir (dired-current-directory)))
    (progn (kill-buffer (current-buffer))
           (dired dir))))

;; delete not kill it into kill-ring
;; _based on_ http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my/delete-word-backward (arg)
  "Delete backward, not triggering `kill-ring', dont bother with `ARG' tho."
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
  "the arg `extra-predicate' is to filter the buffer list in it
   it is describe as a lambda function containing arg `buf'
   please refer to
   `+vertico--workspace-generate-sources' and
   `+vertico/switch-workspace-buffer'
   for more information."
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
(defun my-jump-matching-pair ()
  (interactive)
  (cond ((looking-at "\\s\(") (forward-sexp) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-sexp))))

(defun +pixel-scroll-interpolate-up-or-down (&optional direction lines)
  (interactive)
  (let ((dir_p (if (eq direction 'up)
                   0.99
                 -0.99)))             ; sth bad will happen if it is 1.0/-1.0, the last line may be cutoff
    (if lines
        (pixel-scroll-precision-interpolate (* dir_p lines (pixel-line-height)))
      (pixel-scroll-interpolate-down))))

(defun my/scroll-down-half-page ()
  "scroll down half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
        (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
          ((= ln lmax) (recenter (window-end)))
          ;; (t (progn
          ;;      (+pixel-scroll-interpolate-down
          ;;         'down
          ;;         (/ (window-body-height) 2)))))))
          (t (progn
               (move-to-window-line -1)
               (recenter))))))

(defun my/scroll-up-half-page ()
  "scroll up half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
        (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
          ((= ln lmax) (move-to-window-line nil))
          ;; (t (progn
          ;;      (+pixel-scroll-interpolate-down
          ;;         'up
          ;;         (/ (window-body-height) 2)))))))
          (t (progn
               (move-to-window-line 0)
               (recenter))))))

;;@@ 文字计数，可以处理中文和英文
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; http://www.emacswiki.org/emacs/download/basic-toolkit.el
(defun count-ce-words (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end))
    (setq total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (CN: %d, EN: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

(defun meow-repeat-expand-10 ()
  "I don't want to hit 0 because it changes the mind painfully."
  (interactive)
  (meow-expand 0))

                                        ; ================== bindings ==================

(after! meow
  (setopt meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("<escape>" . ignore)
   '("/"        . isearch-forward)
   '(";"        . meow-reverse)
   '(","        . meow-inner-of-thing)
   '("."        . meow-bounds-of-thing)
   '("["        . meow-beginning-of-thing)
   '("]"        . meow-end-of-thing)
   '("b"        . meow-back-word)
   '("B"        . meow-back-symbol)
   '("e"        . meow-next-word)
   '("E"        . meow-next-symbol)
   '("j"        . meow-next)
   '("J"        . meow-next-expand)
   '("k"        . meow-prev)
   '("K"        . meow-prev-expand)
   '("n"        . meow-search)
   '("o"        . meow-block)
   '("O"        . meow-to-block)
   '("v"        . meow-visit)
   '("w"        . meow-mark-word)
   '("W"        . meow-mark-symbol)
   '("x"        . meow-line)
   '("X"        . meow-goto-line)
   '("m"        . meow-join)
   '("y"        . meow-save)
   '("Y"        . meow-sync-grab)
   '("z"        . meow-pop-selection)
   '("'"        . repeat)
   '("<escape>" . ignore)
   )
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   ;; '("j" . "H-j")
   ;; '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("e"        . dirvish-side)
   '("SPC"      . projectile-find-file)
   '("1"        . meow-digit-argument)
   '("2"        . meow-digit-argument)
   '("3"        . meow-digit-argument)
   '("4"        . meow-digit-argument)
   '("5"        . meow-digit-argument)
   '("6"        . meow-digit-argument)
   '("7"        . meow-digit-argument)
   '("8"        . meow-digit-argument)
   '("9"        . meow-digit-argument)
   '("0"        . meow-digit-argument)
   '("/"        . meow-keypad-describe-key)
   '("?"        . meow-cheatsheet)
   )
  (meow-normal-define-key
   '("0"        . meow-expand-0)
   '("9"        . meow-expand-9)
   '("8"        . meow-expand-8)
   '("7"        . meow-expand-7)
   '("6"        . meow-expand-6)
   '("5"        . meow-expand-5)
   '("4"        . meow-expand-4)
   '("3"        . meow-expand-3)
   '("2"        . meow-expand-2)
   '("1"        . meow-expand-1)
   '("/"        . isearch-forward)
   '(";"        . meow-reverse)
   '(","        . meow-inner-of-thing)
   '("."        . meow-bounds-of-thing)
   '("["        . meow-beginning-of-thing)
   '("]"        . meow-end-of-thing)
   '("a"        . meow-append)
   '("A"        . meow-open-below)
   '("b"        . meow-back-word)
   '("B"        . meow-back-symbol)
   '("c"        . meow-change)
   '("d"        . meow-delete)
   '("D"        . meow-backward-delete)
   '("e"        . meow-next-word)
   '("E"        . meow-next-symbol)
   '("f"        . meow-find)
   '("h"        . meow-left)
   '("H"        . meow-left-expand)
   '("i"        . meow-insert)
   '("I"        . meow-open-above)
   '("j"        . meow-next)
   '("J"        . meow-next-expand)
   '("k"        . meow-prev)
   '("K"        . meow-prev-expand)
   '("l"        . meow-right)
   '("L"        . meow-right-expand)
   '("m"        . meow-join)
   '("n"        . meow-search)
   '("o"        . meow-block)
   '("O"        . meow-to-block)
   '("p"        . meow-yank)
   ;;
   ;;
   '("r"        . meow-replace)
   '("R"        . meow-swap-grab)
   '("s"        . meow-kill)
   '("t"        . meow-till)
   '("u"        . meow-undo)
   '("U"        . meow-undo-in-selection)
   '("v"        . meow-visit)
   '("w"        . meow-mark-word)
   '("W"        . meow-mark-symbol)
   '("x"        . meow-line)
   '("X"        . meow-goto-line)
   '("y"        . meow-save)
   '("Y"        . meow-sync-grab)
   '("z"        . meow-pop-selection)
   '("'"        . repeat)
   '("C-o"      . meow-pop-to-mark)
   '("C-i"      . meow-unpop-to-mark)
   '("<escape>" . ignore)
   )
  )

;; (general-evil-setup)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setopt avy-timeout-seconds 0.4)
(bind-keys
 :prefix "C-w"
 :prefix-map window-management-keys
 ("C-w" . other-window)
 ("x"   . delete-window)
 ("k"   . delete-window)
 ("s"   . split-window-below)
 ("v"   . split-window-right)
 ("o"   . delete-other-windows))

(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c n") 'doom/toggle-narrow-buffer)
(global-set-key (kbd "C-v")   'my/scroll-down-half-page)
(global-set-key (kbd "M-v")   'my/scroll-up-half-page)
(global-set-key (kbd "C-x _") 'comment-kill)
(global-set-key (kbd "M-d")   'hippie-expand)
(global-set-key (kbd "M-o")   'forward-sexp)
(global-set-key (kbd "M-i")   'backward-sexp)
(global-set-key (kbd "M-O")   'up-list)
(global-set-key (kbd "M-n")   `consult-flycheck)
(global-set-key (kbd "<f8>")  'project-shell)


;; normal keybindings:
(after! meow
  (meow-normal-define-key
   '("-"        . evilnc-comment-or-uncomment-lines)
   '("C-r"      . undo-redo)
   '("M-L"      . er/expand-region)
   '("M-H"      . er/contract-region)
   '("M-e"      . embark-act)
   '("M-b"      . my/switch-workspace-buffer-no-dired)
   '("M-d"      . my/switch-workspace-buffer-only-dired)
   '("<escape>" . meow-cancel-selection)
   '("g"        . nil)
   '("gh"       . +lookup/documentation)
   '("gd"       . +lookup/definition)
   '("gl"       . align-regexp)
   '("G"        . end-of-buffer)
   '("q"        . meow-grab)
   '("gg"       . beginning-of-buffer)
   '("gv"       . exchange-point-and-mark)
   '("gc"       . count-ce-words)
   '("%"        . my-jump-matching-pair)
   '("@"        . meow-beacon-apply-kmacro)
   ;; '("C-j"   . my/scroll-down-half-page)
   ;; '("C-k"   . my/scroll-up-half-page)
   '("TAB"      . nil)
   '("_"        . comment-dwim)
   '("S"        . embrace-add)
   '("\\"       . avy-goto-char-timer)
   '("`"        . meow-repeat-expand-10)
   )
  (meow-motion-define-key
   '("M-e"      . embark-act)
   '("M-b"      . my/switch-workspace-buffer-no-dired)
   '("M-d"      . my/switch-workspace-buffer-only-dired)
   '("g"        . nil)
   '("gh"       . +lookup/documentation)
   '("gd"       . +lookup/definition)
   '("G"        . end-of-buffer)
   '("gg"       . beginning-of-buffer)
   '("h"        . meow-left)
   '("l"        . meow-right)
   '("TAB"      . nil)
   ;; '("C-j"      . my/scroll-down-half-page)
   ;;
   )
  )

;; (evil-define-key 'normal 'global
;;   "J"   'back-to-indentation
;;   "K"   'evil-last-non-blank
;;   "gh" '+lookup/documentation
;;   "gb"  'eval-defun
;;   "-"   'evilnc-comment-or-uncomment-lines
;;   ;; (kbd "C-o")    '("jump to back"   . (lambda () (interactive) (better-jumper-jump-backward) (recenter-top-bottom)))
;;   ;; (kbd "C-i")    '("jump to fore"   . (lambda () (interactive) (better-jumper-jump-forward) (recenter-top-bottom)))
;;   (kbd "C-s")    '("jump to below"              . save-buffer)
;;   ;; (kbd "s")      '("sneak"                      . my-ace-sneak)
;;   (kbd "C-j")    '("jump to below"              . sp-next-sexp)
;;   (kbd "C-k")    '("jump to above"              . backward-up-list)
;;   (kbd "C-B")    '("replace word"               . moon/query-replace-point)
;;   (kbd "C-L")    '("multi next"                 . evil-multiedit-match-and-next)
;;   (kbd "C-S-L")  '("multi all"                  . evil-multiedit-match-all)
;;   (kbd "M-L")    '("smart enlarge"              . er/expand-region)
;;   (kbd "M-e")    '("embark"                     . embark-act)
;;   (kbd "M-n")    '("consult notes"              . consult-notes)
;;   ;; (kbd "M-p")    '("paste previous"          . evil-paste-pop) ;; this is replaced by C-p
;;   (kbd "M-b")    '("buffers"                    . my/switch-workspace-buffer-no-dired)
;;   (kbd "M-d")    '("direds"                     . my/switch-workspace-buffer-only-dired)
;;   (kbd "M-H")    '("smart shrink"               . er/contract-region)
;;   (kbd "M-w")    '("alt workspace"              . +workspace/switch-to)
;;   (kbd "SPC fn") '("yank file name"             . my-yank-file-name)
;;   (kbd "SPC e")  '("dirvish side"               . dirvish-side)
;;   (kbd "M-<f4>") '("dirvish side"               . intelligent-close)
;;   (kbd "<f8>")   '("next error"                 . next-error)
;;   (kbd "C-<backspace>") '("delete without copy" . my/delete-word-backward))

(global-unset-key (kbd "C-;"))

;; (map! :leader
;;        :desc "join line      " "j" #'evil-join
;;        :desc "buffer-vertico " "," #'my/switch-workspace-buffer-no-dired)

;; (evil-define-key 'visual 'global
;;   (kbd "C-L")    '("multi next"     . evil-multiedit-match-and-next)
;;   (kbd "C-S-L")  '("multi all"      . evil-multiedit-match-all)
;;   (kbd "C-B")    '("replace region" . moon/query-replace-region)
;;   "J"   'back-to-indentation
;;   "K"   'evil-last-non-blank
;;   "-"   'evilnc-comment-or-uncomment-lines)

;; (after! info
;;   (evil-define-key 'normal Info-mode-map
;;     "J"   'back-to-indentation
;;     "K"   'evil-last-non-blank))

(meow-define-keys 'insert
  '("C-SPC" . comint-dynamic-complete-filename)
  '("M-e"   . embark-act)
  '("C-o"   . my/delete-word-backward)
  '("C-s"   . save-buffer)
  '("C-S-V" . yank)
  )
;; (evil-define-key 'insert 'global
;;   (kbd "C-SPC")  '("complete filename" . comint-dynamic-complete-filename)
;;   (kbd "M-e")    '("embark"            . embark-act)
;;   (kbd "C-o")    '("delete word"       . my/delete-word-backward)
;;   (kbd "M-y")    '("yasnippet expand"  . yas-expand)
;;   (kbd "C-s")    '("save buffer"       . save-buffer)
;;   (kbd "C-S-V")  '("paste"             . evil-paste-after))

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
       :desc "revert"              "r"   #'revert-buffer-quick-no-confirm))

;; dired keybindings:
(map! :leader
      (:prefix ("d" . "dired")
       :desc "open dired in current file" "d" #'dired-jump
       :desc "jump history" "j" #'dirvish-history-jump))
(after! dired
  (define-key dired-mode-map "h" `dired-up-directory)
  (define-key dired-mode-map "l"         'dired-find-file)
  (define-key dired-mode-map "-"         `dired-do-kill-lines)
  (define-key dired-mode-map "w"         `dirvish-layout-toggle)
  (define-key dired-mode-map "r"         `revert-buffer)
  (define-key dired-mode-map "R"         `wdired-change-to-wdired-mode)

  (define-key dired-mode-map "X"         `dired-do-rename)
  (define-key dired-mode-map "o"         `dirvish-quick-access)
  (define-key dired-mode-map "i"         `dirvish-file-info-menu)
  (define-key dired-mode-map "y"         `dirvish-yank-menu)
  (define-key dired-mode-map "f"         `dirvish-narrow)
  (define-key dired-mode-map ","         `dirvish-quicksort)
  (define-key dired-mode-map "."         `dired-omit-mode)
  (define-key dired-mode-map (kbd "s")   `dirvish-fd)
  (define-key dired-mode-map (kbd "TAB") `other-window)
  (define-key dired-mode-map (kbd "M-l") `dirvish-ls-switches-menu)
  (define-key dired-mode-map (kbd "M-m") `dirvish-mark-menu)
  (define-key dired-mode-map (kbd "M-t") `dirvish-layout-toggle)
  (define-key dired-mode-map (kbd "M-s") `dirvish-setup-menu)
  (define-key dired-mode-map (kbd "M-e") `dirvish-emerge-menu)
  (define-key dired-mode-map (kbd "M-j") `dirvish-fd-jump)
  (define-key dired-mode-map (kbd "TAB") `dirvish-toggle-subtree))

                                        ; haskell repl(lol) mode
;; (evil-define-key 'normal haskell-mode-map
;;   (kbd "gk")     '("check info" . haskell-process-do-info)
;;   (kbd "SPC lc") '("load the repl" . haskell-process-load-file))
;; (evil-define-key 'insert haskell-lol-mode-map
;;   (kbd "C-l")   `haskell-interactive-mode-clear
;;   (kbd "<up>")  `haskell-interactive-mode-history-previous
;;   (kbd "<down>")`haskell-interactive-mode-history-next)

                                        ; idris repl(interactive) mode
;; (evil-define-key 'insert idris-repl-mode-map
;;   (kbd "C-l")   `idris-repl-clear-buffer
;;   (kbd "<up>")  `idris-repl-backward-history
;;   (kbd "<down>")`idris-repl-forward-history)

;; (evil-define-key 'normal pdf-view-mode-map
;;   (kbd "j")  (lambda() (interactive) (pdf-view-next-line-or-next-page 2))
;;   (kbd "k")  (lambda() (interactive) (pdf-view-previous-line-or-previous-page 2))
;;   (kbd "d")  (lambda() (interactive) (pdf-view-next-line-or-next-page 8))
;;   (kbd "u")  (lambda() (interactive) (pdf-view-previous-line-or-previous-page 8)))

;; (evil-define-key 'normal vterm-mode-map
;;   (kbd "M-m w")  '("new frame" . make-frame-command))

;; (evil-define-key 'normal org-mode-map
;;   (kbd "C-J")    '("jump to below heading"   . org-next-visible-heading)
;;   (kbd "C-K")    '("jump to above heading"   . org-previous-visible-heading))

;; (evil-define-key 'insert vterm-mode-map
;;   (kbd "C-S-c")  '("copy" . vterm-yank)
;;   (kbd "C-S-v")  '("paste" . vterm-xterm-paste))

;; (evil-define-key 'normal compilation-mode-map
;;   (kbd "M-n")    '("compilation next error" . compilation-next-error)
;;   (kbd "M-p")    '("compilation previous error" . compilation-previous-error))
;; (evil-define-key 'normal compilation-shell-minor-mode-map
;;   (kbd "M-n")    '("compilation next error" . compilation-next-error)
;;   (kbd "M-p")    '("compilation previous error" . compilation-previous-error))

;; (define-key evil-command-line-map
;;   (kbd "C-S-v")  '("paste" . evil-paste-after))

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
 ("r" . revert-buffer-quick-no-confirm))

(defun +my-dash-board-toggle-recent-file ()
  "Toggle whether show the recent file or not, default to no."
  (interactive)
  (progn
    (when (boundp '+doom-dashboard-functions)
      (when (listp +doom-dashboard-functions)
        (if (member 'custom-dashboard-widget-recent-file +doom-dashboard-functions)
            ;; If custom-dashboard-widget-recent-file exists, remove both widgets
            (setq +doom-dashboard-functions
                  (remove 'custom-dashboard-widget-dash-seperator
                          (remove 'custom-dashboard-widget-recent-file +doom-dashboard-functions)))
          ;; If custom-dashboard-widget-recent-file does not exist, add both widgets
          (let ((banner-pos (cl-position 'doom-dashboard-widget-banner +doom-dashboard-functions)))
            (when banner-pos
              (setq +doom-dashboard-functions
                    (append (cl-subseq +doom-dashboard-functions 0 (1+ banner-pos))
                            '(custom-dashboard-widget-dash-seperator custom-dashboard-widget-recent-file)
                            (cl-subseq +doom-dashboard-functions (1+ banner-pos)))))))))
    (+doom-dashboard/open (selected-frame))
    ))

(dotimes (i 5)
  (let ((arg (1+ i)))
    (defalias (intern (format "dashboard-open-recent-file-by-arg-%d" arg))
      (lambda () (interactive) (dashboard-open-recent-file-by-arg arg nil))
      (format "open recent file #%d" arg))))

(dotimes (i 9)
  (global-set-key (kbd (format "M-%d" (1+ i)))
                  (lambda () (interactive)
                    (funcall (intern (format "+workspace/switch-to-%d" i))))))

(bind-keys
 :map +doom-dashboard-mode-map
 ("1" . dashboard-open-recent-file-by-arg-1)
 ("2" . dashboard-open-recent-file-by-arg-2)
 ("3" . dashboard-open-recent-file-by-arg-3)
 ("4" . dashboard-open-recent-file-by-arg-4)
 ("5" . dashboard-open-recent-file-by-arg-5)
 ("r" . +my-dash-board-toggle-recent-file)
 )


;; (evil-define-key `normal +doom-dashboard-mode-map
;;   (kbd "1") 'dashboard-open-recent-file-by-arg-1
;;   (kbd "2") 'dashboard-open-recent-file-by-arg-2
;;   (kbd "3") 'dashboard-open-recent-file-by-arg-3
;;   (kbd "4") 'dashboard-open-recent-file-by-arg-4
;;   (kbd "5") 'dashboard-open-recent-file-by-arg-5)

(provide 'init-keybinding)

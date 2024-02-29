(use-package! better-jumper
    :custom
    ; avoiding conflict with evils jumping stuff
    (better-jumper-use-evil-jump-advice nil)

    :config
    (better-jumper-mode 1)

    ; this lets me toggle between two points. (adapted from evil-jump-backward-swap)
    (evil-define-motion better-jumper-toggle (count)
      (let ((pnt (point)))
        (better-jumper-jump-backward 1)
        (better-jumper-set-jump pnt)))

    ; this is the key here. This advice makes it so you only set a jump point
    ; if you move more than one line with whatever command you call. For example
    ; if you add this advice around evil-next-line, you will set a jump point
    ; if you do 10 j, but not if you just hit j. I did not write this code, I
    ; I found it a while back and updated it to work with better-jumper.
    (defun my-jump-advice (oldfun &rest args)
      (let ((old-pos (point)))
        (apply oldfun args)
        (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))
                  1)
          (better-jumper-set-jump old-pos))))

    ; jump scenarios
    (advice-add 'evil-next-line :around #'my-jump-advice)
    (advice-add 'evil-previous-line :around #'my-jump-advice)
    (advice-add 'evil-goto-definition :around #'my-jump-advice)
    (advice-add 'evil-goto-mark  :around #'my-jump-advice)
    (advice-add 'evil-goto-line  :around #'my-jump-advice)
    (advice-add 'consult-imenu  :around #'my-jump-advice)
    (advice-add '+lookup/references  :around #'my-jump-advice)
    (advice-add '+lookup/definition  :around #'my-jump-advice)
    (advice-add 'backward-up-list  :around #'my-jump-advice)
    (advice-add 'forward-list  :around #'my-jump-advice)
    (advice-add 'avy-goto-char-timer  :around #'my-jump-advice))

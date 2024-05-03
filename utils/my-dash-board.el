(defvar recent-file-list-nr 1)
(defvar custom-dashboard-widget-blank-seperator "\n\n")

(defun custom-dashboard-widget-dash-seperator (&optional real-width)
  (let* ((width (or real-width (current-column)))
         (margin (max 0 (floor (/ (- +doom-dashboard--width
                                     width)
                                  2)))))
    (beginning-of-line)
    (insert (make-string +doom-dashboard--width ?-))
    (insert "\n")
    (end-of-line)))

(defun custom-dashboard-widget-space-seperator (&optional real-width)
  (let* ((width (or real-width (current-column)))
         (margin (max 0 (floor (/ (- +doom-dashboard--width
                                     width)
                                  2)))))
    (beginning-of-line)
    (insert (make-string +doom-dashboard--width ? ))
    (insert "\n")
    (end-of-line)))

(defun recent-file-inserter (list-display-name list)
  (when (car list)
    (setq recent-file-list-nr 1)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n          ")
            (let ((button-text
                   (format "%2s %s" (number-to-string
                                     recent-file-list-nr)
                           (abbreviate-file-name el))))
              (widget-create 'push-button
                             :action (lambda (&rest _)
                                       (find-file-existing el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text))
            (setq recent-file-list-nr
                  (1+ recent-file-list-nr)))
          list)))

(defun filter-ignore-recent-files (path)
  "check if a path should be ignored and not appear in the recent file list"
  (let ((expanded-path (expand-file-name path))
        (expanded-prefix-cargo (expand-file-name "~/.cargo"))
        (expanded-prefix-autosave (expand-file-name "~/.emacs.d/.local/etc/workspaces/autosave")))
    (and
      (not (string-prefix-p expanded-prefix-cargo expanded-path))
      (not (string-prefix-p expanded-prefix-autosave expanded-path))
      t)))

(defun custom-dashboard-widget-recent-file ()
  (recentf-mode 1)
  (let ((recent-files-list
          (seq-take (seq-filter 'filter-ignore-recent-files recentf-list) 5)))
    (when (recent-file-inserter
           "Recent Files:"
           recent-files-list)
      t))
  (insert custom-dashboard-widget-blank-seperator))

(defun dashboard-open-recent-file-by-arg (arg is_force)
  "when in dashboard mode, enter the file selecting by arg (arg started by 1)"
  (interactive)
  (when
    (or is_force (eq major-mode '+doom-dashboard-mode))
    (let*
      ((recent-files-list (seq-take (seq-filter 'filter-ignore-recent-files recentf-list) 5))
       (selected-file (expand-file-name (nth (- arg 1) recent-files-list))))
      (find-file selected-file))))

;; thare are also hooks added in ~/.config/doom/configs/packages/auto-dark.el
(setq fancy-splash-image
 (if (auto-dark--is-dark-mode)
   "~/.config/doom/banner3.pbm"
   "~/.config/doom/banner3_inv.pbm"
   ))

(setq +doom-dashboard-functions `(custom-dashboard-widget-space-seperator
                                  doom-dashboard-widget-banner
                                  custom-dashboard-widget-dash-seperator
                                  custom-dashboard-widget-recent-file
                                  doom-dashboard-widget-loaded))

(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))

(provide 'my-dash-board)

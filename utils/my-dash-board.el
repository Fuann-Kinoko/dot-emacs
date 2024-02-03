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

(defun custom-dashboard-widget-recent-file ()
  (recentf-mode 1)
  (let ((recent-files-list
         (seq-take recentf-list 5)))
    (when (recent-file-inserter
           "Recent Files:"
           recent-files-list)
      t))
  (insert custom-dashboard-widget-blank-seperator))

(setq fancy-splash-image "~/.config/doom/banner3.pbm")
(setq +doom-dashboard-functions `(custom-dashboard-widget-space-seperator
                                  doom-dashboard-widget-banner
                                  custom-dashboard-widget-dash-seperator
                                  custom-dashboard-widget-recent-file
                                  doom-dashboard-widget-loaded))

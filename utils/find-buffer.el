;;; utils/find-buffer.el -*- lexical-binding: t; -*-


(defun find-buffer-or-recentf-candidates ()
  "Return candidates for `find-buffer-or-recentf'."
  (let ((buffers
         (delq nil
               (mapcar (lambda (b)
                         (when (buffer-file-name b)
                           (buffer-file-name b)))
                       (buffer-list)))))
    (append
     buffers
     (cl-remove-if (lambda (f) (member f buffers))
                   (mapcar #'substring-no-properties recentf-list)))))

(defun find-buffer-or-recentf ()
  "Find a buffer visiting a file or file on `recentf-list'."
  (interactive)
  (let ((file (completing-read "Buffer File or Recentf: "
                               (find-buffer-or-recentf-candidates)
                               nil t)))
    (if (bufferp file)
        (switch-to-buffer file)
      (find-file file))))

;;; utils/workspacePermanent.el -*- lexical-binding: t; -*-


(setq workspace-minibuffer-enabled nil)

(after! persp-mode
  (defun display-workspaces-in-minibuffer ()
    (when workspace-minibuffer-enabled
      (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (+workspace--tabline)))))
  (setq my-idle-workspace-timer
        (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer))
  (+workspace/display))


(map! :leader (:when (modulep! :ui workspaces)
        (:prefix-map ("TAB" . "workspace")
        :desc "toggle minibuffer show" "t" #'toggle-workspace-in-minibuffer)))


(defun toggle-workspace-in-minibuffer ()
  "Toggle the value of `display-workspace-permanent`."
  (interactive)
  (setq workspace-minibuffer-enabled (not workspace-minibuffer-enabled))
  (if workspace-minibuffer-enabled
      ; true
      (setq my-idle-workspace-timer (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer))
      ; false
      (progn
        (cancel-timer my-idle-workspace-timer)
        (with-current-buffer " *Minibuf-0*" (erase-buffer)))))
;; 下面两个是最终手段，都能用
;; (cancel-timer my-idle-workspace-timer) (with-current-buffer " *Minibuf-0*" (erase-buffer))
;; (cancel-function-timers 'display-workspaces-in-minibuffer) (with-current-buffer " *Minibuf-0*" (erase-buffer))

;;; utils/switchTheme.el -*- lexical-binding: t; -*-

(defgroup auto-dark nil
  "Automatically changes Emacs theme acording to Windows dark-mode status."
  :group 'tools
  :prefix "auto-dark-*")

(defcustom auto-dark-dark-theme 'doom-gruvbox
  "The theme to enable when dark-mode is active."
  :group 'auto-dark
  :type 'symbol)

(defcustom auto-dark-light-theme 'doom-flatwhite
  "The theme to enable when dark-mode is inactive."
  :group 'auto-dark
  :type 'symbol)

(defcustom auto-dark-polling-interval-seconds 5
  "The number of seconds between which to poll for dark mode state.
Emacs must be restarted for this value to take effect."
  :group 'auto-dark
  :type 'integer)

(defcustom auto-dark-allow-powershell t
  "Wheter to allow function `auto-dark-mode' to shell out to powershell:
to check dark-mode state."
  :group 'auto-dark
  :type 'boolean)

(defcustom auto-dark-detection-method `powershell
  "The method auto-dark should use to detect the system theme.

Defaults to nil and will be populated through feature detection
if left as such.  Only change this value if you know what you're
doing!"
  :group 'auto-dark
  :type 'symbol
  :options '(powershell))

(defvar auto-dark--last-dark-mode-state 'unknown)

(defvar auto-dark--dbus-listener-object nil)

(defun auto-dark--is-dark-mode-powershell ()
  "Invoke powershell using Emacs using external shell command."
  (string-equal "0" (string-trim (shell-command-to-string "powershell -noprofile -noninteractive \
-nologo -ex bypass -command Get-ItemPropertyValue \
HKCU:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize \
-Name AppsUseLightTheme"))))

(defvar auto-dark-dark-mode-hook nil
  "List of hooks to run after dark mode is loaded." )

(defvar auto-dark-light-mode-hook nil
  "List of hooks to run after light mode is loaded." )

(defun auto-dark--is-dark-mode ()
  "If dark mode is enabled."
  (cond (t (auto-dark--is-dark-mode-powershell))))

(defun auto-dark--check-and-set-dark-mode ()
  "Set the theme according to the OS's dark mode state.
In order to prevent flickering, we only set the theme if we haven't
already set the theme for the current dark mode state."
  (let ((appearance (if (auto-dark--is-dark-mode) 'dark 'light)))
    (unless (eq appearance auto-dark--last-dark-mode-state)
      (auto-dark--set-theme appearance))))

(defun auto-dark--set-theme (appearance)
  "Set light/dark theme using emacs-plus ns-system-appearance.
Argument APPEARANCE should be light or dark."
  (mapc #'disable-theme custom-enabled-themes)
  (setq auto-dark--last-dark-mode-state appearance)
  (pcase appearance
    ('dark
     (disable-theme auto-dark-light-theme)
     (load-theme auto-dark-dark-theme t)
     (run-hooks 'auto-dark-dark-mode-hook))
    ('light
     (disable-theme auto-dark-dark-theme)
     (load-theme auto-dark-light-theme t)
     (run-hooks 'auto-dark-light-mode-hook))))

(defvar auto-dark--timer nil)
(defun auto-dark-start-timer ()
  "Start auto-dark timer."
  (setq auto-dark--timer
        (run-with-timer 0 auto-dark-polling-interval-seconds #'auto-dark--check-and-set-dark-mode)))

(defun auto-dark-stop-timer ()
  "Stop auto-dark timer."
  (when (timerp auto-dark--timer)
    (cancel-timer auto-dark--timer)))
(defun auto-dark--register-change-listener ()
  "Register a listener to listen for the system theme to change."
  (cond
   ((auto-dark--use-ns-system-appearance)
    (add-hook 'ns-system-appearance-change-functions #'auto-dark--set-theme))
   (t (auto-dark-start-timer))))

(defun auto-dark--unregister-change-listener ()
  "Remove an existing listener for the system theme."
  (cond
   (t (auto-dark-stop-timer))))

(defun auto-dark--use-ns-system-appearance ()
  "Determine whether we should use the ns-system-appearance-* functions."
  (boundp 'ns-system-appearance-change-functions))

(defun auto-dark--determine-detection-method ()
  "Determine which theme detection method auto-dark should use."
  (cond (t `powershell)))

;;;###autoload
(define-minor-mode auto-dark-mode
  "Toggle `auto-dark-mode' on or off."
  :group 'auto-dark
  :global t
  :lighter " AD"
  (if auto-dark-mode
      (progn
        (unless auto-dark-detection-method
          (setq auto-dark-detection-method (auto-dark--determine-detection-method)))
        (auto-dark--check-and-set-dark-mode)
        (auto-dark--register-change-listener))
    (auto-dark--unregister-change-listener)))

(provide 'auto-dark)

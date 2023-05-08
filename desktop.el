(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun efs/config-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-move-window 2)
                (exwm-workspace-switch-create 2))
  )
)

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
      "feh" nil "feh --bg-scale /usr/share/backgrounds/Mirror_by_Uday_Nakade.jpg"))

(defun efs/exwm-init-hook ()
  ;; make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Launch app that will run in the background
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueman-applet")
)

(defun efs/update-displays ()
  (efs/run-in-background "autorandr --change --force")
  ;; (message "Display config: %s" (string-trim (shell-command-to-string "autorandr --current")))
)

(use-package! exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Config window as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/config-window-by-class)

  ;; Rebind CapsLock to Ctrl
  ;; (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;; xrandr --output DisplayPort-0 --primary --mode 1920x1080 --rate 144.00 --output DVI-D-0 --mode 1920x1080 --rate 60.00 --right-of DisplayPort-0
  (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --rate 60.00 --output HDMI-2 --mode 1920x1080 --rate 60.00 --right-of eDP-1")

  ;; This will need to be updated to the name of a display!  You can find
  ;; the names of your displays by looking at arandr or the output of xrandr
  ;; (setq exwm-randr-workspace-monitor-plist '(2 "HDMI-2"))
  (setq exwm-randr-workspace-monitor-plist
    (pcase (system-name)
      ("duyhustvn" '(2 "HDMI-2"))
      ))

  ;; react to display connectivity changes, do initial display update
  (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
  (efs/update-displays)

  ;; set wallpaper
  (efs/set-wallpaper)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 20)
  (exwm-systemtray-enable)

  ;; warp cursor
  (setq exwm-workspace-warp-cursor t)

  ;; These keys should always pass through to Emacs
  ;; for example: when firefox is running in buffer
  ;; press M-x will go to emacs not firefox
  (setq exwm-input-prefix-keys
    '(?\C-x ;; ?\C => Ctrl
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-enable))


(use-package! desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))
(defun sodcof/natural-scroll-activated ()
  (start-process-shell-command "xinput" nil "xinput set-prop 'pointer:ELAN0676:00 04F3:3195 Touchpad' 'libinput Natural Scrolling Enabled' 1")
)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun sodcof/touchpad-tapping-activated ()
  (start-process-shell-command "xinput" nil "xinput set-prop 'pointer:ELAN0676:00 04F3:3195 Touchpad' 'libinput Tapping Enabled' 1")
)

(defun efs/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
      "feh" nil "feh --bg-scale /usr/share/backgrounds/Mirror_by_Uday_Nakade.jpg"))

(defun sodcof/get-list-monitors()
  ;; Get a list of connected monitors
  (setq all-monitor (shell-command-to-string "xrandr --query | grep ' connected' | cut -d ' ' -f1"))
  ; => "eDP-1
  ; DP-1
  ; "

  ; t -> remove null/empty
  (split-string all-monitor "\n" t) ; => ("eDP-1" "DP-1")
)


(defun sodcof/set-up-multiple-monitor()
  (interactive)
  (let ((monitors (sodcof/get-list-monitors)))
    ;; (message "current monitors: %s" monitors)
    (if (> (length monitors) 1)
        (progn
          (setq main-monitor (nth 0 monitors))
          (setq attached-monitor (nth 1 monitors))
          ;;(message "attached-monitor: %s" attached-monitor)
          ;;(message "main-monitor: %s" main-monitor)

          ;; Using xrandr to setup multiple monitor
          ;; xrandr --output eDP-1 --primary --mode 1920x1080 --rate 60.00 --output DP-2 --mode 1920x1080 --rate 60.00 --right-of eDP-1
          ;; xrandr --output eDP-1 --primary --mode 1920x1080 --rate 60.00 --output HDMI-1 --mode 1920x1080 --rate 60.00 --above eDP-1

          (if (string= attached-monitor 'HDMI-1)
              (setq position "--above")
              (setq position "--right-of"))

          (setq xrandr-command (format "xrandr --output %s --primary --mode 1920x1080 --rate 60.00 --output %s --mode 1920x1080 --rate 60.00 %s %s" main-monitor attached-monitor position main-monitor))

          (message "xrandr command: %s" xrandr-command)
          ;; execute xrandr command
          ;; example: (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --rate 60.00 --output DP-1 --mode 1920x1080 --rate 60.00")
          (start-process-shell-command "xrandr" nil xrandr-command)

          ;; set workspace to monitor
          ;; (setq exwm-randr-workspace-monitor-plist '(2 "HDMI-1"))
          ;; set workspace 2 to monitor HDMI-1
          (setq exwm-randr-workspace-monitor-plist (list 2 attached-monitor))
        )
    )
  )
)

(defun efs/update-displays ()
  ;; update display when plug/unplug sencond screen
  (efs/run-in-background "autorandr --change --force")
  ;; (message "Display config: %s" (string-trim (shell-command-to-string "autorandr --current")))
)

(defun sodcof/ssh-auth-sock-exists-p()
  ;; check if SSH_AUTH_SOCK enviroment variable is set and points to a valid socket
  (let ((auth-sock (getenv "SSH_AUTH_SOCK")))
    (and auth-sock (file-exists-p auth-sock) auth-sock))
)

(defun sodcof/ssh-auth-enable()
  (if (sodcof/ssh-auth-sock-exists-p)
      (start-process-shell-command "ssh-agent" nil "ssh-agent")
  )
)

(defun sodcof/ansi-term-zsh ()
  (interactive)
  (ansi-term "/usr/bin/zsh")
)

(defun sodcof/ansi-term-bash ()
  (interactive)
  (ansi-term "/usr/bin/bash")
)

(defun sodcof/nautilus ()
  (interactive)
  (start-process-shell-command "/usr/bin/nautilus" nil "/usr/bin/nautilus")
)

(defun efs/exwm-update-class ()
  ;; Set the name of buffer to the name of oppened application
  (exwm-workspace-rename-buffer exwm-class-name))


(defun efs/exwm-update-title ()
  ;; Improve buffer name
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun efs/config-window-by-class ()
  ;; move application to workspace by the application name
  ;; firefox go to space 2
  ;; thunderbird go to space 3
  ;; etc
  (interactive)
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-move-window 2)(exwm-workspace-switch-create 2))
    ("thunderbird" (exwm-workspace-move-window 3)(exwm-workspace-switch-create 3))
    ("ViberPC" (exwm-workspace-move-window 4)(exwm-workspace-switch-create 4))
    ("TelegramDesktop" (exwm-workspace-move-window 4)(exwm-workspace-switch-create 4))
    ("Postman" (exwm-workspace-move-window 5)(exwm-workspace-switch-create 5))
    ("DBeaver" (exwm-workspace-move-window 6)(exwm-workspace-switch-create 6))
    ("RESP.app - Developer GUI for Redis" (exwm-workspace-move-window 7)(exwm-workspace-switch-create 7))
    ("VirtualBox Manager" (exwm-workspace-move-window 8)(exwm-workspace-switch-create 8))
    ("Vmplayer" (exwm-workspace-move-window 8)(exwm-workspace-switch-create 8))
    ("VirtualBox Machine" (exwm-workspace-move-window 9)(exwm-workspace-switch-create 9))
))

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-init-hook ()
  ;; start some application by default
  (sodcof/startup-program)
)


(use-package! exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)

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

  (sodcof/set-up-multiple-monitor)

  ;; react to display connectivity changes, do initial display update
  (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
  (efs/update-displays)

  ;; set wallpaper
  (efs/set-wallpaper)

  ;; enable natural scroll
  (sodcof/natural-scroll-activated)

  ;; enable touchpad tapping
  (sodcof/touchpad-tapping-activated)

  ;; enable ssh-agent
  (sodcof/ssh-auth-enable)

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
          ([C-s-left] . windmove-left)
          ([C-s-right] . windmove-right)
          ([C-s-up] . windmove-up)
          ([C-s-down] . windmove-down)

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

  (exwm-input-set-key (kbd "s-a") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-e") 'sodcof/nautilus)
  (exwm-input-set-key (kbd "s-t") 'sodcof/ansi-term-zsh)
  (exwm-enable))

(defun sodcof/disable-desktop-notification()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\"")
)

(defun sodcof/enable-desktop-notification()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\"")
)

(defun sodcof/toggle-desktop-notification()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\"")
)

(use-package! desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(defun sodcof/startup-program()
  (exwm-workspace-switch-create 4)
  (split-window-right)
  (windmove-right)

  (start-process-shell-command "thunderbird" nil "thunderbird")
  (start-process-shell-command "dropbox" nil "dropbox start -i")
  (start-process-shell-command "viber" nil "/opt/viber/Viber")
  (start-process-shell-command "telegram-desktop" nil "telegram-desktop")


  ;; Launch app that will run in the background
  (efs/run-in-background "dunst")
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueman-applet")
)

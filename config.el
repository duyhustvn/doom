;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "duyle"
      user-mail-address "duyleitbka95@gmail.com")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(global-set-key (kbd "s-h") 'dap-hydra)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; M-x treemacs-load-theme to set theme for treemacs
(setq doom-themes-treemacs-theme 'Idea)

;; (setq debug-on-error t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; VISUAL
(use-package! all-the-icons)
(use-package! all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
)

(use-package! diminish
  :config
  (diminish 'visual-line-mode)
)

(use-package! dimmer
  ;; dim the inactive window
  :custom (dimmer-fraction 0.2) ;; the bigger the dimmer-fraction is, the darker the inactive windown is
  :config
  (dimmer-mode 1)
)

(use-package! paren
  :config (show-paren-mode)
  :custom (show-paren-style 'expression)
)

(use-package! rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode))
)

;; display only opening project
(use-package! treemacs
  :hook (treemacs . 'treemacs-display-current-project-exclusively)
)

(use-package! treemacs-projectile
  :after (treemacs projectile)
)

(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-continuous-scroll-mode t)
)

(after! doom-modeline
  (setq doom-modeline-buffer-encoding nil)
  ;; (setq doom-modeline-env-enable-python nil)

  (setq display-time-default-load-average nil)      ; don't show load average
  (display-battery-mode 1)
  (setq display-time-day-and-date t)
  (display-time-mode 1)
)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\env3\\'")
  ;; or
  ;; (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.my-files\\'")
)

(use-package! dap-mode
  ;; :init
  ;; code here will be run immediately
  :config
  ;; code here will be run after the package is loaded

  ;; (setq dap-ui-buffer-configurations
  ;;     `((,"*dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
  ;;       (,"*dap-ui-repl*" . ((side . right) (slot . 1) (window-width . 0.50))) ;; added this! TODO enable when release on MELPA
  ;;       (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.20)))
  ;;       (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.20)))
  ;;       (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
  ;;       (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))

  (dap-ui-mode 1)
  (dap-ui-many-windows-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  ;;
  (require 'dap-dlv-go)
)

(after! dap-dlv-go
  ;; Eval Buffer with `M-x eval-buffer' to register the newly created template.
  (dap-register-debug-template
   "Go: Custom Launch File"
   (list :type "go"
         :cwd (lsp-workspace-root)
        :request "launch"
        :name "Go: Custom Launch File"
        :mode "auto"
        :program "main.go"
        :buildFlags nil
        :args nil
        :env nil))
)

(after! dap-mode
  (require 'dap-python)
  (setq dap-python-executable "python3")
  (setq dap-python-debugger 'debugpy)

  ;; Eval Buffer with `M-x eval-buffer' to register the newly created template.
  (dap-register-debug-template
   "Django: Debug Template"
   (list :type "python"
         :args "runserver --noreload 8083" ;; update
         :cwd (lsp-workspace-root)
         :request "launch"
         :name "Django: Debug Template"
         :program "manage.py"
         ;; :env '(("PYTHONPATH" . "env3/bin/python"))
         ;; :pythonPath "venv/bin/python" ;; update to the path to virtual environment
         :django t))

  (dap-register-debug-template
   "Celery: Debug Template"
   (list :type "python"
         :cwd (lsp-workspace-root)
         :request "launch"
         :name "Celery: Celery Easm"
         :program ".direnv/python-3.7.16/bin/celery" ;; update path to celery
         :args "--app=app worker --hostname=scan-node@%%n -l info -Q scan --purge --without-mingle --without-gossip --without-heartbeat -Ofair" ;; update module name
         :django t))
)

(after! go-mode
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package! undo-tree
  :config
  (global-undo-tree-mode +1)
)

(use-package! engine-mode
  :config
  (setq engine/browser-function 'browse-url-firefox)
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
  (engine-mode 1)
)

(use-package! counsel
  :config
  (counsel-mode 1)
)

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1) ;; wrap line
)

(use-package! org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
        '("~/Code/org/tasks.org"
          "~/Code/org/birthdays.org"))

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)
      ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  (efs/org-font-setup))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

; (use-package! visual-fill-column
;   :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.config/doom/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

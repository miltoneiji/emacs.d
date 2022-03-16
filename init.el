;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; Before installing ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; - Make sure you have the Hack font installed
;; - Make sure you have `gcc` installed (to compile emacs sql)
;; - Make sure you have `ag` and `fd` installed (to make projectile faster)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In the first opening ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - Run `M-x all-the-icons-install-fonts`

;;;;;;;;;;;;;;;;;;;;;
;; Useful commands ;;
;;;;;;;;;;;;;;;;;;;;;

;; - =C-h k= shows what command is executed by a key binding
;; - =C-h= is very helpful when trying to understand things

;; Allow 100MB of memory before calling garbage collection. This means CG runs less often,
;; which speeds up some operations.
(setq gc-cons-threshold (* 100 1024 1024))

;;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loading my preferences
(load-file "~/.emacs.d/tk-defaults.el")
(tk-defaults/use-all-settings)

;; Get rid of the scroll jumpiness
(setq scroll-step           1
      scroll-conservatively 10000)

;; Set Tab indent as 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Increasing the amount of data which Emacs reads
;; from the process. It increases the LSP performance
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb

;; use-package setup
;; the =use-package= macro allows you to isolate package configuration in your
;; .emacs file in a way that is both performance-oriented and, well, tidy.
;;
;; - when using =:hook= omit the "-hook" suffix.
;; - use =:init= to execute code before a package is loaded (so, this will run
;;   even if the package is deferred).
;; - use =:config= to execute code after a package is loaded.
(straight-use-package 'use-package)
;; avoid having to add :straight t in all use-package statements
(setq straight-use-package-by-default t)

;; To gather statistics about how many packages you've loaded, how much time
;; they've spent, etc.
;; Just uncomment this, restart Emacs and run =M-x use-package-report=
;;(setq use-package-compute-statistics t)

;; Keep ~/.emacs.d clean
;; This should be called as early as possible
(use-package no-littering
  :config (require 'no-littering))

;; ensure environmental variables inside Emacs look the same as in the user's shell
(use-package exec-path-from-shell
  :config
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

;; evil
(use-package evil
  :config
  (evil-mode 1)
  :custom
  ;; without this, TAB does not work properly
  (evil-want-C-i-jump nil)
  ;; necessary for evil-collection
  (evil-want-keybinding nil))

(use-package evil-escape
  :config
  (evil-escape-mode 1)
  :custom
  (evil-escape-key-sequence "jk"))

(use-package evil-collection
  :config
  (evil-collection-init 'dired)
  (evil-collection-init 'ivy)
  (evil-collection-init 'magit))

;; =general.el= provides a more convenient method for binding keys in emacs.
(defconst my-local-leader ",")

;; Unbinding SPC since it will be used as a prefix
(define-key evil-motion-state-map (kbd "SPC") nil)

(use-package general)

(general-create-definer my-local-leader-def :prefix my-local-leader)

;; it is recommended that you install =fd= and =ag= to makes things faster
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (general-define-key :prefix "SPC"
                      :states 'motion
                      "p" '(:ignore t :which-key "project")
                      "p p" '(projectile-switch-project :which-key "Switch project")
                      "p l" '(projectile-discover-projects-in-search-path :which-key "Discover projects")
                      "p i" '(projectile-invalidate-cache :which-key "Invalidate cache")
                      "SPC" '(projectile-find-file :which-key "Find file")
                      "p /" '(projectile-ag :which-key "Search")
                      "p D" '(projectile-dired-other-window :which-key "Dired"))

  (defun tk/add-project-paths (paths)
    "Add project paths if they exist."
    (setq projectile-project-search-path
          (seq-filter #'file-directory-p paths)))
  (tk/add-project-paths '("~/repos")))

;; this remaps projectile commands to use counsel
;(use-package counsel-projectile
;  :init (counsel-projectile-mode))

;; Search
;; useful:
;;   - ag-kill-buffers: "Kill all ag-mode buffers."
;;   - ag-project-files: "Search using ag for a given literal search STRING, limited to files that match FILE-TYPE. STRING defaults to the symbol under point."
;;   - ag-project: "Guess the root of the current project and search it with ag for the given literal search STRING."
;;   - ag-files: "Search using ag in a given DIRECTORY"
;;
;; winnow:
;;   - m: show only files that match
;;   - x: exclude files that match
;;   - recompile: sdf
(use-package winnow)
(use-package ag
  :hook ((ag-mode . winnow-mode))
  :custom
  (ag-highlight-search t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; text completion framework for Emacs. It uses pluggable back-ends and front-ends to retrieve
;; and display completion candidates.
(use-package company
  :hook ((after-init . global-company-mode))
  :custom
  (company-idle-delay 0))

;; a generic completion mechanism for Emacs
(use-package ivy
  :init (ivy-mode)
  :custom
  (ivy-use-selectable-prompt t))

;; a collection of Ivy-enhanced versions of common Emacs commands
(use-package counsel
  ;; Enabling =counsel-mode= remaps built-in Emacs funcions that have counsel replacement
  :init (counsel-mode))

;; a minor mode that displays the key bindings following your currently entered incomplete
;; command in a popup.
(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  :custom
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10))

;; template system for Emacs
(use-package yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Font
(defun tk/set-font ()
  "Set the font according to the Operating System."
  (set-face-attribute 'default nil :family "Hack")

  (if (eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :height 135)
    (set-face-attribute 'default nil :height 155)))

(tk/set-font)

(use-package doom-themes
  :config
  (load-theme 'doom-vibrant t)
  ;; Enable custom neotree theme
  (doom-themes-neotree-config)
  ;; Corrects (and i proves) org-mode's native fontification.
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

(setq tk/themes '(doom-one doom-one-light))
(setq tk/themes-index 0)

(defun tk/cycle-theme ()
  (interactive)
  (setq tk/themes-index (% (1+ tk/themes-index) (length tk/themes)))
  (tk/load-indexed-theme))

(defun tk/load-indexed-theme ()
  (tk/try-load-theme (nth tk/themes-index tk/themes)))

(defun tk/try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))
    (message "Unable to find theme file for ‘%s’" theme)))

;; After installing, you will need to run =M-x all-the-icons-install-fonts=
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; File and project explorer
(use-package treemacs
  :hook ((treemacs-mode . (lambda() (display-line-numbers-mode -1))))
  :custom
  (treemacs-width 30)
  (treemacs-show-hidden-files t)
  (treemacs-resize-icons 22)
  (treemacs-RET-actions-config treemacs-doubleclick-actions-config)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  :config
  (general-define-key :prefix "SPC"
                      :states 'motion
                      "p t" '(treemacs :which-key "Toggle file explorer")))
(use-package treemacs-evil)
(use-package treemacs-projectile)
(use-package treemacs-magit)

;; Show inserted/modified/deleted lines in files that have version control
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((ruby-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;;;;;;;;;
;; Org ;;
;;;;;;;;;

(use-package org
  :hook (org-mode . tk/org-mode-setup)
  :config
  (defun tk/org-mode-setup ()
    ;; disable automatic line breaking
    (auto-fill-mode 0)
    ;; wrap lines
    (visual-line-mode t)
    ;; indent text according to outline structure
    (org-indent-mode))
  :custom
  (org-ellipsis "...")
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAIT" "CODE-REVIEW" "HOLD" "|" "DONE" "DELEGATED" "CANCELLED")))
  ;; =M-x list-colors-display= to see colors
  (org-todo-keyword-faces '(("TODO" . "chartreuse4")
                            ("IN-PROGRESS" . "green")
                            ("WAIT" . "orange2")
                            ("CODE-REVIEW" . "orange2")
                            ("HOLD" . "firebrick3")
                            ("DONE" . "dim gray"))))

;; Pure asthetics
(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :custom
  (org-bullets-bullet-list '("◉" "○")))

;;;;;;;;;;
;; Json ;;
;;;;;;;;;;

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq-default js-indent-level 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
  :hook ((after-init . smartparens-global-mode))
  :config
  (require 'smartparens-config)
  (general-define-key :prefix "SPC"
                      :states 'normal
                      "k" '(:ignore t :which-key "smartparens")
                      "k s" '(sp-forward-slurp-sexp :which-key "slurp")
                      "k r" '(sp-raise-sexp :which-key "raise")
                      "k L" '(sp-forward-sexp :which-key "next expression")))

;; Syntax checking
(use-package flycheck
  :hook ((after-init . global-flycheck-mode)))

(use-package magit
  :config
  (general-define-key :prefix "SPC"
                      :states 'motion
                      "g" '(:ignore t :which-key "git")
                      "g b" '(magit-blame :which-key "Blame")
                      "g l" '(magit-log-current :which-key "Log")
                      "g L" '(magit-log-buffer-file :which-key "Log [for file]")
                      "g s" '(magit-status :which-key "Status")))

;; revert buffers when their files/state have changed on disk
(use-package autorevert
  :straight (:type built-in)
  :hook (after-init . global-auto-revert-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global key bindings ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common key bindings
(define-key evil-motion-state-map (kbd "SPC TAB") '("Previous" . evil-switch-to-windows-last-buffer))
(define-key evil-motion-state-map (kbd "SPC :") '("M-:" . execute-extended-command))
(define-key evil-motion-state-map (kbd "SPC ;") '("Eval expression" . pp-eval-expression))

(general-define-key :prefix "SPC"
                    :states 'motion
                    "f" '(:ignore t :which-key "file")
                    "f f" '(find-file :which-key "Find file")
                    "f p" '((lambda () (interactive) (find-file user-init-file)) :which-key "Open init.el")
                    "f r" '((lambda () (interactive) (load-file user-init-file)) :which-key "Reload init.el"))

(general-define-key :prefix "SPC"
                    :states 'motion
                    "b" '(:ignore t :which-key "buffer")
                    "b b" '(switch-to-buffer :which-key "Switch to buffer")
                    "b d" '(kill-buffer :which-key "Kill buffer")
                    "b p" '(previous-buffer :which-key "Previous buffer")
                    "b n" '(next-buffer :which-key "Next buffer"))

(general-define-key :prefix "SPC"
                    :states 'motion
                    "u" '(:ignore t :which-key "util")
                    "u g" '(straight-freeze-versions :which-key "Generate lockfile")
                    "u e" '(straight-thaw-versions :which-key "Ensure lockfile"))

(general-define-key :prefix "SPC"
                    :states 'motion
                    "t" '(:ignore t :which-key "toggle")
                    "t t" '(tk/cycle-theme :which-key "Toggle theme"))

;; Toggle buffer fullscreen
(define-key evil-motion-state-map (kbd "C-f") 'tk/window-split-toggle-one-window)

;; panel resize
(define-key evil-motion-state-map (kbd "C-S-<right>") (lambda () (interactive) (enlarge-window 10 t)))
(define-key evil-motion-state-map (kbd "C-S-<left>")  (lambda () (interactive) (enlarge-window -10 t)))
(define-key evil-motion-state-map (kbd "C-S-<up>")    (lambda () (interactive) (enlarge-window 10)))
(define-key evil-motion-state-map (kbd "C-S-<down>")  (lambda () (interactive) (enlarge-window -10)))

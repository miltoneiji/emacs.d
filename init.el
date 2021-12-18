;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; Before installing ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; - Make sure you have the Hack font installed
;; - Make sure you have the `~/repos/tk-wiki` directory (used by org-roam)
;; - Make sure you have `gcc` installed (to compile emacs sql)
;; - Make sure you have `ag` and `fd` installed (to make projectile faster)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In the first opening ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - Run `M-x all-the-icons-install-fonts`
;; - Run `M-x plantuml-download-jar`

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
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; use-package setup
;; the =use-package= macro allows you to isolate package configuration in your
;; .emacs file in a way that is both performance-oriented and, well, tidy.
;;
;; - when using =:hook= omit the "-hook" suffix.
;; - use =:init= to execute code before a package is loaded (so, this will run
;;   even if the package is deferred).
;; - use =:config= to execute code after a package is loaded.
(straight-use-package 'use-package)

;; To gather statistics about how many packages you've loaded, how much time
;; they've spent, etc.
;; Just uncomment this, restart Emacs and run =M-x use-package-report=
;;(setq use-package-compute-statistics t)

;; Keep ~/.emacs.d clean
;; This should be called as early as possible
(use-package no-littering
  :straight t
  :config (require 'no-littering))

;; ensure environmental variables inside Emacs look the same as in the user's shell
(use-package exec-path-from-shell
  :straight t
  :config
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

;; evil
(use-package evil
  :straight t
  :config
  (evil-mode 1)
  :custom
  ;; without this, TAB does not work properly
  (evil-want-C-i-jump nil)
  ;; necessary for evil-collection
  (evil-want-keybinding nil))

(use-package evil-escape
  :straight t
  :config
  (evil-escape-mode 1)
  :custom
  (evil-escape-key-sequence "jk"))

(use-package evil-collection
  :straight t
  :config
  (evil-collection-init 'dired)
  (evil-collection-init 'ivy)
  (evil-collection-init 'magit))

;; =general.el= provides a more convenient method for binding keys in emacs.
(defconst my-local-leader ",")

;; Unbinding SPC since it will be used as a prefix
(define-key evil-motion-state-map (kbd "SPC") nil)

(use-package general
  :straight t)

(general-create-definer my-local-leader-def :prefix my-local-leader)

;; it is recommended that you install =fd= and =ag= to makes things faster
(use-package projectile
  :straight t
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
  (tk/add-project-paths '("~/repos" "~/dev/nu")))

;; this remaps projectile commands to use counsel
(use-package counsel-projectile
  :straight t
  :init (counsel-projectile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; text completion framework for Emacs. It uses pluggable back-ends and front-ends to retrieve
;; and display completion candidates.
(use-package company
  :straight t
  :hook ((after-init . global-company-mode))
  :custom
  (company-idle-delay 0))

;; a generic completion mechanism for Emacs
(use-package ivy
  :straight t
  :init (ivy-mode)
  :custom
  (ivy-use-selectable-prompt t))

;; a collection of Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :straight t
  ;; Enabling =counsel-mode= remaps built-in Emacs funcions that have counsel replacement
  :init (counsel-mode))

;; a minor mode that displays the key bindings following your currently entered incomplete
;; command in a popup.
(use-package which-key
  :straight t
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
(use-package yasnippet
  :straight t)

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
  :straight t
  :config
  (load-theme 'doom-one t)
  ;; Enable custom neotree theme
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
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
(use-package all-the-icons
  :straight t)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package neotree
  :straight t
  :hook (neotree-mode . (lambda ()
                          (display-line-numbers-mode -1)
                          (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                          (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                          (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
                          (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
                          ;; for some reason, this is not working
                          (define-key evil-normal-state-local-map (kbd "r") 'neotree-rename-node)))
  :config
  (defun tk/toggle-neotree ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

  (general-define-key :prefix "SPC"
                      :states 'motion
                      "p t" '(tk/toggle-neotree :which-key "Toggle neotree"))
  :custom
  (neo-show-hidden-files t))

;; Show inserted/modified/deleted lines in files that have version control
(use-package git-gutter-fringe
  :straight t
  :config
  (global-git-gutter-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((ruby-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

;;;;;;;;;;
;; Ruby ;;
;;;;;;;;;;

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

(use-package clojure-mode
  :straight t)

(use-package clj-refactor
  :straight t
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (yas-minor-mode 1))))

(use-package cider
  :straight t
  :config
  (add-to-list 'cider-test-defining-forms "defflow")
  (general-define-key
   :states 'normal
   :keymaps 'cider-repl-mode-map
   ", c" '(cider-repl-clear-buffer :which-key "Clean buffer"))
  :hook (clojure-mode . (lambda ()
                          (define-clojure-indent
                            (flow 1))))
  :general
  (my-local-leader-def 'normal 'override
    "a" #'cider-jack-in
    ;; eval
    "e f" #'cider-eval-defun-at-point
    "e e" #'cider-eval-last-sexp
    "e b" #'cider-eval-buffer
    ;; repl
    "r b" #'cider-switch-to-repl-buffer
    ;; test
    "t t" #'cider-test-run-test)
  :custom
  (cider-repl-display-help-banner nil))



;;;;;;;;;
;; Org ;;
;;;;;;;;;

(use-package org
  :straight t
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
  :straight t
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :custom
  (org-bullets-bullet-list '("◉" "○")))

;; Second brain
;; It may be necessary to call =org-roam-db-clear-all= and a sync after
;; you use it in another pc.
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :config
  (general-define-key :prefix "SPC"
                      :states 'motion
                      "o" '(:ignore t :which-key "org")
                      "o i" '(org-roam-node-insert :which-key "Insert node")
                      "o f" '(org-roam-node-find :which-key "Find node")
                      "o j" '((lambda () (interactive) (find-file "~/repos/tk-wiki/journal.org")) :which-key "Journal")
                      "o a" '((lambda () (interactive) (find-file "~/repos/tk-wiki/fleeting.org")) :which-key "Fleeting")
                      "o b" '(org-roam-buffer-toggle :which-key "Backlinks"))
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-db-location "~/.emacs.d/org-roam.db")
  (org-roam-list-files-commands '(fd))
  (org-roam-directory "~/repos/tk-wiki/pages")
  (org-roam-dailies-directory "journal/"))

;; dependencies for org-roam-ui
(use-package websocket
  :straight t)
(use-package simple-httpd
  :straight t)
(use-package f
  :straight t)
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :config
  (general-define-key :prefix "SPC"
                      :states 'motion
                      "o u" '(org-roam-ui-mode :which-key "UI mode")))

;;;;;;;;;;
;; Json ;;
;;;;;;;;;;

(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :config
  (setq-default js-indent-level 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ledger-mode
  :straight t)

(use-package olivetti
  :straight t)

(use-package smartparens
  :straight t
  :hook ((after-init . smartparens-global-mode))
  :config
  (require 'smartparens-config)
  (general-define-key :prefix "SPC"
                      :states 'normal
                      "k" '(:ignore t :which-key "smartparens")
                      "k s" '(sp-forward-slurp-sexp :which-key "slurp")
                      "k r" '(sp-raise-sexp :which-key "raise")
                      "k L" '(sp-forward-sexp :which-key "next expression")))

(use-package flycheck
  :straight t
  :hook ((after-init . global-flycheck-mode)))

(use-package magit
  :straight t
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

;; The first time you run this package, you will need to run
;; plantuml-download-jar
(use-package plantuml-mode
  :straight t
  :commands plantuml-download-jar
  :mode "\\.plantuml\\'"
  :general
  (my-local-leader-def 'normal 'override
    "p" #'plantuml-preview)
  :custom
  (plantuml-default-exec-mode 'jar))

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

;; panel resize
(define-key evil-motion-state-map (kbd "C-S-<right>") (lambda () (interactive) (enlarge-window 10 t)))
(define-key evil-motion-state-map (kbd "C-S-<left>")  (lambda () (interactive) (enlarge-window -10 t)))
(define-key evil-motion-state-map (kbd "C-S-<up>")    (lambda () (interactive) (enlarge-window 10)))
(define-key evil-motion-state-map (kbd "C-S-<down>")  (lambda () (interactive) (enlarge-window -10)))

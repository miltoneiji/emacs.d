;;; init.el --- Startup file for Emacs      -*- lexical-binding: t; -*-

;;; Commentary:

;;; =C-h= is your friend!

;;; Dependencies:
;;; - Hack font
;;; - ripgrep for searching
;;; - fd for searching for files

;;; Code:

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
(load-file "./.emacs.d/tk-defaults.el")
(tk-defaults/use-all-settings)

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

(use-package general)

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
  (evil-collection-init 'magit))

;; Unbinding SPC since it will be used as a prefix
(define-key evil-motion-state-map (kbd "SPC") nil)

(general-create-definer map-local!
  :states  'motion
  :keymaps 'override
  :prefix  ",")

;;;;;;;;;;;;;;;;;;;;;;;;
;; Project management ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defun tk/add-project-paths (paths)
    "Add PATHS to projectile projects path if they exist."
    (setq projectile-project-search-path
          (seq-filter #'file-directory-p paths)))

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
                      "p /" '(consult-ripgrep :which-key "Search"))
  (tk/add-project-paths '("~/repos")))

;;;;;;;;;;;;;;;;;
;;; Completion ;;
;;;;;;;;;;;;;;;;;

(use-package vertico
  :init
  (vertico-mode))

;; Marginalia adds annotations to the completion candidates.
(use-package marginalia
  :init
  (marginalia-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; This package provides an orderless completion style that divides the pattern into
;; space-separated components, and matches candidates that match all of the components
;; in any order.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Provides search and navigation commands.
(use-package consult)

;; text completion framework for Emacs. It uses pluggable back-ends and front-ends to retrieve
;; and display completion candidates.
(use-package company
  :hook ((after-init . global-company-mode))
  :custom
  (company-idle-delay 0))

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

;;;;;;;;;;
;; Font ;;
;;;;;;;;;;

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(use-package doom-themes
  :config
  (setq-default doom-themes-enable-bold t)
  (setq-default doom-themes-enable-italic t)
  (load-theme 'doom-one-light t)

  (doom-themes-org-config))

(defun tk/set-font ()
  "Set the font according to the Operating System."
  (set-face-attribute 'default nil :family "Hack")

  (if (eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :height 135)
    (set-face-attribute 'default nil :height 155)))
(tk/set-font)

(use-package mood-line
  :config
  (mood-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File and project explorer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :hook ((treemacs-mode . (lambda() (display-line-numbers-mode -1))))
  :custom
  (treemacs-width 30)
  (treemacs-show-hidden-files t)
  (treemacs-resize-icons 22)
  (treemacs-RET-actions-config treemacs-doubleclick-actions-config)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-no-png-images t)
  :config
  (general-define-key :prefix "SPC"
                      :states 'motion
                      "p t" '(treemacs :which-key "Toggle file explorer")))
(use-package treemacs-evil)
(use-package treemacs-projectile)
(use-package treemacs-magit)

;;;;;;;;;
;; Org ;;
;;;;;;;;;

(use-package org
  :hook (org-mode . tk/org-mode-setup)
  :config
  (map-local! org-mode-map
    "e"   '(:ignore t :which-key "eval")
    "e f" 'org-babel-execute-maybe
    "e b" 'org-babel-execute-buffer
    "'"   'org-edit-special
    "c"   'org-babel-remove-result-one-or-many
    "s"   'org-download-screenshot)

  (defun tk/org-mode-setup ()
    ;; disable automatic line breaking
    (auto-fill-mode 0)
    ;;
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (sqlite . t)))
    ;; wrap lines
    (visual-line-mode t)
    ;; indent text according to outline structure
    (org-indent-mode))
  :custom
  (org-ellipsis "...")

  (org-confirm-babel-evaluate nil)
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "DELEGATED" "CANCELLED")))
  ;; =M-x list-colors-display= to see colors
  (org-todo-keyword-faces '(("TODO" . "chartreuse4")
                            ("IN-PROGRESS" . "green")
                            ("WAITING" . "orange2")
                            ("DONE" . "dim gray"))))

(use-package olivetti)

;; Pure asthetics
(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :custom
  (org-bullets-bullet-list '("◉" "○")))

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

(defun tk/clean-repl ()
  "Cleans repl buffer without having to change to it."
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-repl-clear-buffer)
  (cider-switch-to-last-clojure-buffer))

(defun tk/clojure-custom-indent ()
  "Define custom indent for Clojure."
  (define-clojure-indent
    (facts 1)
    (fact 1)))

(use-package clojure-mode)

(use-package cider
  :init
  (add-hook 'cider-mode-hook #'tk/clojure-custom-indent)
  :custom
  (cider-repl-display-help-banner nil)
  :config
  (map-local! clojure-mode-map
    "a"   'cider-jack-in
    "e"   '(:ignore t :which-key "eval")
    "e f" 'cider-eval-defun-at-point
    "e e" 'cider-eval-last-sexp
    "e b" 'cider-eval-buffer
    "r"   '(:ignore t :which-key "repl")
    "r b" 'cider-switch-to-repl-buffer
    "r c" 'tk/clean-repl
    "t"   '(:ignore t :which-key "test")
    "t t" 'cider-test-run-test))

(use-package clj-refactor
  :hook ((clojure-mode . (lambda ()
			   (clj-refactor-mode 1)
			   (yas-minor-mode 1)))))

;;;;;;;;;;;
;; Elisp ;;
;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (map-local! emacs-lisp-mode-map
	      "e"   '(:ignore t :which-key "eval")
	      "e e" 'eval-last-sexp
	      "e b" 'eval-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frontend development ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package protobuf-mode)
(use-package yaml-mode)
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

;; This is used by some modes to initialize a file with some content.
;; e.g. new files created in clojude-more starts with (ns ...)
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Version control ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other key bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tk/open-config ()
  "Open init.el."
  (interactive)
  (find-file user-init-file))

(defun tk/load-config ()
  "Load my init.el."
  (interactive)
  (load-file user-init-file))

;; Common key bindings
(define-key evil-motion-state-map (kbd "SPC TAB") '("Previous" . evil-switch-to-windows-last-buffer))
(define-key evil-motion-state-map (kbd "SPC :")   '("M-:" . execute-extended-command))
(define-key evil-motion-state-map (kbd "SPC ;")   '("Eval expression" . pp-eval-expression))

(general-define-key :prefix "SPC"
                    :states 'motion
                    "f" '(:ignore t :which-key "file")
                    "f f" '(find-file :which-key "Find file")
                    "f p" '(tk/open-config :which-key "Open init.el")
                    "f r" '(tk/load-config :which-key "Reload init.el"))

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
                    "t r" '(toggle-truncate-lines :which-key "Toggle truncate lines")
                    "t l" '(display-line-numbers-mode :which-key "Toggle line number"))

;;;;;;;;;;;;;;;;;;;;;;
;; Windows resizing ;;
;;;;;;;;;;;;;;;;;;;;;;

(define-key evil-motion-state-map (kbd "C-S-h") 'shrink-window-horizontally)
(define-key evil-motion-state-map (kbd "C-S-r") 'enlarge-window-horizontally)
(define-key evil-motion-state-map (kbd "C-S-j") 'shrink-window)
(define-key evil-motion-state-map (kbd "C-S-k") 'enlarge-window)

;;; init.el ends here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(ledger-reports
   '((nil "ledger ")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

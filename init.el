;;; init.el --- Startup file for Emacs      -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Allow 100MB of memory before calling garbage collection. This means CG runs less often,
;; which speeds up some operations.
(setq gc-cons-threshold (* 100 1024 1024))

;;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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

(dolist (module '("preferences.el"
		  "tk-modeline.el"))
  (load (concat user-emacs-directory (format "modules/%s" module))))

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

(use-package evil-collection)

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

;;;;;;;;;;;;;;;;;
;;; Minibuffer ;;
;;;;;;;;;;;;;;;;;

;; Minimalistic vertical completion UI based on the default completion system.
(use-package vertico
  :init (vertico-mode))

;; Adds annotations to the completion candidates.
(use-package marginalia
  :init (marginalia-mode))

;; Preview, narrowing, grouping, search, etc.
(use-package consult
  :init
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
	  xref-show-definitions-function #'consult-xref)))

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

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(use-package modus-themes
  :config
  (setq modus-themes-to-toggle '(modus-vivendi modus-operandi)
	;;modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted)
	;;modus-themes-to-toggle '(modus-vivendi-deuteranopia modus-operandi-deuteranopia)
	;;modus-themes-to-toggle '(modus-vivendi-tritanopia modus-operandi-tritanopia)
	modus-themes-bold-constructs nil
	modus-themes-italic-constructs t)
  (modus-themes-load-theme (car modus-themes-to-toggle))

  (general-define-key :prefix "SPC"
		      :states 'motion
		      "t t" '(modus-themes-toggle :which-key "Toggle theme")))

;;;;;;;;;;
;; Font ;;
;;;;;;;;;;

(set-face-attribute 'default nil :font "Fira Code Retina" :height 200)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 200)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 200)

;;;;;;;;;;;
;; Elisp ;;
;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (map-local! emacs-lisp-mode-map
	      "e"   '(:ignore t :which-key "eval")
	      "e e" 'eval-last-sexp
	      "e b" 'eval-buffer)))

;;;;;;;;;;;;;;;;;
;; Development ;;
;;;;;;;;;;;;;;;;;

;; This is used by some modes to initialize a file with some content.
;; e.g. new files created in clojude-more starts with (ns ...)
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(add-hook 'prog-mode-hook 'hs-minor-mode)
(general-define-key
 :states 'normal
 :keymaps 'hs-minor-mode-map
 :prefix "SPC"
 "h"   '(:ignore t :which-key "hide/show")
 "h h" '(hs-hide-block :which-key "Hide block")
 "h H" '(hs-hide-all :which-key "Hide all")
 "h s" '(hs-show-block :which-key "Show block")
 "h S" '(hs-show-all :which-key "Show all"))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . (lambda ()
		       (evil-local-set-key 'normal (kbd "g d") 'lsp-find-definition)
		       (evil-local-set-key 'normal (kgd "g r") 'lsp-find-references))))
  :config
  (lsp-enable-which-key-integration))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Syntax checking
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

;; For completion popups.
(use-package company
  :hook ((after-init . global-company-mode))
  :custom
  (company-idle-delay 0))

(use-package python-mode
  :custom (python-shell-interpreter "python3.11")
  :hook (python-mode . lsp-deferred))

(defun tk/ts-lint-buffer ()
  (interactive)
  (let ((command "npm run lint . ")
	(current-file-path (buffer-file-name))
	(default-directory "/Users/takamura/repos/comp/comp-app/"))
    (compile (format "%s %s" command current-file-path))))

(defun tk/ts-lint-project ()
  (interactive)
  (let ((command "npm run lint")
	(default-directory "/Users/takamura/repos/comp/comp-app/"))
    (compile command)))

(defun tk/ts-format-project ()
  (interactive)
  (let ((command "npm run format")
	(default-directory "/Users/takamura/repos/comp/comp-app/"))
    (compile command)))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook ((typescript-mode . lsp-deferred))
  :config
  (setq typescript-indent-level 2)
  (map-local! typescript-mode-map
    "l" '(tk/ts-lint-buffer :which-key "Lint buffer")
    "L" '(tk/ts-lint-project :which-key "Lint project")
    "F" '(tk/ts-format-project :which-key "Format project")))

;;;;;;;;;;;
;;; misc ;;
;;;;;;;;;;;

(use-package markdown-mode
  :custom
  (markdown-hide-urls t))

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

;;;;;;;;;;;;;;;;;;;;;
;; Version control ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode))

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
                    "b b" '(consult-buffer :which-key "Switch to buffer")
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

(general-define-key :prefix "SPC"
		    :states 'motion
		    "s" '(:ignore t :which-key "search")
		    "s l" '(consult-line :which-key "in file")
		    "s p" '(consult-ripgrep :which-key "in project"))

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
   '("013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
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

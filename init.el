;  :custom
;; init.el --- Startup file for Emacs      -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; TODO 2024-08-09: Add narrow to modeline

;; Packages
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; Keep ~/.emacs.d clean
(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/")))))

(dolist (module '("preferences.el"
                  "tk-modeline.el"))
  (load (concat user-emacs-directory (format "modules/%s" module))))

;; ensure environmental variables inside Emacs look the same as in the user's shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq (window-system) '(mac ns x))
  :config (exec-path-from-shell-initialize))

(use-package general
  :ensure t)

;; evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  :custom
  ;; without this, TAB does not work properly
  (evil-want-C-i-jump nil)
  ;; necessary for evil-collection
  (evil-want-keybinding nil))

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  :custom
  (evil-escape-key-sequence "jk"))

(use-package evil-collection
  :ensure t)

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

;; TODO 2024-08-06: Try using Emacs's default project manager?

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (general-define-key :prefix "SPC"
                      :states 'motion
                      "p" '(:ignore t :which-key "project")
                      "p p" '(projectile-switch-project :which-key "Switch project")
                      "p l" '(projectile-discover-projects-in-search-path :which-key "Discover projects")
                      "p i" '(projectile-invalidate-cache :which-key "Invalidate cache")
                      "SPC" '(projectile-find-file :which-key "Find file")
                      "p /" '(consult-ripgrep :which-key "Search"))
  (tk/add-project-paths '("~/repos")))

(defun tk/projectile-find-file-with-extension (extension)
  "Return files with EXTENSION within the current project."
  (interactive "sExtension (without dot): ")
  (let* ((project-files (projectile-current-project-files))
         (filtered-files (seq-filter (lambda (file)
                                       (string-suffix-p (concat "." extension) file))
                                     project-files)))
    (find-file (completing-read "File: " filtered-files))))


;; TODO 2024-08-04: Search can be improved.
;; - Search files with a specific extension.
;; - Search files within a specific path.
;; - Search within files with a specific extension.
;; - Search within files within a specific path.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File and project explorer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :ensure t
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
(use-package treemacs-evil
  :ensure t)
(use-package treemacs-projectile
  :ensure t)

;;;;;;;;;;;;;;;;;
;;; Minibuffer ;;
;;;;;;;;;;;;;;;;;

;; Minimalistic vertical completion UI based on the default completion system.
(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (keymap-set vertico-map "S-<up>" #'vertico-previous-group)
  (keymap-set vertico-map "S-<down>" #'vertico-next-group))

;; Adds annotations to the completion candidates.
(use-package marginalia
  :ensure t
  :config (marginalia-mode))

;; Preview, narrowing, grouping, search, etc.
(use-package consult
  :ensure t
  :config
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :config
  (savehist-mode))

;; This package provides an orderless completion style that divides the pattern into
;; space-separated components, and matches candidates that match all of the components
;; in any order.
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; a minor mode that displays the key bindings following your currently entered incomplete
;; command in a popup.
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10))

;; In-buffer completion
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  :custom
  (corfu-cycle t) ;; Allows cycling through candidates
  (corfu-auto t)  ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-echo-documentation 0.25))

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(use-package modus-themes
  :ensure t
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
  :ensure t
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

;;(use-package lsp-mode
;;  :ensure t
;;  :commands (lsp lsp-deferred)
;;  :hook ((lsp-mode . (lambda ()
;;             (evil-local-set-key 'normal (kbd "g d") 'lsp-find-definition)
;;             (evil-local-set-key 'normal (kbd "g r") 'lsp-find-references))))
;;  :config
;;  (lsp-enable-which-key-integration))
;;
;;(use-package lsp-ui
;;  :ensure t
;;  :hook (lsp-mode . lsp-ui-mode)
;;  :custom
;;  (lsp-ui-doc-position 'bottom))

;; Note: Before using the treesit modes, you need to run
;; M-x treesit-install-language-grammar
(use-package treesit
  :ensure nil
  :config
  (setq-default treesit-language-source-alist
                '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"
                                 "master"
                                 "typescript/src"))
                  (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript"
                          "master"
                          "tsx/src"))
                  (python . ("https://github.com/tree-sitter/tree-sitter-python")))))

(use-package eglot
  :ensure nil
  :hook
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (eglot-managed-mode . (lambda ()
                          (evil-local-set-key 'normal (kbd "g d") 'eglot-find-implementation)
                          (evil-local-set-key 'normal (kbd "g r") 'xref-find-references)))
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (setq eglot-autoshutdown t))

;; Syntax checking
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(use-package python-ts-mode
  :ensure nil
  :init
  (defun python-setup ()
    "Python setup."
    (general-define-key :prefix "SPC"
                        :keymaps 'python-base-mode-map
                        :states 'normal
                        "n d" '(py-narrow-to-def :which-key "def")
                        "n l" '(py-narrow-to-block :which-key "block")
                        "n c" '(py-narrow-to-class :which-key "class"))
    (map-local! python-base-mode-map
      "e" '(:ignore t :which-key "eval")
      "e e" '(python-shell-send-statement :which-key "statement")
      "e d" '(python-shell-send-defun :which-key "defun")
      "e r" '(python-shell-send-region :which-key "region")
      "e b" '(python-shell-send-buffer :which-key "buffer")))
  :mode "\\.py\\'"
  :hook ((python-ts-mode . python-setup)))

(use-package pet
  :ensure t
  :ensure-system-package ((dasel . "brew install dasel")
                          (sqlite3 . "brew install sqlite3"))
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)

  (defun python-pet-setup ()
    "Set up my Python environment."
    (setq-local python-shell-interpreter (pet-executable-find "python")
                python-shell-virtualenv-root (pet-virtualenv-root))
    (pet-eglot-setup)
    (eglot-ensure))

  (add-hook 'python-base-mode-hook 'python-pet-setup))

(use-package ruff-format
  :ensure t
  :hook ((python-base-mode . ruff-format-on-save-mode))
  :config
  (map-local! python-base-mode-map
    "f" '(:ignore t :which-key "format")
    "f b" '(ruff-format-buffer :which-key "buffer")
    "f r" '(ruff-format-region :which-key "region")))

;;;;;;;;;;;;;;;;;;;
;; End of Python ;;
;;;;;;;;;;;;;;;;;;;

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

(use-package jtsx
  :ensure t
  :mode (("\\.tsx?\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :hook ((jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :custom
  (typescript-ts-mode-indent-offset 2)
  (jtsx-indent-statement-block-regarding-standalone-parent t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (map-local! mode-map
      "l" '(tk/ts-lint-buffer :which-key "Lint buffer")
      "L" '(tk/ts-lint-project :which-key "Lint proj")
      "F" '(tk/ts-format-project :which-key "Format proj")))
  (defun jtsx-bind-keys-to-jtsx-tsx-mode ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))
  (defun jtsx-bind-keys-to-jtsx-typescript-mode ()
    (jtsx-bind-keys-to-mode-map jtsx-typescript-mode-map))

  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode)
  (add-hook 'jtsx-typescript-mode-hook 'jtsx-bind-keys-to-jtsx-typescript-mode))

;;;;;;;;;;;
;;; misc ;;
;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-hide-urls t))

(use-package yaml-mode
  :ensure t)

(use-package smartparens
  :ensure t
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
  :ensure t
  :config
  (global-git-gutter-mode))

;; revert buffers when their files/state have changed on disk
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;;;;;;;;;;
;; Misc ;;
;;;;;;;;;;

(use-package visual-fill-column
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 120))


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
                    "s p" '(consult-ripgrep :which-key "in project")
                    "s f" '(projectile-find-file :which-key "file")
                    "s F" '(tk/projectile-find-file-with-extension :which-key "file with extension"))

(general-define-key :prefix "SPC"
                    :states 'motion
                    "n" '(:ignore t :which-key "narrowing")
                    "n r" '(narrow-to-region :which-key "region")
                    "n q" '(widen :which-key "quit narrowing"))

;;;;;;;;;;;;;;;;;;;;;;
;; Windows resizing ;;
;;;;;;;;;;;;;;;;;;;;;;

(define-key evil-motion-state-map (kbd "C-S-h") 'shrink-window-horizontally)
(define-key evil-motion-state-map (kbd "C-S-l") 'enlarge-window-horizontally)
(define-key evil-motion-state-map (kbd "C-S-j") 'shrink-window)
(define-key evil-motion-state-map (kbd "C-S-k") 'enlarge-window)

;;; init.el ends here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)

;  :custom
;; init.el --- Startup file for Emacs      -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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
        `((".*" ,(no-littering-expand-var-file-name "auto-save/"))))
  ;; Below doesn't belong to no-littering but since it has the same goal...
  (make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

  (setq backup-directory-alist
        `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))

        auto-save-list-file-prefix
        (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)

        auto-save-file-name-transforms
        `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))))


(dolist (module '("preferences.el"
                  "tk-modeline.el"
                  "setup-org-mode.el"))
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
  (evil-want-C-i-jump nil))

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  :custom
  (evil-escape-key-sequence "jk"))

;; Unbinding SPC since it will be used as a prefix
(define-key evil-motion-state-map (kbd "SPC") nil)

(general-create-definer map-local!
  :states  'motion
  :keymaps 'override
  :prefix  ",")

;;;;;;;;;;;;;;
;; UndoTree ;;
;;;;;;;;;;;;;;

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist
   '(("." . "~/.emacs.d/tmp/undo")))
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Project management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun tk/normal-keymap-set (key prefix-map which-key-text)
  "Set KEY binding for PREFIX-MAP with WHICH-KEY-TEXT."
  (keymap-set evil-normal-state-map
              key prefix-map)
  (which-key-add-key-based-replacements key which-key-text))

(use-package project
  :ensure nil
  :config
  (tk/normal-keymap-set "<SPC> p" project-prefix-map "project"))

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
  (treemacs-wrap-around nil)
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
  :custom
  (consult-async-min-input 0)
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
  :config (savehist-mode))

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
  :ensure nil
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

;;;;;;;;;;;;
;; Search ;;
;;;;;;;;;;;;

;; consult-ripgrep:
;; - To narrow to specific file extensions: `#<find term>#\.tsx'
;; - To narrow to specific file path: `#<find term>#/path'

(general-define-key :prefix "SPC"
                    :states 'motion
                    "SPC" '(consult-fd :which-key "file")
                    "s" '(:ignore t :which-key "search")
                    "s l" '(consult-line :which-key "in file")
                    "s p" '(consult-ripgrep :which-key "in project")
                    "s f" '(consult-fd :which-key "file"))

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

;; Note: Before using the treesit modes, you need to run
;; M-x treesit-install-language-grammar
(use-package treesit
  :ensure nil
  :config
  (setq-default treesit-language-source-alist
                '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"
                                 "v0.20.4"
                                 "typescript/src"))
                  (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript"
                                 "v0.20.4"
                                 "tsx/src"))
                  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"
                                 "v0.20.4"))
                  (python     . ("https://github.com/tree-sitter/tree-sitter-python"
                                 "v0.21.0"))
                  (json       . ("https://github.com/tree-sitter/tree-sitter-json"
                                 "v0.21.0")))))

(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold" :rev "0.1.0")
  :hook ((python-ts-mode . treesit-fold-mode)
         (tsx-ts-mode . treesit-fold-mode)
         (typescript-ts-mode . treesit-fold-mode)
         (js-ts-mode . treesit-fold-mode)
         (json-ts-mode . treesit-fold-mode)
         (treesit-fold-mode . (lambda ()
                                (general-define-key :prefix "SPC"
                                                    :states 'normal
                                                    :keymaps 'local
                                                    "h" '(:ignore t :which-key "hide/show")
                                                    "h h" 'treesit-fold-close
                                                    "h H" 'treesit-fold-close-all
                                                    "h s" 'treesit-fold-open
                                                    "h S" 'treesit-fold-open-all
                                                    "h t" 'treesit-fold-toggle)))))

(use-package eglot
  :ensure nil
  :hook
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (eglot-managed-mode . (lambda ()
                          (evil-local-set-key 'normal (kbd "g d") 'xref-find-definitions)
                          (evil-local-set-key 'normal (kbd "g r") 'xref-find-references)))
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (setq eglot-autoshutdown t))

;; Syntax checking
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

;; This is used by some modes to initialize a file with some content.
;; e.g. new files created in clojude-more starts with (ns ...)
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

;;(use-package flymake-eslint
;;  :ensure t
;;  :functions flymake-eslint-enable
;;  :preface
;;  (defun me/flymake-eslint-enable-maybe ()
;;    "Enable `flymake-eslint' based on the project configuration.
;;Search for the project ESLint configuration to determine whether the buffer
;;should be checked."
;;    (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
;;                (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
;;      (make-local-variable 'exec-path)
;;      (push (file-name-concat root "node_modules" ".bin") exec-path)
;;      (flymake-eslint-enable))))

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(use-package python-ts-mode
  :ensure nil
  :init
  (defun python-setup ()
    "Python setup."
    (setq python-indent-offset 4)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript & Javascript ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package tsx-ts-mode
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package js-ts-mode
  :ensure nil
  :mode (("\\.jsx?\\'" . js-ts-mode))
  :custom
  (js-indent-level 2))

(use-package json-ts-mode
  :ensure nil
  :mode (("\\.json\\'" . json-ts-mode)))

(use-package prettier
  :ensure t
  :hook ((typescript-ts-mode . prettier-mode)
         (tsx-ts-mode . prettier-mode)
         (js-ts-mode . prettier-mode))
  :custom
  (prettier-mode-sync-config-flag nil))

(use-package nvm
  :ensure t)

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

(defun tk/open-inbox ()
  "Open inbox.org."
  (interactive)
  (find-file-other-window "~/repos/org-directory/inbox.org"))

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
                    "f r" '(tk/load-config :which-key "Reload init.el")
                    "f i" '(tk/open-inbox :which-key "Inbox"))

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
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((ts-fold :url "https://github.com/emacs-tree-sitter/ts-fold"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)

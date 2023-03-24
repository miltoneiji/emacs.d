;;; setup-clojure.el -*- lexical-binding: t; -*-

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

(provide 'setup-clojure)

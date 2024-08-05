;;; setup-org-mode.el -*- lexical-binding: t; -*-

(use-package org
  :hook (org-mode . tk/org-mode-setup)
  :config
  (map-local! org-mode-map
    "e"   '(:ignore t :which-key "eval")
    "e f" 'org-babel-execute-maybe
    "e b" 'org-babel-execute-buffer
    "e e" 'org-babel-execute-src-block
    "'"   'org-edit-special
    "c"   'org-babel-remove-result-one-or-many
    "s"   'org-download-screenshot)

  (defun tk/org-mode-setup ()
    (setq org-hide-emphasis-markers t)
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


(provide 'setup-org-mode)

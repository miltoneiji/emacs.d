(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode))
  :config
  (setq org-directory "~/repos/org-directory"
        org-default-notes-file (concat org-directory "/notes.org"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)))

  (setq
   ;; hides bold/italic/etc markers
   org-hide-emphasis-markers t
   ;; hide keywords
   ;;org-hidden-keywords t
   ;; show an alpha symbol instead of \alpha
   org-pretty-entities t
   ;; ellipsis
   org-ellipsis "...")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

;; Show emphasis markers depending on cursor position
(use-package org-appear
  :ensure t
  :hook ((org-mode . org-appear-mode))
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t))

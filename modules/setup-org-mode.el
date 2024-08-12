(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode))
  :config
  (setq org-directory "~/repos/org-directory"
        org-default-notes-file (concat org-directory "/notes.org"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t))))

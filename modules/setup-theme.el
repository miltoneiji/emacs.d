;;; setup-theme.el -*- lexical-binding: t; -*-

(use-package doom-themes
  :config
  (setq-default doom-themes-enable-bold t)
  (setq-default doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  (doom-themes-org-config))
(defun tk/set-font ()
  "Set the font according to the Operating System.
https://www.reddit.com/r/emacs/comments/shzif1/n%CE%BBno_font_stack/"
  (set-face-attribute 'default nil
		      :family "Roboto Mono" :weight 'light :height 140)
  (set-face-attribute 'bold nil
		      :family "Roboto Mono" :weight 'regular)
  (set-face-attribute 'italic nil
		      :family "Victor Mono" :weight 'semilight :slant 'italic)
  (set-fontset-font t 'unicode
		    (font-spec :name "Inconsolata Light" :size 16) nil)
  (set-fontset-font t '(#xe000 . #xffdd)
		    (font-spec :name "Roboto Mono Nerd Font" :size 12) nil))
(tk/set-font)

(use-package mood-line
  :config
  (mood-line-mode))

(provide 'setup-theme)

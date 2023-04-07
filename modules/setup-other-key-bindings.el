;;; setup-other-key-bindings.el -*- lexical-binding: t; -*-

(define-key evil-motion-state-map (kbd "C-S-h") 'shrink-window-horizontally)
(define-key evil-motion-state-map (kbd "C-S-r") 'enlarge-window-horizontally)
(define-key evil-motion-state-map (kbd "C-S-j") 'shrink-window)
(define-key evil-motion-state-map (kbd "C-S-k") 'enlarge-window)

(provide 'setup-other-key-bindings)

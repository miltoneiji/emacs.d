;;; preferences.el --- Reasonable settings

(defun tk-defaults/open-files-from-home-directory ()
  "When opening a file, start searching at the user's home directory."
  (setq default-directory "~/"))

(defun tk-defaults/delete-trailing-whitespace ()
  "Call DELETE-TRAILING-WHITESPACE every time a buffer is saved."
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(defun tk-defaults/treat-camelcase-as-separate-words ()
  "Treat CamelCaseSubWords as separate words in every programming mode."
  (add-hook 'prog-mode-hook 'subword-mode))

(defun tk-defaults/automatically-follow-symlinks ()
  "When opening a file, always follow symlinks."
  (setq vc-follow-symlinks t))

(defun tk-defaults/single-space-after-periods ()
  "Don't assume that sentences should have two spaces after periods.
This ain't a typewriter."
  (setq sentence-end-double-space nil))

(defun tk-defaults/ensure-that-files-end-with-newline ()
  "If you save a file that doesn't end with a newline, automatically append one."
  (setq require-final-newline t))

(defun tk-defaults/quiet-startup ()
  "Don't present the usual startup message, and clear the scratch buffer."
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil))

(defun tk-defaults/shorten-yes-or-no ()
  "Don't ask `yes/no?', ask `y/n?'."
  (fset 'yes-or-no-p 'y-or-n-p))

(defun tk-defaults/break-line-automatically-in-text-modes ()
  "Break line automatically when line length it reached."
  (add-hook 'text-mode-hook #'(lambda () (auto-fill-mode 1))))

(defun tk-defaults/show-matching-parens ()
  "Visually indicate matching pairs of parentheses."
  (show-paren-mode t))

(defun tk-defaults/highlight-current-line ()
  "Highligh the current line."
  (global-hl-line-mode))

(defun tk-defaults/disable-gui-elements ()
  "I don't want those GUI elements."
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun tk-defaults/quit-prompts-with-esc ()
  "I prefer."
  (global-set-key (kbd "<escape>") #'keyboard-escape-quit))

(defun tk-defaults/get-rid-of-the-scroll-jumpiness ()
  "Get rid of the scroll jumpiness."
  (setq scroll-step           1
        scroll-conservatively 10000))

(defun tk-defaults/dont-indent-with-tabs ()
  "Don't indent with tabs."
  (setq-default indent-tabs-mode nil
                tab-width 4))

(tk-defaults/open-files-from-home-directory)
(tk-defaults/delete-trailing-whitespace)
(tk-defaults/treat-camelcase-as-separate-words)
(tk-defaults/automatically-follow-symlinks)
(tk-defaults/single-space-after-periods)
(tk-defaults/ensure-that-files-end-with-newline)
(tk-defaults/quiet-startup)
(tk-defaults/shorten-yes-or-no)
(tk-defaults/show-matching-parens)
(tk-defaults/highlight-current-line)
(tk-defaults/disable-gui-elements)
(tk-defaults/quit-prompts-with-esc)
(tk-defaults/break-line-automatically-in-text-modes)
(tk-defaults/get-rid-of-the-scroll-jumpiness)
(tk-defaults/dont-indent-with-tabs)

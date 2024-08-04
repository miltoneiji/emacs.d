;;; tk-modeline.el -*- lexical-binding: t; -*-

(defcustom tk/modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;; Helpers
(defun tk/common-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold',
respectively."
  (or (and (numberp split-width-threshold)
	   (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
	   (< (window-total-height) split-height-threshold))))

(defun tk/modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (tk/common-window-small-p)
       (> (length str) tk/modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))

(defun tk/modeline-string-truncate (str)
  "Return truncated STR, if appropriate, else return STR.
Truncation is done up to `tk/modeline-string-truncate-length'."
  (if (tk/modeline--string-truncate-p str)
      (concat "..." (substring str (- tk/modeline-string-truncate-length)))
    str))

;;;; Buffer identification

(defun tk/modeline-buffer-read-only-status ()
  "Return a lock symbol if buffer is read-only, else return empty string."
  (if buffer-read-only
      (format " %s" (char-to-string #xE0A2))
    ""))

(defun tk/modeline-buffer-name ()
  "Return `buffer-name', truncating it if necessary."
  (when-let ((name (buffer-name)))
    ;(format " %s" (tk/modeline-string-truncate name))
    name))

(defvar-local tk/modeline-buffer-identification
    '(:eval
      (list
       (tk/modeline-buffer-read-only-status)
       (propertize (tk/modeline-buffer-name)
		   'face 'bold)))
  "Mode line construct for identifying the buffer being displayed.")

;;;; Position in buffer

(defvar-local tk/modeline-position-in-buffer
    '(:eval
      (if (mode-line-window-selected-p)
	  " [%l,%c]")))

;;;; Major mode

(defun tk/modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (format " %s" (capitalize (string-replace "-mode" "" (symbol-name major-mode)))))

(defvar-local tk/modeline-major-mode
    '(:eval
      (if (mode-line-window-selected-p)
	  (propertize (tk/modeline-major-mode-name)))))

;;;; VC branch
(defun tk/modeline--vc-branch-name (file)
  "Return VC branch name for FILE if it exits, else return empty."
  (when-let ((backend (vc-backend file))
	     (rev (vc-working-revision file backend))
	     (branch (or (vc-git--symbolic-ref file)
			 (substring rev 0 7))))
    (format " [%s]" branch)))

(defvar-local tk/modeline-vc-branch
    '(:eval
      (if (mode-line-window-selected-p)
	  (propertize (tk/modeline--vc-branch-name (buffer-file-name)))))
  "Mode line construct to return propertized VC branch.")

;;;; Flymake
(declare-function flymake--severity "flymake" (type))

(defun tk/modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
	       (flymake--severity (flymake-diagnostic-type d)))
	(cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar tk/modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defmacro tk/modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "tk/modeline-flymake-%s" type)) ()
     (when-let ((count (tk/modeline-flymake-counter
			,(intern (format ":%s" type)))))
       (concat
	(propertize ,indicator 'face 'shadow)
	(propertize count
		    'face ',(or face type)
		    'mouse-face 'mode-line-highlight
		    'local-map tk/modeline-flymake-map
		    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(tk/modeline-flymake-type error "☣")
(tk/modeline-flymake-type warning "!")
(tk/modeline-flymake-type note "." success)

(defvar-local tk/modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
		 (mode-line-window-selected-p))
	(list
	 '(:eval (tk/modeline-flymake-error))
	 '(:eval (tk/modeline-flymake-warning))
	 '(:eval (tk/modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

(setq mode-line-right-align-edge 'right-margin)

(setq-default mode-line-format
	      '(" "
		tk/modeline-buffer-identification
		tk/modeline-position-in-buffer
		tk/modeline-major-mode
		mode-line-format-right-align
		tk/modeline-vc-branch
		" "
		tk/modeline-flymake
		" "))

(dolist (construct '(tk/modeline-buffer-identification
		     tk/modeline-position-in-buffer
		     tk/modeline-major-mode
		     tk/modeline-vc-branch
		     tk/modeline-flymake))
  (put construct 'risky-local-variable t))

;;; tk-location-tags.el --- Tag and navigate buffer locations -*- lexical-binding: t; -*-

;;; Commentary:
;; A system for tagging buffer locations (buffer + line) with optional names,
;; and quickly navigating between them.

;;; Code:

(require 'consult)

;;; Configuration

(defvar tk/location-tags nil
  "List of tagged locations.
Each element is a plist with keys:
  :name - Optional string name for the tag
  :buffer - Buffer name
  :file - File path (if buffer is visiting a file)
  :line - Line number
  :column - Column number
  :timestamp - Time when tag was created")

(defvar tk/location-tags-file
  (expand-file-name "location-tags.el" user-emacs-directory)
  "File to persist tagged locations across sessions.")

(defvar tk/location-tags-current-index 0
  "Index of the current tag when navigating.")

;;; Core Functions

(defun tk/location-tag--make-tag (name)
  "Create a tag for the current location with optional NAME."
  (list :name name
        :buffer (buffer-name)
        :file (buffer-file-name)
        :line (line-number-at-pos)
        :column (current-column)
        :timestamp (current-time)))

(defun tk/location-tag--format-tag (tag)
  "Format TAG for display in completion interface."
  (let ((name (plist-get tag :name))
        (buffer (plist-get tag :buffer))
        (file (plist-get tag :file))
        (line (plist-get tag :line)))
    (format "%-25s %s:%d"
            (if (and name (not (string-empty-p name)))
                (propertize name 'face 'font-lock-keyword-face)
              (propertize "<unnamed>" 'face 'font-lock-comment-face))
            (propertize (or file buffer) 'face 'font-lock-string-face)
            line)))

(defun tk/location-tag--jump-to-tag (tag)
  "Jump to the location specified by TAG."
  (let ((buffer-name (plist-get tag :buffer))
        (file (plist-get tag :file))
        (line (plist-get tag :line))
        (column (plist-get tag :column)))
    (if (and file (file-exists-p file))
        (find-file file)
      (if (get-buffer buffer-name)
          (switch-to-buffer buffer-name)
        (user-error "Buffer '%s' no longer exists" buffer-name)))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

(defun tk/location-tag--valid-p (tag)
  "Check if TAG points to a valid location."
  (let ((file (plist-get tag :file))
        (buffer-name (plist-get tag :buffer)))
    (or (and file (file-exists-p file))
        (get-buffer buffer-name))))

(defun tk/location-tag--cleanup-invalid ()
  "Remove tags that point to non-existent buffers/files."
  (setq tk/location-tags
        (seq-filter #'tk/location-tag--valid-p tk/location-tags)))

;;; Interactive Commands

;;;###autoload
(defun tk/location-tag-add (name)
  "Add a tag at the current location with optional NAME."
  (interactive "sTag name (optional): ")
  (let ((tag (tk/location-tag--make-tag name)))
    (push tag tk/location-tags)
    (message "Tagged location: %s" (tk/location-tag--format-tag tag))))

;;;###autoload
(defun tk/location-tag-remove-at-point ()
  "Remove tag at the current location."
  (interactive)
  (let* ((current-buffer (buffer-name))
         (current-line (line-number-at-pos))
         (matching-tags (seq-filter
                         (lambda (tag)
                           (and (string= (plist-get tag :buffer) current-buffer)
                                (= (plist-get tag :line) current-line)))
                         tk/location-tags)))
    (if matching-tags
        (progn
          (dolist (tag matching-tags)
            (setq tk/location-tags (remove tag tk/location-tags)))
          (message "Removed %d tag(s) at current location" (length matching-tags)))
      (message "No tags at current location"))))

;;;###autoload
(defun tk/location-tag-clear-all ()
  "Clear all tags."
  (interactive)
  (when (yes-or-no-p (format "Clear all %d tags? " (length tk/location-tags)))
    (setq tk/location-tags nil)
    (message "All tags cleared")))

;;;###autoload
(defun tk/location-tag-jump ()
  "Jump to a tagged location using consult interface."
  (interactive)
  (tk/location-tag--cleanup-invalid)
  (if (null tk/location-tags)
      (user-error "No tags available")
    (let* ((tags-with-index (seq-map-indexed
                             (lambda (tag idx)
                               (cons (tk/location-tag--format-tag tag) idx))
                             (reverse tk/location-tags)))
           (selection (consult--read
                       tags-with-index
                       :prompt "Jump to tag: "
                       :category 'tk/location-tag
                       :sort nil
                       :require-match t
                       :history 'tk/location-tag-history
                       :lookup #'consult--lookup-cdr
                       :state (consult--jump-state))))
      (when selection
        (let ((tag (nth selection tk/location-tags)))
          (tk/location-tag--jump-to-tag tag)
          (setq tk/location-tags-current-index selection))))))

;;;###autoload
(defun tk/location-tag-next ()
  "Navigate to the next tag in creation order."
  (interactive)
  (tk/location-tag--cleanup-invalid)
  (if (null tk/location-tags)
      (user-error "No tags available")
    (setq tk/location-tags-current-index
          (mod (1+ tk/location-tags-current-index) (length tk/location-tags)))
    (let ((tag (nth tk/location-tags-current-index tk/location-tags)))
      (tk/location-tag--jump-to-tag tag)
      (message "Tag %d/%d: %s"
               (1+ tk/location-tags-current-index)
               (length tk/location-tags)
               (tk/location-tag--format-tag tag)))))

;;;###autoload
(defun tk/location-tag-previous ()
  "Navigate to the previous tag in creation order."
  (interactive)
  (tk/location-tag--cleanup-invalid)
  (if (null tk/location-tags)
      (user-error "No tags available")
    (setq tk/location-tags-current-index
          (mod (1- tk/location-tags-current-index) (length tk/location-tags)))
    (let ((tag (nth tk/location-tags-current-index tk/location-tags)))
      (tk/location-tag--jump-to-tag tag)
      (message "Tag %d/%d: %s"
               (1+ tk/location-tags-current-index)
               (length tk/location-tags)
               (tk/location-tag--format-tag tag)))))

;;;###autoload
(defun tk/location-tag-list ()
  "Display all tags in a list."
  (interactive)
  (tk/location-tag--cleanup-invalid)
  (if (null tk/location-tags)
      (message "No tags available")
    (with-current-buffer (get-buffer-create "*Location Tags*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Location Tags\n" 'face 'bold))
        (insert (propertize (make-string 80 ?─) 'face 'shadow))
        (insert "\n\n")
        (dolist (tag (reverse tk/location-tags))
          (insert (tk/location-tag--format-tag tag))
          (insert "\n"))
        (insert "\n")
        (insert (format "Total: %d tags\n" (length tk/location-tags))))
      (goto-char (point-min))
      (view-mode)
      (display-buffer (current-buffer)))))

;;; Persistence

(defun tk/location-tag--save ()
  "Save tags to disk."
  (with-temp-file tk/location-tags-file
    (insert ";;; Location Tags -*- mode: emacs-lisp -*-\n\n")
    (prin1 tk/location-tags (current-buffer))
    (insert "\n")))

(defun tk/location-tag--load ()
  "Load tags from disk."
  (when (file-exists-p tk/location-tags-file)
    (with-temp-buffer
      (insert-file-contents tk/location-tags-file)
      (goto-char (point-min))
      (forward-line 2) ; Skip header
      (setq tk/location-tags (read (current-buffer))))
    (tk/location-tag--cleanup-invalid)
    (message "Loaded %d location tags" (length tk/location-tags))))

;;;###autoload
(defun tk/location-tag-save-to-disk ()
  "Manually save tags to disk."
  (interactive)
  (tk/location-tag--save)
  (message "Saved %d tags to %s" (length tk/location-tags) tk/location-tags-file))

;;;###autoload
(defun tk/location-tag-load-from-disk ()
  "Manually load tags from disk."
  (interactive)
  (tk/location-tag--load))

;; Auto-save on exit
(add-hook 'kill-emacs-hook #'tk/location-tag--save)

;; Auto-load on startup
(add-hook 'after-init-hook #'tk/location-tag--load)

(provide 'tk-location-tags)
;;; tk-location-tags.el ends here

;;; dired-image-thumbnail.el --- Enhanced workflow for image-dired -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Dyer

;; Author: James Dyer
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: multimedia, files, dired, images
;; URL: https://github.com/captainflasmr/dired-image-thumbnail

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package extends `image-dired' with an improved workflow inspired by
;; `dired-video-thumbnail'.  It adds:
;;
;; - Sorting: Sort thumbnails by name, date, size
;; - Filtering: Filter by name regexp, file size range
;; - Subdirectory support: Works with inserted subdirectories
;; - Wrap display mode: Thumbnails flow naturally and wrap to window width
;; - Enhanced header line: Shows current image info with sort/filter status
;; - Marking: Uses built-in image-dired marking with visual border
;; - File operations: Delete images, navigate to dired buffer
;; - Window layout: Automatic split-screen layout (thumbnails left, image right)
;; - Auto-display: Navigate with n/p to automatically show full-size images
;;
;; Usage:
;;
;; From a Dired buffer, call `M-x dired-image-thumbnail' to display
;; thumbnails with enhanced features. You can also use the standard
;; `M-x image-dired' and the enhanced features will be available.
;;
;; To include images from subdirectories, use 'i' (`dired-maybe-insert-subdir`)
;; to insert subdirectories before calling `dired-image-thumbnail', or use
;; the helper commands:
;; - `dired-image-thumbnail-insert-subdir-recursive' - Insert all subdirectories
;; - `dired-image-thumbnail-insert-image-subdirs' - Insert only subdirs with images
;; - `dired-image-thumbnail-kill-all-subdirs' - Remove all inserted subdirectories
;;
;; The package uses the standard *image-dired* buffer, so all native
;; image-dired marking commands work as expected.
;;
;; Key bindings in thumbnail buffer:
;;
;;   s   - Sorting prefix (s n: name, s d: date, s s: size, s r: reverse)
;;   S   - Interactive sort selection
;;   /   - Filtering prefix (/ n: name, / s: size, / c: clear)
;;   w   - Toggle wrap display mode
;;   r   - Refresh display
;;   n/p - Next/previous image (with auto-display when enabled)
;;   +/- - Increase/decrease size
;;   m   - Mark image (uses image-dired's native marking with border)
;;   u   - Unmark image
;;   M   - Mark all
;;   U   - Unmark all
;;   t   - Toggle all marks
;;   d   - Go to Dired buffer
;;   D   - Delete image at point
;;   C-d - Delete image and move to next
;;   x   - Delete marked images
;;   ?/h - Help
;;

;;; Code:

(require 'image-dired)
(require 'image-dired-util)
(require 'image)
(require 'dired)

(declare-function image-size "image.c" (spec &optional pixels frame))

;;; Customization

(defgroup dired-image-thumbnail nil
  "Enhanced workflow for image-dired."
  :group 'image-dired
  :prefix "dired-image-thumbnail-")

(defcustom dired-image-thumbnail-sort-by 'date
  "Default sorting criteria for thumbnails."
  :type '(choice (const :tag "Name" name)
                 (const :tag "Date modified" date)
                 (const :tag "Size" size))
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-sort-order 'descending
  "Default sort order for thumbnails."
  :type '(choice (const :tag "Ascending" ascending)
                 (const :tag "Descending" descending))
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-wrap-display nil
  "Whether to wrap thumbnails to fill the buffer width.
When non-nil, thumbnails flow naturally and wrap based on window width.
When nil, the standard `image-dired' line-up method is used."
  :type 'boolean
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-manage-window-layout t
  "Whether to manage window layout for thumbnail and image display buffers.
When non-nil, thumbnail buffer appears on the left (50% width) and
image display buffer appears on the right (50% width).
When nil, use Emacs default window placement."
  :type 'boolean
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-auto-display-on-navigate t
  "Whether to automatically display full-size image when navigating thumbnails.
When non-nil, pressing `n` or `p` in the thumbnail buffer automatically
updates the image display buffer. When nil, you must press RET or
C-<return> to view the full-size image."
  :type 'boolean
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-image-extensions
  '("jpg" "jpeg" "png" "gif" "bmp" "tiff" "tif" "webp" "svg" "ico" "heic" "heif")
  "List of image file extensions to recognise."
  :type '(repeat string)
  :group 'dired-image-thumbnail)

;;; Internal variables

(defvar-local dired-image-thumbnail--all-images nil
  "List of all images before filtering/sorting.")

(defvar-local dired-image-thumbnail--current-images nil
  "List of images after filtering/sorting.")

(defvar-local dired-image-thumbnail--source-dir nil
  "Source directory for the current thumbnail buffer.")

(defvar-local dired-image-thumbnail--dired-buffer nil
  "The Dired buffer associated with this thumbnail buffer.")

(defvar-local dired-image-thumbnail--sort-by nil
  "Current sort criteria for this buffer.")

(defvar-local dired-image-thumbnail--sort-order nil
  "Current sort order for this buffer.")

(defvar-local dired-image-thumbnail--filter-name nil
  "Current name filter regexp.")

(defvar-local dired-image-thumbnail--filter-size-min nil
  "Minimum size filter in bytes.")

(defvar-local dired-image-thumbnail--filter-size-max nil
  "Maximum size filter in bytes.")

(defvar-local dired-image-thumbnail--display-size 150
  "Current display size for thumbnails (for zoom).")

(defvar-local dired-image-thumbnail--dimension-cache (make-hash-table :test 'equal)
  "Cache for image dimensions keyed by file name.")

(defvar-local dired-image-thumbnail--dimension-pending (make-hash-table :test 'equal)
  "Files pending dimension calculation. Value is t if process is running.")

(defun dired-image-thumbnail--get-image-dimensions (file)
  "Get dimensions of image FILE as (width . height), or (0 . 0) if unknown.
If not cached, launch an async process (`identify`) to fill the cache."
  (or (gethash file dired-image-thumbnail--dimension-cache)
      (progn
        (unless (gethash file dired-image-thumbnail--dimension-pending)
          (puthash file t dired-image-thumbnail--dimension-pending)
          (dired-image-thumbnail--start-identify-process file))
        ;; Fallback until process finishes
        (cons 0 0))))

(defun dired-image-thumbnail--nearest-image-original-file-name (&optional pos)
  "Return the 'original-file-name property at POS, or search nearby if not found."
  (let ((pos (or pos (point))))
    (or (get-text-property pos 'original-file-name)
        ;; Search backward then forward for nearest image property
        (save-excursion
          (let ((found nil)
                (limit (point-min)))
            (goto-char pos)
            (while (and (not found) (> (point) limit))
              (setq found (get-text-property (point) 'original-file-name))
              (unless found (backward-char)))
            found))
        (save-excursion
          (let ((found nil)
                (limit (point-max)))
            (goto-char pos)
            (while (and (not found) (< (point) limit))
              (setq found (get-text-property (point) 'original-file-name))
              (unless found (forward-char)))
            found)))))

(defun dired-image-thumbnail--start-identify-process (file)
  "Start an async process to get dimensions for FILE with improved logging."
  (let ((buf (generate-new-buffer " *dired-image-thumb-identify*"))
        (file-attr file))
    (make-process
     :name "dired-image-thumb-identify"
     :buffer buf
     :command (list "identify" "-format" "%w %h" (expand-file-name file))
     :noquery t
     :sentinel
     (lambda (proc _event)
       (let ((file-attr file-attr)) ; ensure file-attr is always bound in closure
         (when (eq (process-status proc) 'exit)
           (let ((exit-status (process-exit-status proc)))
             (with-current-buffer (process-buffer proc)
               (goto-char (point-min))
               (when (zerop exit-status)
                 (let* ((line (buffer-substring-no-properties (point-min) (point-max)))
                        (nums (split-string line)))
                   (when (and (= (length nums) 2)
                              (string-match-p "^[0-9]+$" (car nums))
                              (string-match-p "^[0-9]+$" (cadr nums)))
                     (let ((w (string-to-number (car nums)))
                           (h (string-to-number (cadr nums))))
                       (puthash file-attr (cons w h) dired-image-thumbnail--dimension-cache)
                       (dolist (buf (buffer-list))
                         (with-current-buffer buf
                           (when (derived-mode-p 'image-dired-thumbnail-mode)
                             (image-dired--update-header-line))))))))
               (remhash file-attr dired-image-thumbnail--dimension-pending)
               (kill-buffer (process-buffer proc))))))))))

;;; Utility functions

(defun dired-image-thumbnail--image-p (file)
  "Return non-nil if FILE is an image file."
  (and (file-regular-p file)
       (member (downcase (or (file-name-extension file) ""))
               dired-image-thumbnail-image-extensions)))

(defun dired-image-thumbnail--find-images (directory &optional recursive)
  "Find all image files in DIRECTORY.
If RECURSIVE is non-nil, search subdirectories as well."
  (if recursive
      (let ((images nil)
            (regexp (concat "\\." (regexp-opt dired-image-thumbnail-image-extensions) "\\'")))
        (dolist (file (directory-files-recursively directory regexp nil))
          (when (dired-image-thumbnail--image-p file)
            (push file images)))
        (nreverse images))
    (seq-filter #'dired-image-thumbnail--image-p
                (directory-files directory t nil t))))

(defun dired-image-thumbnail--file-marked-p (file)
  "Return non-nil if FILE is marked in the associated dired buffer."
  (when (and dired-image-thumbnail--dired-buffer
             (buffer-live-p dired-image-thumbnail--dired-buffer))
    (with-current-buffer dired-image-thumbnail--dired-buffer
      (save-excursion
        (goto-char (point-min))
        (when (dired-goto-file file)
          (image-dired-dired-file-marked-p))))))

(defun dired-image-thumbnail--relative-name (file)
  "Return FILE name relative to the source directory."
  (if (and dired-image-thumbnail--source-dir
           (string-prefix-p (expand-file-name dired-image-thumbnail--source-dir)
                            (expand-file-name file)))
      (file-relative-name file dired-image-thumbnail--source-dir)
    (file-name-nondirectory file)))

(defun dired-image-thumbnail--format-file-size (file)
  "Return human-readable file size for FILE."
  (let ((attrs (file-attributes file)))
    (if attrs
        (file-size-human-readable (file-attribute-size attrs))
      "?")))

(defun dired-image-thumbnail--format-image-dimensions (file)
  "Return formatted dimensions string for image FILE (e.g., \"1920x1080\")."
  (let ((dims (dired-image-thumbnail--get-image-dimensions file)))
    (if (and dims (> (car dims) 0) (> (cdr dims) 0))
        (format "%dx%d" (car dims) (cdr dims))
      "?")))

(defun dired-image-thumbnail--count-marked ()
  "Count the number of marked images."
  (let ((count 0))
    (when dired-image-thumbnail--current-images
      (dolist (file dired-image-thumbnail--current-images)
        (when (dired-image-thumbnail--file-marked-p file)
          (setq count (1+ count)))))
    count))

;;; Sorting functions

(defun dired-image-thumbnail--sort-images (images)
  "Sort IMAGES according to current sort settings."
  (let* ((sort-by (or dired-image-thumbnail--sort-by dired-image-thumbnail-sort-by))
         (sort-order (or dired-image-thumbnail--sort-order dired-image-thumbnail-sort-order))
         (sorted
          (pcase sort-by
            ('name
             (sort (copy-sequence images)
                   (lambda (a b)
                     (string< (downcase (file-name-nondirectory a))
                              (downcase (file-name-nondirectory b))))))
            ('date
             (sort (copy-sequence images)
                   (lambda (a b)
                     (time-less-p (file-attribute-modification-time (file-attributes a))
                                  (file-attribute-modification-time (file-attributes b))))))
            ('size
             (sort (copy-sequence images)
                   (lambda (a b)
                     (< (or (file-attribute-size (file-attributes a)) 0)
                        (or (file-attribute-size (file-attributes b)) 0)))))
            (_ images))))
    (if (eq sort-order 'descending)
        (nreverse sorted)
      sorted)))

;;; Filtering functions

(defun dired-image-thumbnail--filter-images (images)
  "Filter IMAGES according to current filter settings."
  (let ((result images))
    ;; Filter by name
    (when dired-image-thumbnail--filter-name
      (setq result
            (seq-filter
             (lambda (file)
               (string-match-p dired-image-thumbnail--filter-name
                               (file-name-nondirectory file)))
             result)))
    ;; Filter by size
    (when (or dired-image-thumbnail--filter-size-min
              dired-image-thumbnail--filter-size-max)
      (setq result
            (seq-filter
             (lambda (file)
               (let ((size (file-attribute-size (file-attributes file))))
                 (and (or (null dired-image-thumbnail--filter-size-min)
                          (>= size dired-image-thumbnail--filter-size-min))
                      (or (null dired-image-thumbnail--filter-size-max)
                          (<= size dired-image-thumbnail--filter-size-max)))))
             result)))
    result))

(defun dired-image-thumbnail--format-active-filters ()
  "Return a string describing active filters."
  (let ((filters nil))
    (when dired-image-thumbnail--filter-name
      (push (format "name:/%s/" dired-image-thumbnail--filter-name) filters))
    (when (or dired-image-thumbnail--filter-size-min
              dired-image-thumbnail--filter-size-max)
      (push (format "size:%s-%s"
                    (if dired-image-thumbnail--filter-size-min
                        (file-size-human-readable dired-image-thumbnail--filter-size-min)
                      "0")
                    (if dired-image-thumbnail--filter-size-max
                        (file-size-human-readable dired-image-thumbnail--filter-size-max)
                      "∞"))
            filters))
    (if filters
        (mapconcat #'identity (nreverse filters) " ")
      "")))

;;; Apply sort and filter

(defun dired-image-thumbnail--apply-sort-and-filter ()
  "Apply current sort and filter settings and refresh display."
  ;; Initialize if not already done
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (when dired-image-thumbnail--all-images
    (let ((filtered (dired-image-thumbnail--filter-images dired-image-thumbnail--all-images)))
      (setq dired-image-thumbnail--current-images
            (dired-image-thumbnail--sort-images filtered))))
  (dired-image-thumbnail-refresh))

;;; Initialization for standard image-dired

(defun dired-image-thumbnail--initialize-buffer ()
  "Initialize dired-image-thumbnail variables in the current thumbnail buffer.
This is called via hook when entering `image-dired-thumbnail-mode'."
  (unless dired-image-thumbnail--all-images
    ;; Collect images from the buffer's text properties
    (let ((images nil)
          (dired-buf nil)
          (source-dir nil))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((file (get-text-property (point) 'original-file-name)))
            (push file images)
            (unless dired-buf
              (setq dired-buf (get-text-property (point) 'associated-dired-buffer)))
            (unless source-dir
              (setq source-dir (file-name-directory file))))
          (forward-char)))
      ;; Get source-dir from dired buffer if available
      (when (and dired-buf (buffer-live-p dired-buf))
        (with-current-buffer dired-buf
          (unless source-dir
            (setq source-dir dired-image-thumbnail--source-dir))))
      (when images
        (setq dired-image-thumbnail--all-images (nreverse images))
        (setq dired-image-thumbnail--current-images dired-image-thumbnail--all-images)
        (setq dired-image-thumbnail--dired-buffer dired-buf)
        (setq dired-image-thumbnail--source-dir (or source-dir default-directory))
        (setq dired-image-thumbnail--sort-by dired-image-thumbnail-sort-by)
        (setq dired-image-thumbnail--sort-order dired-image-thumbnail-sort-order)))))

;;; Header line

(defun dired-image-thumbnail--format-properties-string (orig-fun buf file image-count props comment)
  "Advice around `image-dired-format-properties-string' for enhanced header line.
ORIG-FUN is the original function.
BUF, FILE, IMAGE-COUNT, PROPS, and COMMENT are passed to the original function.
When `dired-image-thumbnail--all-images' is set, return our enhanced header line.
Otherwise, fall back to the original function."
  (if dired-image-thumbnail--all-images
      ;; Use our enhanced header line
      (let* ((sort-info (format "[%s %s]"
                                (or dired-image-thumbnail--sort-by dired-image-thumbnail-sort-by)
                                (if (eq (or dired-image-thumbnail--sort-order
                                            dired-image-thumbnail-sort-order)
                                        'ascending)
                                    "<" ">")))
             (filter-info (dired-image-thumbnail--format-active-filters))
             (marked-count (dired-image-thumbnail--count-marked))
             (marked-info (if (> marked-count 0)
                              (format " [%d marked]" marked-count)
                            ""))
             (rel-name (dired-image-thumbnail--relative-name file))
             (size (dired-image-thumbnail--format-file-size file))
             (dimensions (dired-image-thumbnail--format-image-dimensions file)))
        (concat
         "  "
         (propertize rel-name 'face 'image-dired-thumb-header-file-name)
         "  "
         (propertize image-count 'face 'image-dired-thumb-header-image-count)
         "  "
         (propertize size 'face 'image-dired-thumb-header-file-size)
         "  "
         (propertize dimensions 'face 'shadow)
         "  "
         sort-info
         (if (string-empty-p filter-info) "" (format "  %s" filter-info))
         marked-info))
    ;; Fall back to original function
    (funcall orig-fun buf file image-count props comment)))

(advice-add 'image-dired-format-properties-string :around #'dired-image-thumbnail--format-properties-string)

;;; Display functions

(defun dired-image-thumbnail-refresh ()
  "Refresh the thumbnail display with current images."
  (interactive)
  ;; Initialize if not already done
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (when dired-image-thumbnail--all-images
    (let ((current-file (image-dired-original-file-name))
          (dired-buf dired-image-thumbnail--dired-buffer)
          (source-dir dired-image-thumbnail--source-dir)
          (sort-by dired-image-thumbnail--sort-by)
          (sort-order dired-image-thumbnail--sort-order)
          (filter-name dired-image-thumbnail--filter-name)
          (filter-size-min dired-image-thumbnail--filter-size-min)
          (filter-size-max dired-image-thumbnail--filter-size-max)
          (display-size dired-image-thumbnail--display-size)
          (all-images dired-image-thumbnail--all-images)
          (inhibit-read-only t))
      (erase-buffer)
      ;; Restore state
      (setq dired-image-thumbnail--all-images all-images)
      (setq dired-image-thumbnail--source-dir source-dir)
      (setq dired-image-thumbnail--dired-buffer dired-buf)
      (setq dired-image-thumbnail--display-size display-size)
      (setq dired-image-thumbnail--sort-by sort-by)
      (setq dired-image-thumbnail--sort-order sort-order)
      (setq dired-image-thumbnail--filter-name filter-name)
      (setq dired-image-thumbnail--filter-size-min filter-size-min)
      (setq dired-image-thumbnail--filter-size-max filter-size-max)
      ;; Apply filter and sort
      (let ((filtered (dired-image-thumbnail--filter-images all-images)))
        (setq dired-image-thumbnail--current-images
              (dired-image-thumbnail--sort-images filtered)))
      ;; Temporarily override thumb size if needed
      (let ((standard-size image-dired-thumb-size))
        (when (and display-size (/= display-size standard-size))
          (setq-local image-dired-thumb-size display-size))
        ;; Insert thumbnails using image-dired's standard function
        ;; This ensures proper marking support
        (dolist (file dired-image-thumbnail--current-images)
          (let ((thumb-file (image-dired-thumb-name file)))
            ;; Create thumbnail if it doesn't exist
            (unless (file-exists-p thumb-file)
              (image-dired-create-thumb file thumb-file))
            ;; Insert with all three required arguments
            (image-dired-insert-thumbnail thumb-file file dired-buf))))
      ;; Line up
      (if dired-image-thumbnail-wrap-display
          (progn
            (setq-local word-wrap t)
            (setq-local truncate-lines nil))
        (image-dired--line-up-with-method))
      ;; Restore mark display
      (image-dired--thumb-update-marks)
      (image-dired--update-header-line)
      ;; Try to restore position
      (when current-file
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (when (equal (get-text-property (point) 'original-file-name) current-file)
              (setq found t))
            (unless found (forward-char)))
          (unless found (goto-char (point-min))))))))

(defun dired-image-thumbnail-sort-by-name ()
  "Sort thumbnails by name."
  (interactive)
  (setq dired-image-thumbnail--sort-by 'name)
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sorted by name"))

(defun dired-image-thumbnail-sort-by-date ()
  "Sort thumbnails by date."
  (interactive)
  (setq dired-image-thumbnail--sort-by 'date)
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sorted by date"))

(defun dired-image-thumbnail-sort-by-size ()
  "Sort thumbnails by size."
  (interactive)
  (setq dired-image-thumbnail--sort-by 'size)
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sorted by size"))

(defun dired-image-thumbnail-sort-reverse ()
  "Reverse current sort order."
  (interactive)
  (setq dired-image-thumbnail--sort-order
        (if (eq dired-image-thumbnail--sort-order 'ascending)
            'descending
          'ascending))
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sort order: %s" dired-image-thumbnail--sort-order))

(defun dired-image-thumbnail-sort ()
  "Interactively choose sort criteria."
  (interactive)
  (let ((choice (completing-read "Sort by: "
                                 '("name" "date" "size" "reverse")
                                 nil t)))
    (pcase choice
      ("name" (dired-image-thumbnail-sort-by-name))
      ("date" (dired-image-thumbnail-sort-by-date))
      ("size" (dired-image-thumbnail-sort-by-size))
      ("reverse" (dired-image-thumbnail-sort-reverse)))))

;;; Filtering commands

(defun dired-image-thumbnail-filter-by-name (regexp)
  "Filter thumbnails by name matching REGEXP."
  (interactive "sFilter by name (regexp): ")
  (setq dired-image-thumbnail--filter-name
        (if (string-empty-p regexp) nil regexp))
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Name filter: %s" (or dired-image-thumbnail--filter-name "none")))

(defun dired-image-thumbnail-filter-by-size (min max)
  "Filter thumbnails by size between MIN and MAX bytes.
Enter size in human-readable format (e.g., 100k, 1M)."
  (interactive
   (list (read-string "Minimum size (e.g., 100k, 1M, empty for none): ")
         (read-string "Maximum size (e.g., 100k, 1M, empty for none): ")))
  (setq dired-image-thumbnail--filter-size-min
        (if (string-empty-p min) nil (dired-image-thumbnail--parse-size min)))
  (setq dired-image-thumbnail--filter-size-max
        (if (string-empty-p max) nil (dired-image-thumbnail--parse-size max)))
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Size filter: %s - %s"
           (if dired-image-thumbnail--filter-size-min
               (file-size-human-readable dired-image-thumbnail--filter-size-min)
             "none")
           (if dired-image-thumbnail--filter-size-max
               (file-size-human-readable dired-image-thumbnail--filter-size-max)
             "none")))

(defun dired-image-thumbnail--parse-size (str)
  "Parse human-readable size STR to bytes."
  (let ((str (downcase (string-trim str))))
    (cond
     ((string-match "\\([0-9.]+\\)g" str)
      (* (string-to-number (match-string 1 str)) 1073741824))
     ((string-match "\\([0-9.]+\\)m" str)
      (* (string-to-number (match-string 1 str)) 1048576))
     ((string-match "\\([0-9.]+\\)k" str)
      (* (string-to-number (match-string 1 str)) 1024))
     (t (string-to-number str)))))

(defun dired-image-thumbnail-filter-clear ()
  "Clear all filters."
  (interactive)
  (setq dired-image-thumbnail--filter-name nil)
  (setq dired-image-thumbnail--filter-size-min nil)
  (setq dired-image-thumbnail--filter-size-max nil)
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Filters cleared"))

(defun dired-image-thumbnail-filter ()
  "Interactively choose filter type."
  (interactive)
  (let ((choice (completing-read "Filter by: "
                                 '("name" "size" "clear")
                                 nil t)))
    (pcase choice
      ("name" (call-interactively #'dired-image-thumbnail-filter-by-name))
      ("size" (call-interactively #'dired-image-thumbnail-filter-by-size))
      ("clear" (dired-image-thumbnail-filter-clear)))))

(defun dired-image-thumbnail-increase-size ()
  "Increase thumbnail display size.
When size exceeds the cached thumbnail size, images are scaled from
the original files for crisp display (slower but higher quality)."
  (interactive)
  (let ((current (or dired-image-thumbnail--display-size 128)))
    (setq dired-image-thumbnail--display-size (min 512 (+ current 32)))
    (dired-image-thumbnail-refresh)
    (if (> dired-image-thumbnail--display-size image-dired-thumb-height)
        (message "Thumbnail size: %d (using original images for quality)"
                 dired-image-thumbnail--display-size)
      (message "Thumbnail size: %d" dired-image-thumbnail--display-size))))

(defun dired-image-thumbnail-decrease-size ()
  "Decrease thumbnail display size."
  (interactive)
  (let ((current (or dired-image-thumbnail--display-size 128)))
    (setq dired-image-thumbnail--display-size (max 32 (- current 32)))
    (dired-image-thumbnail-refresh)
    (message "Thumbnail size: %d" dired-image-thumbnail--display-size)))

(defun dired-image-thumbnail-mark-all ()
  "Mark all visible images in the thumbnail buffer."
  (interactive)
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (when dired-image-thumbnail--current-images
    ;; Mark each file in dired
    (dolist (file dired-image-thumbnail--current-images)
      ;; Ensure subdir is in dired for recursive mode
      ;; Mark in dired buffer
      (when (and dired-image-thumbnail--dired-buffer
                 (buffer-live-p dired-image-thumbnail--dired-buffer))
        (with-current-buffer dired-image-thumbnail--dired-buffer
          (save-excursion
            (goto-char (point-min))
            (when (dired-goto-file file)
              (dired-mark 1))))))
    ;; Update all thumbnail marks using image-dired's function
    (image-dired--thumb-update-marks)
    (message "Marked all %d images" (length dired-image-thumbnail--current-images))))

(defun dired-image-thumbnail-toggle-all-marks ()
  "Toggle mark on all visible images."
  (interactive)
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (dolist (file dired-image-thumbnail--current-images)
    (let ((is-marked (dired-image-thumbnail--file-marked-p file)))
      (if is-marked
          ;; Unmark
          (when (and dired-image-thumbnail--dired-buffer
                     (buffer-live-p dired-image-thumbnail--dired-buffer))
            (with-current-buffer dired-image-thumbnail--dired-buffer
              (save-excursion
                (goto-char (point-min))
                (when (dired-goto-file file)
                  (dired-unmark 1)))))
        ;; Mark
        (when (and dired-image-thumbnail--dired-buffer
                   (buffer-live-p dired-image-thumbnail--dired-buffer))
          (with-current-buffer dired-image-thumbnail--dired-buffer
            (save-excursion
              (goto-char (point-min))
              (when (dired-goto-file file)
                (dired-mark 1))))))))
  ;; Update all thumbnail marks using image-dired's function
  (image-dired--thumb-update-marks)
  (message "%d images now marked" (dired-image-thumbnail--count-marked)))

;;; File operations

(defun dired-image-thumbnail-goto-dired ()
  "Switch to the associated Dired buffer."
  (interactive)
  (if (and dired-image-thumbnail--dired-buffer
           (buffer-live-p dired-image-thumbnail--dired-buffer))
      (pop-to-buffer dired-image-thumbnail--dired-buffer)
    (when dired-image-thumbnail--source-dir
      (dired dired-image-thumbnail--source-dir))))

(defun dired-image-thumbnail-get-marked ()
  "Return list of marked images, or image at/near point if none marked."
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (let ((marked (when dired-image-thumbnail--current-images
                  (seq-filter #'dired-image-thumbnail--file-marked-p
                              dired-image-thumbnail--current-images))))
    (or marked
        (when-let ((file (dired-image-thumbnail--nearest-image-original-file-name)))
          (list file)))))

(defun dired-image-thumbnail-delete-marked ()
  "Delete marked images (or image at point if none marked)."
  (interactive)
  (let ((files (dired-image-thumbnail-get-marked)))
    (unless files
      (user-error "No images to delete"))
    (when (yes-or-no-p (format "Delete %d image(s)? " (length files)))
      (dolist (file files)
        (delete-file file t)
        (setq dired-image-thumbnail--current-images
              (delete file dired-image-thumbnail--current-images))
        (setq dired-image-thumbnail--all-images
              (delete file dired-image-thumbnail--all-images)))
      ;; Refresh dired buffer
      (when (and dired-image-thumbnail--dired-buffer
                 (buffer-live-p dired-image-thumbnail--dired-buffer))
        (with-current-buffer dired-image-thumbnail--dired-buffer
          (revert-buffer)))
      (dired-image-thumbnail-refresh)
      (message "Deleted %d image(s)" (length files)))))

(defun dired-image-thumbnail-delete ()
  "Delete the image at or near point."
  (interactive)
  (if-let ((file (dired-image-thumbnail--nearest-image-original-file-name)))
      (when (yes-or-no-p (format "Delete %s? " (file-name-nondirectory file)))
        ;; Find the next image to move to after deletion
        (let ((index (cl-position file dired-image-thumbnail--current-images :test #'equal)))
          (delete-file file t)
          (setq dired-image-thumbnail--current-images
                (delete file dired-image-thumbnail--current-images))
          (setq dired-image-thumbnail--all-images
                (delete file dired-image-thumbnail--all-images))
          ;; Refresh dired buffer
          (when (and dired-image-thumbnail--dired-buffer
                     (buffer-live-p dired-image-thumbnail--dired-buffer))
            (with-current-buffer dired-image-thumbnail--dired-buffer
              (revert-buffer)))
          (dired-image-thumbnail-refresh)
          ;; Move to the same index position (or last if we deleted the last one)
          (when dired-image-thumbnail--current-images
            (let ((target-index (min index (1- (length dired-image-thumbnail--current-images)))))
              (dired-image-thumbnail--goto-nth target-index)))
          (message "Deleted %s" (file-name-nondirectory file))))
    (user-error "No image at point")))

(defun dired-image-thumbnail--goto-nth (n)
  "Move point to the Nth thumbnail (0-indexed)."
  (goto-char (point-min))
  (dotimes (_ n)
    (image-dired-forward-image)))

(defun dired-image-thumbnail-toggle-wrap ()
  "Toggle between wrap display and standard line-up."
  (interactive)
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (setq dired-image-thumbnail-wrap-display (not dired-image-thumbnail-wrap-display))
  (dired-image-thumbnail-refresh)
  (message "Wrap display: %s" (if dired-image-thumbnail-wrap-display "ON" "OFF")))

;;; Other commands

(defun dired-image-thumbnail-help ()
  "Show help for image thumbnail commands."
  (interactive)
  (with-help-window "*Image Thumbnail Help*"
    (princ "Image Thumbnail Mode Commands:\n\n")
    (princ "Navigation:\n")
    (princ "  n, →         Next image")
    (when dired-image-thumbnail-auto-display-on-navigate
      (princ " (auto-display)"))
    (princ "\n")
    (princ "  p, ←         Previous image")
    (when dired-image-thumbnail-auto-display-on-navigate
      (princ " (auto-display)"))
    (princ "\n")
    (princ "  +/-          Increase/decrease size\n\n")
    (princ "Marking:\n")
    (princ "  m            Mark image (visual border)\n")
    (princ "  u            Unmark image\n")
    (princ "  U            Unmark all\n")
    (princ "  M            Mark all\n")
    (princ "  t            Toggle all marks\n\n")
    (princ "File Operations:\n")
    (princ "  D            Delete image at point\n")
    (princ "  C-d          Delete image and move to next\n")
    (princ "  x            Delete marked images\n")
    (princ "  d            Go to Dired buffer\n\n")
    (princ "Display:\n")
    (princ "  r            Refresh display\n")
    (princ "  w            Toggle wrap mode\n\n")
    (princ "Sorting (s prefix):\n")
    (princ "  sn           Sort by name\n")
    (princ "  sd           Sort by date\n")
    (princ "  ss           Sort by size\n")
    (princ "  sr           Reverse sort order\n\n")
    (princ "Filtering (/ prefix):\n")
    (princ "  /n           Filter by name\n")
    (princ "  /s           Filter by size\n")
    (princ "  /c           Clear filters\n\n")
    (princ "Subdirectories:\n")
    (princ "  Use 'i' in dired to insert subdirectories, or:\n")
    (princ "  M-x dired-image-thumbnail-insert-subdir-recursive\n")
    (princ "  M-x dired-image-thumbnail-insert-image-subdirs\n")
    (princ "  M-x dired-image-thumbnail-kill-all-subdirs\n\n")
    (princ "In Image Display Buffer:\n")
    (princ "  C-d          Delete image and move to next\n\n")
    (princ "Other:\n")
    (princ "  q            Quit window\n")
    (princ "  h, ?         This help\n")))

;;; Main entry point

(defun dired-image-thumbnail--find-subdirs (directory &optional max-depth)
  "Return a list of all subdirectories under DIRECTORY.
Does not include DIRECTORY itself.
Optional MAX-DEPTH limits recursion (nil means unlimited, 1 means direct children only)."
  (let ((subdirs nil)
        (dirs-to-process (list (cons directory 0))))
    (while dirs-to-process
      (let* ((item (pop dirs-to-process))
             (current-dir (car item))
             (current-depth (cdr item)))
        (dolist (file (directory-files current-dir t "^[^.]" t))
          (when (and (file-directory-p file)
                     (not (member (file-name-nondirectory file) '("." ".."))))
            (push file subdirs)
            ;; Only recurse if we haven't hit max depth
            (when (or (null max-depth) (< (1+ current-depth) max-depth))
              (push (cons file (1+ current-depth)) dirs-to-process))))))
    (nreverse subdirs)))

(defun dired-image-thumbnail--find-image-subdirs (directory &optional max-depth)
  "Return subdirectories under DIRECTORY that contain image files.
Optional MAX-DEPTH limits recursion depth."
  (let ((all-subdirs (dired-image-thumbnail--find-subdirs directory max-depth))
        (image-subdirs nil))
    (dolist (subdir all-subdirs)
      (when (dired-image-thumbnail--directory-has-images-p subdir)
        (push subdir image-subdirs)))
    (nreverse image-subdirs)))

(defun dired-image-thumbnail--directory-has-images-p (directory)
  "Return non-nil if DIRECTORY contains image files (non-recursive check)."
  (let ((has-images nil))
    (dolist (file (directory-files directory t "^[^.]" t))
      (when (and (not (file-directory-p file))
                 (dired-image-thumbnail--image-p file))
        (setq has-images t)))
    has-images))

(defun dired-image-thumbnail--insert-subdirs (subdirs)
  "Insert SUBDIRS into the current dired buffer.
SUBDIRS should be a list of directory paths."
  (let ((inserted 0))
    (dolist (subdir subdirs)
      (let ((subdir-path (file-name-as-directory subdir)))
        (condition-case err
            (progn
              (save-excursion
                ;; Check if this subdir is already inserted
                (goto-char (point-min))
                (unless (re-search-forward 
                        (concat "^  " (regexp-quote subdir-path) ":$") 
                        nil t)
                  (goto-char (point-max))
                  (dired-insert-subdir subdir-path)
                  (setq inserted (1+ inserted)))))
          (error
           (message "Could not insert subdir %s: %s" subdir-path err)))))
    inserted))

;;;###autoload
(defun dired-image-thumbnail ()
  "Display thumbnails for image files in current dired buffer.
If files are marked, show thumbnails for marked images only.
Otherwise, show thumbnails for all images visible in the dired buffer.

This works with inserted subdirectories - use 'i' (`dired-maybe-insert-subdir`)
to insert subdirectories before calling this command to include images from
those subdirectories. See `dired-image-thumbnail-insert-subdir-recursive'
for a helper to insert all subdirectories at once.

This function calls vanilla `image-dired' which triggers our hooks for
enhanced features like sorting and filtering."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (let ((dired-buf (current-buffer))
        (source-dir default-directory))
    ;; Store state for our hooks to use
    (setq-local dired-image-thumbnail--source-dir source-dir)
    (setq-local dired-image-thumbnail--dired-buffer dired-buf)
    
    ;; Call vanilla image-dired which will trigger our hooks and enhancements
    (call-interactively 'image-dired)))

;;;###autoload
(defun dired-image-thumbnail-insert-subdir-recursive (&optional max-depth)
  "Insert all subdirectories recursively into the current dired buffer.
Optional MAX-DEPTH limits recursion depth (nil means unlimited).
This makes images in subdirectories visible to `dired-image-thumbnail'.

Note: This can be slow for directories with many subdirectories.
Consider using `dired-image-thumbnail-insert-image-subdirs' instead,
which only inserts subdirectories that contain images."
  (interactive "P")
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (let* ((depth (if max-depth (prefix-numeric-value max-depth) nil))
         (subdirs (dired-image-thumbnail--find-subdirs default-directory depth)))
    (if subdirs
        (progn
          (message "Inserting %d subdirectories..." (length subdirs))
          (dired-image-thumbnail--insert-subdirs subdirs)
          (message "Inserted %d subdirectories" (length subdirs)))
      (message "No subdirectories found"))))

;;;###autoload
(defun dired-image-thumbnail-insert-image-subdirs (&optional max-depth)
  "Insert only subdirectories that contain image files.
Optional MAX-DEPTH limits recursion depth (nil means unlimited).
This is more efficient than `dired-image-thumbnail-insert-subdir-recursive'
for directories with many non-image subdirectories."
  (interactive "P")
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (let* ((depth (if max-depth (prefix-numeric-value max-depth) nil))
         (subdirs (dired-image-thumbnail--find-image-subdirs default-directory depth)))
    (if subdirs
        (progn
          (message "Inserting %d subdirectories with images..." (length subdirs))
          (dired-image-thumbnail--insert-subdirs subdirs)
          (message "Inserted %d subdirectories" (length subdirs)))
      (message "No subdirectories with images found"))))

;;;###autoload
(defun dired-image-thumbnail-kill-all-subdirs ()
  "Remove all inserted subdirectories from the current dired buffer.
This returns the view to just the top-level directory."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (let ((count 0))
    (save-excursion
      (goto-char (point-max))
      ;; Work backwards to avoid position issues
      (while (dired-get-subdir)
        (dired-kill-subdir)
        (setq count (1+ count))))
    (if (> count 0)
        (message "Removed %d subdirectories" count)
      (message "No subdirectories to remove"))))

;;;###autoload
;;; Keymaps

(defvar dired-image-thumbnail-sort-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'dired-image-thumbnail-sort-by-name)
    (define-key map (kbd "d") #'dired-image-thumbnail-sort-by-date)
    (define-key map (kbd "s") #'dired-image-thumbnail-sort-by-size)
    (define-key map (kbd "r") #'dired-image-thumbnail-sort-reverse)
    map)
  "Keymap for sorting commands.")

(defvar dired-image-thumbnail-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'dired-image-thumbnail-filter-by-name)
    (define-key map (kbd "s") #'dired-image-thumbnail-filter-by-size)
    (define-key map (kbd "/") #'dired-image-thumbnail-filter-clear)
    (define-key map (kbd "c") #'dired-image-thumbnail-filter-clear)
    map)
  "Keymap for filtering commands.")

;;;###autoload
(defun dired-image-thumbnail-setup-keys ()
  "Add dired-image-thumbnail keybindings to `image-dired-thumbnail-mode-map'."
  (define-key image-dired-thumbnail-mode-map (kbd "s") dired-image-thumbnail-sort-map)
  (define-key image-dired-thumbnail-mode-map (kbd "S") #'dired-image-thumbnail-sort)
  (define-key image-dired-thumbnail-mode-map (kbd "/") dired-image-thumbnail-filter-map)
  (define-key image-dired-thumbnail-mode-map (kbd "\\") #'dired-image-thumbnail-filter)
  (define-key image-dired-thumbnail-mode-map (kbd "w") #'dired-image-thumbnail-toggle-wrap)
  (define-key image-dired-thumbnail-mode-map (kbd "r") #'dired-image-thumbnail-refresh)
  (define-key image-dired-thumbnail-mode-map (kbd "+") #'dired-image-thumbnail-increase-size)
  (define-key image-dired-thumbnail-mode-map (kbd "-") #'dired-image-thumbnail-decrease-size)
  ;; Marking
  (define-key image-dired-thumbnail-mode-map (kbd "M") #'dired-image-thumbnail-mark-all)
  (define-key image-dired-thumbnail-mode-map (kbd "t") #'dired-image-thumbnail-toggle-all-marks)
  ;; File operations
  (define-key image-dired-thumbnail-mode-map (kbd "d") #'dired-image-thumbnail-goto-dired)
  (define-key image-dired-thumbnail-mode-map (kbd "D") #'dired-image-thumbnail-delete)
  (define-key image-dired-thumbnail-mode-map (kbd "C-d") #'dired-image-thumbnail-delete-and-next)
  (define-key image-dired-thumbnail-mode-map (kbd "x") #'dired-image-thumbnail-delete-marked)
  ;; Enhanced navigation with auto-display
  (when dired-image-thumbnail-auto-display-on-navigate
    (define-key image-dired-thumbnail-mode-map (kbd "n") #'dired-image-thumbnail-next-image)
    (define-key image-dired-thumbnail-mode-map (kbd "p") #'dired-image-thumbnail-previous-image))
  ;; Other
  (define-key image-dired-thumbnail-mode-map (kbd "?") #'dired-image-thumbnail-help)
  (define-key image-dired-thumbnail-mode-map (kbd "h") #'dired-image-thumbnail-help))

;;; Enhanced navigation and deletion

(defun dired-image-thumbnail-next-image ()
  "Move to next thumbnail and display full-size image."
  (interactive)
  (image-dired-forward-image)
  (image-dired-display-this))

(defun dired-image-thumbnail-previous-image ()
  "Move to previous thumbnail and display full-size image."
  (interactive)
  (image-dired-backward-image)
  (image-dired-display-this))

(defun dired-image-thumbnail-delete-and-next ()
  "Delete current image file and move to next thumbnail.
This permanently deletes the file from disk and removes its thumbnail."
  (interactive)
  (let ((file-name (image-dired-original-file-name)))
    (when (and file-name
               (y-or-n-p (format "Delete %s? " (file-name-nondirectory file-name))))
      (delete-file file-name)
      (image-dired-delete-char)
      (when (not (eobp))
        (image-dired-display-this))
      (message "Deleted %s" file-name))))

(defun dired-image-thumbnail-delete-image-and-next ()
  "Delete current image in image-mode buffer and move to next.
For use in the *image-dired-display-image* buffer."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when (and current-file
               (y-or-n-p (format "Delete %s? " (file-name-nondirectory current-file))))
      (image-next-file 1)
      (delete-file current-file)
      (message "Deleted %s" current-file))))

;;; Window layout management

(defun dired-image-thumbnail-setup-display-buffer ()
  "Configure display-buffer rules for thumbnail and image buffers."
  (when dired-image-thumbnail-manage-window-layout
    ;; Thumbnail buffer on the left
    (add-to-list 'display-buffer-alist
                 '("\\*image-dired\\*"
                   display-buffer-in-direction
                   (direction . left)
                   (window . root)
                   (window-width . 0.5)))
    ;; Image display buffer on the right
    (add-to-list 'display-buffer-alist
                 '("\\*image-dired-display-image\\*"
                   display-buffer-in-direction
                   (direction . right)
                   (window . root)
                   (window-width . 0.5)))))

;;;###autoload
(with-eval-after-load 'image-dired
  (dired-image-thumbnail-setup-keys)
  (dired-image-thumbnail-setup-display-buffer)
  ;; Hook to initialize our variables when entering thumbnail mode
  (add-hook 'image-dired-thumbnail-mode-hook #'dired-image-thumbnail--initialize-buffer))

;;;###autoload
(with-eval-after-load 'image-mode
  ;; Add C-d keybinding for deleting in image display buffer
  (define-key image-mode-map (kbd "C-d") #'dired-image-thumbnail-delete-image-and-next))

;; Load transient menu support if available
(when (require 'dired-image-thumbnail-transient nil t)
  (dired-image-thumbnail-transient-setup-keys))

(provide 'dired-image-thumbnail)
;;; dired-image-thumbnail.el ends here

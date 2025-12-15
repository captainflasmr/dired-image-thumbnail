;;; dired-image-thumbnail.el --- Enhanced workflow for image-dired -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Dyer

;; Author: James Dyer
;; Version: 0.1.0
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
;; - Sorting: Sort thumbnails by name, date, or size
;; - Filtering: Filter by name regexp or size range
;; - Recursive directory support: Browse images in subdirectories
;; - Wrap display mode: Thumbnails flow naturally and wrap to window width
;; - Enhanced header line: Shows current image info with sort/filter status
;;
;; Usage:
;;
;; After loading this package, the enhanced features are available in
;; `image-dired-thumbnail-mode' buffers via new keybindings:
;;
;;   s   - Sorting prefix (s n: name, s d: date, s s: size, s r: reverse)
;;   S   - Interactive sort selection
;;   /   - Filtering prefix (/ n: name, / s: size, / c: clear)
;;   R   - Toggle recursive directory search
;;   w   - Toggle wrap display mode
;;
;; Or call `dired-image-thumbnail-show-all-from-dir' for the enhanced entry point.

;;; Code:

(require 'image-dired)
(require 'image-dired-util)
(require 'image)
(require 'dired)

(declare-function image-size "image.c" (spec &optional pixels frame))

;;; Compatibility

(defun dired-image-thumbnail--insert-thumbnail (thumb-file original-file dired-buf image-number)
  "Insert thumbnail THUMB-FILE for ORIGINAL-FILE.
DIRED-BUF is the associated dired buffer.
IMAGE-NUMBER is the index of this image.
This is a compatibility wrapper for `image-dired-insert-thumbnail'."
  (condition-case nil
      ;; Try the 4-argument version (Emacs 29+)
      (image-dired-insert-thumbnail thumb-file original-file dired-buf image-number)
    (wrong-number-of-arguments
     ;; Fall back to 3-argument version (older Emacs) or other signatures
     (condition-case nil
         (with-no-warnings
           (image-dired-insert-thumbnail thumb-file original-file dired-buf))
       (wrong-number-of-arguments
        ;; Try with just 2 arguments
        (condition-case nil
            (with-no-warnings
              (image-dired-insert-thumbnail thumb-file original-file))
          (wrong-number-of-arguments
           ;; Last resort: insert manually
           (dired-image-thumbnail--insert-thumbnail-manual thumb-file original-file dired-buf image-number))))))))

(defun dired-image-thumbnail--insert-thumbnail-manual (thumb-file original-file dired-buf image-number)
  "Manually insert thumbnail THUMB-FILE for ORIGINAL-FILE.
DIRED-BUF is the associated dired buffer.
IMAGE-NUMBER is the index of this image."
  (let ((beg (point)))
    (insert-image (create-image thumb-file nil nil
                                :relief image-dired-thumb-relief
                                :margin image-dired-thumb-margin))
    (add-text-properties
     beg (point)
     (list 'image-dired-thumbnail t
           'original-file-name original-file
           'associated-dired-buffer dired-buf
           'image-number image-number
           'mouse-face 'highlight
           'keymap nil))))

;;; Customization

(defgroup dired-image-thumbnail nil
  "Enhanced workflow for image-dired."
  :group 'image-dired
  :prefix "dired-image-thumbnail-")

(defcustom dired-image-thumbnail-sort-by 'name
  "Default sorting criteria for thumbnails."
  :type '(choice (const :tag "Name" name)
                 (const :tag "Date modified" date)
                 (const :tag "Size" size))
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-sort-order 'ascending
  "Default sort order for thumbnails."
  :type '(choice (const :tag "Ascending" ascending)
                 (const :tag "Descending" descending))
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-recursive nil
  "Whether to search for images recursively in subdirectories by default."
  :type 'boolean
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-auto-recursive t
  "Whether to automatically search recursively when no images in current directory.
When non-nil, if the current directory contains no image files but has
subdirectories, the package will automatically search recursively."
  :type 'boolean
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-wrap-display nil
  "Whether to wrap thumbnails to fill the buffer width.
When non-nil, thumbnails flow naturally and wrap based on window width.
When nil, the standard `image-dired' line-up method is used."
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

(defvar-local dired-image-thumbnail--recursive nil
  "Whether current buffer is showing images recursively.")

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

(defun dired-image-thumbnail--has-subdirectories-p (directory)
  "Return non-nil if DIRECTORY has subdirectories."
  (let ((found nil))
    (dolist (file (directory-files directory t "^[^.]" t))
      (when (and (file-directory-p file)
                 (not (member (file-name-nondirectory file) '("." ".."))))
        (setq found t)))
    found))

(defun dired-image-thumbnail--relative-name (file)
  "Return FILE name relative to the source directory."
  (if (and dired-image-thumbnail--source-dir
           (file-name-absolute-p file))
      (file-relative-name file dired-image-thumbnail--source-dir)
    (file-name-nondirectory file)))

;;; File information

(defun dired-image-thumbnail--get-file-size-bytes (file)
  "Return file size in bytes for FILE."
  (or (file-attribute-size (file-attributes (expand-file-name file))) 0))

(defun dired-image-thumbnail--get-file-date (file)
  "Return modification time for FILE as a float."
  (let ((attrs (file-attributes (expand-file-name file))))
    (if attrs
        (float-time (file-attribute-modification-time attrs))
      0)))

(defun dired-image-thumbnail--format-file-size (file)
  "Return file size in human-readable format for FILE."
  (let ((size (dired-image-thumbnail--get-file-size-bytes file)))
    (if (and size (> size 0))
        (file-size-human-readable size)
      "?")))

;;; Sorting

(defun dired-image-thumbnail--sort-images (images)
  "Sort IMAGES according to current sort settings."
  (let ((sort-by (or dired-image-thumbnail--sort-by dired-image-thumbnail-sort-by))
        (sort-order (or dired-image-thumbnail--sort-order dired-image-thumbnail-sort-order)))
    (let ((sorted
           (sort (copy-sequence images)
                 (lambda (a b)
                   (pcase sort-by
                     ('name (string< (downcase (file-name-nondirectory a))
                                     (downcase (file-name-nondirectory b))))
                     ('date (< (dired-image-thumbnail--get-file-date a)
                               (dired-image-thumbnail--get-file-date b)))
                     ('size (< (dired-image-thumbnail--get-file-size-bytes a)
                               (dired-image-thumbnail--get-file-size-bytes b)))
                     (_ (string< a b)))))))
      (if (eq sort-order 'descending)
          (nreverse sorted)
        sorted))))

;;; Filtering

(defun dired-image-thumbnail--filter-images (images)
  "Filter IMAGES according to current filter settings."
  (let ((result images))
    ;; Filter by name
    (when dired-image-thumbnail--filter-name
      (setq result
            (seq-filter (lambda (f)
                          (string-match-p dired-image-thumbnail--filter-name
                                          (file-name-nondirectory f)))
                        result)))
    ;; Filter by file size
    (when dired-image-thumbnail--filter-size-min
      (setq result
            (seq-filter (lambda (f)
                          (>= (dired-image-thumbnail--get-file-size-bytes f)
                              dired-image-thumbnail--filter-size-min))
                        result)))
    (when dired-image-thumbnail--filter-size-max
      (setq result
            (seq-filter (lambda (f)
                          (<= (dired-image-thumbnail--get-file-size-bytes f)
                              dired-image-thumbnail--filter-size-max))
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
      (when images
        (setq dired-image-thumbnail--all-images (nreverse images))
        (setq dired-image-thumbnail--current-images dired-image-thumbnail--all-images)
        (setq dired-image-thumbnail--dired-buffer dired-buf)
        (setq dired-image-thumbnail--source-dir source-dir)
        (setq dired-image-thumbnail--sort-by dired-image-thumbnail-sort-by)
        (setq dired-image-thumbnail--sort-order dired-image-thumbnail-sort-order)))))

;;; Header line

(defun dired-image-thumbnail--header-line ()
  "Generate enhanced header line."
  (let* ((file (image-dired-original-file-name))
         (recursive-info (if dired-image-thumbnail--recursive " [recursive]" ""))
         (sort-info (format "[%s %s]"
                            (or dired-image-thumbnail--sort-by dired-image-thumbnail-sort-by)
                            (if (eq (or dired-image-thumbnail--sort-order
                                        dired-image-thumbnail-sort-order)
                                    'ascending)
                                "↑" "↓")))
         (filter-info (dired-image-thumbnail--format-active-filters))
         (showing (length dired-image-thumbnail--current-images))
         (total (length dired-image-thumbnail--all-images)))
    (if file
        (let* ((rel-name (dired-image-thumbnail--relative-name file))
               (size (dired-image-thumbnail--format-file-size file))
               (image-count (format "%d/%d" (get-text-property (point) 'image-number) total)))
          (concat
           "  "
           (propertize rel-name 'face 'image-dired-thumb-header-file-name)
           "  "
           (propertize image-count 'face 'image-dired-thumb-header-image-count)
           "  "
           (propertize size 'face 'image-dired-thumb-header-file-size)
           "  "
           sort-info
           (if (string-empty-p filter-info) "" (format "  %s" filter-info))
           recursive-info))
      (format "Image-Dired+ | %d/%d images %s%s%s"
              showing total
              sort-info
              (if (string-empty-p filter-info) "" (format " %s" filter-info))
              recursive-info))))

;;; Display functions

(defun dired-image-thumbnail--display-thumbs-internal (images source-dir dired-buf recursive)
  "Display IMAGES thumbnails with enhanced features.
SOURCE-DIR is the original directory.
DIRED-BUF is the associated dired buffer.
RECURSIVE indicates if images were found recursively."
  (let ((buf (image-dired-create-thumbnail-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Set buffer-local variables
        (setq dired-image-thumbnail--all-images images)
        (setq dired-image-thumbnail--source-dir source-dir)
        (setq dired-image-thumbnail--dired-buffer dired-buf)
        (setq dired-image-thumbnail--recursive recursive)
        ;; Initialize sort settings
        (setq dired-image-thumbnail--sort-by dired-image-thumbnail-sort-by)
        (setq dired-image-thumbnail--sort-order dired-image-thumbnail-sort-order)
        ;; Apply filter and sort
        (let ((filtered (dired-image-thumbnail--filter-images images)))
          (setq dired-image-thumbnail--current-images
                (dired-image-thumbnail--sort-images filtered)))
        ;; Insert thumbnails
        (setq image-dired--number-of-thumbnails 0)
        (dolist (file dired-image-thumbnail--current-images)
          (let ((thumb-file (image-dired-thumb-name file)))
            (dired-image-thumbnail--insert-thumbnail
             (if (file-exists-p thumb-file)
                 thumb-file
               (image-dired--get-create-thumbnail-file file))
             file dired-buf
             (cl-incf image-dired--number-of-thumbnails))))
        ;; Line up
        (if dired-image-thumbnail-wrap-display
            (progn
              (setq-local word-wrap t)
              (setq-local truncate-lines nil))
          (image-dired--line-up-with-method))
        ;; Set enhanced header line
        ;; (setq-local header-line-format '(:eval (dired-image-thumbnail--header-line)))
        (goto-char (point-min))
        (image-dired-forward-image)))
    (pop-to-buffer buf)
    buf))

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
          (recursive dired-image-thumbnail--recursive)
          (all-images dired-image-thumbnail--all-images)
          (inhibit-read-only t))
      (erase-buffer)
      ;; Restore state
      (setq dired-image-thumbnail--all-images all-images)
      (setq dired-image-thumbnail--source-dir source-dir)
      (setq dired-image-thumbnail--dired-buffer dired-buf)
      (setq dired-image-thumbnail--recursive recursive)
      (setq dired-image-thumbnail--sort-by sort-by)
      (setq dired-image-thumbnail--sort-order sort-order)
      (setq dired-image-thumbnail--filter-name filter-name)
      (setq dired-image-thumbnail--filter-size-min filter-size-min)
      (setq dired-image-thumbnail--filter-size-max filter-size-max)
      ;; Apply filter and sort
      (let ((filtered (dired-image-thumbnail--filter-images all-images)))
        (setq dired-image-thumbnail--current-images
              (dired-image-thumbnail--sort-images filtered)))
      ;; Insert thumbnails
      (setq image-dired--number-of-thumbnails 0)
      (dolist (file dired-image-thumbnail--current-images)
        (let ((thumb-file (image-dired-thumb-name file)))
          ;; Only insert if thumbnail exists, otherwise create it
          (dired-image-thumbnail--insert-thumbnail
           (if (file-exists-p thumb-file)
               thumb-file
             (image-dired--get-create-thumbnail-file file))
           file dired-buf
           (cl-incf image-dired--number-of-thumbnails))))
      ;; Line up
      (if dired-image-thumbnail-wrap-display
          (progn
            (setq-local word-wrap t)
            (setq-local truncate-lines nil))
        (image-dired--line-up-with-method))
      ;; Set header line
      ;; (setq-local header-line-format '(:eval (dired-image-thumbnail--header-line)))
      ;; Restore position
      (goto-char (point-min))
      (when current-file
        (dired-image-thumbnail--goto-file current-file))
      (unless (image-dired-image-at-point-p)
        (image-dired-forward-image)))))

(defun dired-image-thumbnail--goto-file (file)
  "Move point to the thumbnail for FILE."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (equal file (get-text-property (point) 'original-file-name))
          (setq found t)
        (goto-char (next-single-property-change (point) 'original-file-name nil (point-max)))))
    found))

;;; Interactive commands

;;;###autoload
(defun dired-image-thumbnail-show-all-from-dir (dir &optional recursive)
  "Show thumbnails for all images in DIR with enhanced features.
With prefix argument RECURSIVE, include images from subdirectories."
  (interactive "DShow thumbnails for directory: \nP")
  (let* ((recursive-p (or recursive
                          dired-image-thumbnail-recursive
                          ;; Auto-recursive: no local images but has subdirs
                          (and dired-image-thumbnail-auto-recursive
                               (null (dired-image-thumbnail--find-images dir nil))
                               (dired-image-thumbnail--has-subdirectories-p dir))))
         (images (dired-image-thumbnail--find-images dir recursive-p)))
    (unless images
      (user-error "No image files found%s"
                  (if recursive-p " (searched recursively)" "")))
    (when (and dired-image-thumbnail-auto-recursive
               (not recursive)
               (not dired-image-thumbnail-recursive)
               recursive-p)
      (message "No images in current directory, searching recursively..."))
    (message "Found %d image files%s"
             (length images)
             (if recursive-p " (recursive)" ""))
    ;; Create a dired buffer for the directory
    (let ((dired-buf (dired-noselect dir)))
      (dired-image-thumbnail--display-thumbs-internal images dir dired-buf recursive-p))))

;;;###autoload
(defun dired-image-thumbnail-show-all-recursive (dir)
  "Show thumbnails for all images in DIR recursively."
  (interactive "DShow thumbnails recursively for directory: ")
  (dired-image-thumbnail-show-all-from-dir dir t))

;;; Sorting commands

(defun dired-image-thumbnail-sort ()
  "Interactively choose sort criteria."
  (interactive)
  (let ((choice (completing-read "Sort by: "
                                 '("name" "date" "size")
                                 nil t)))
    (setq dired-image-thumbnail--sort-by (intern choice))
    (dired-image-thumbnail--apply-sort-and-filter)
    (message "Sorted by %s %s"
             choice
             (if (eq dired-image-thumbnail--sort-order 'ascending) "↑" "↓"))))

(defun dired-image-thumbnail-sort-reverse ()
  "Reverse the current sort order."
  (interactive)
  (setq dired-image-thumbnail--sort-order
        (if (eq dired-image-thumbnail--sort-order 'ascending)
            'descending
          'ascending))
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sort order: %s"
           (if (eq dired-image-thumbnail--sort-order 'ascending)
               "ascending"
             "descending")))

(defun dired-image-thumbnail-sort-by-name ()
  "Sort thumbnails by filename."
  (interactive)
  (setq dired-image-thumbnail--sort-by 'name)
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sorted by name"))

(defun dired-image-thumbnail-sort-by-date ()
  "Sort thumbnails by modification date."
  (interactive)
  (setq dired-image-thumbnail--sort-by 'date)
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sorted by date"))

(defun dired-image-thumbnail-sort-by-size ()
  "Sort thumbnails by file size."
  (interactive)
  (setq dired-image-thumbnail--sort-by 'size)
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sorted by size"))

;;; Filtering commands

(defun dired-image-thumbnail-filter-by-name ()
  "Filter images by filename regexp."
  (interactive)
  (let ((regexp (read-regexp "Filter by name (regexp): ")))
    (if (string-empty-p regexp)
        (setq dired-image-thumbnail--filter-name nil)
      (setq dired-image-thumbnail--filter-name regexp))
    (dired-image-thumbnail--apply-sort-and-filter)
    (message "Name filter: %s" (or dired-image-thumbnail--filter-name "none"))))

(defun dired-image-thumbnail-filter-by-size ()
  "Filter images by file size range."
  (interactive)
  (let* ((min-str (read-string "Minimum size (e.g., 100K, 1M, empty for none): "))
         (max-str (read-string "Maximum size (e.g., 10M, 100M, empty for none): "))
         (min-bytes (dired-image-thumbnail--parse-size min-str))
         (max-bytes (dired-image-thumbnail--parse-size max-str)))
    (setq dired-image-thumbnail--filter-size-min min-bytes)
    (setq dired-image-thumbnail--filter-size-max max-bytes)
    (dired-image-thumbnail--apply-sort-and-filter)
    (message "Size filter: %s to %s"
             (if min-bytes (file-size-human-readable min-bytes) "0")
             (if max-bytes (file-size-human-readable max-bytes) "∞"))))

(defun dired-image-thumbnail--parse-size (str)
  "Parse size string STR into bytes.
Accepts formats like: 100, 100K, 1M, 1G"
  (when (and str (not (string-empty-p str)))
    (let ((num (string-to-number str))
          (suffix (upcase (substring str -1))))
      (cond
       ((string= suffix "G") (* num 1024 1024 1024))
       ((string= suffix "M") (* num 1024 1024))
       ((string= suffix "K") (* num 1024))
       (t num)))))

(defun dired-image-thumbnail-filter-clear ()
  "Clear all filters."
  (interactive)
  (setq dired-image-thumbnail--filter-name nil)
  (setq dired-image-thumbnail--filter-size-min nil)
  (setq dired-image-thumbnail--filter-size-max nil)
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "All filters cleared"))

(defun dired-image-thumbnail-filter ()
  "Interactively choose filter type."
  (interactive)
  (let ((choice (completing-read "Filter by: "
                                 '("name" "size" "clear all")
                                 nil t)))
    (pcase choice
      ("name" (dired-image-thumbnail-filter-by-name))
      ("size" (dired-image-thumbnail-filter-by-size))
      ("clear all" (dired-image-thumbnail-filter-clear)))))

;;; Toggle commands

(defun dired-image-thumbnail-toggle-recursive ()
  "Toggle recursive display and reload images."
  (interactive)
  ;; Initialize if not already done
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (let ((new-recursive (not dired-image-thumbnail--recursive))
        (source-dir dired-image-thumbnail--source-dir))
    (unless source-dir
      (setq source-dir default-directory))
    (message "Searching for images%s..."
             (if new-recursive " recursively" ""))
    (let ((images (dired-image-thumbnail--find-images source-dir new-recursive)))
      (if images
          (progn
            (setq dired-image-thumbnail--all-images images)
            (setq dired-image-thumbnail--source-dir source-dir)
            (setq dired-image-thumbnail--recursive new-recursive)
            (dired-image-thumbnail--apply-sort-and-filter)
            (message "Found %d image files%s"
                     (length images)
                     (if new-recursive " (recursive)" "")))
        (message "No image files found%s"
                 (if new-recursive " recursively" ""))))))

(defun dired-image-thumbnail-toggle-wrap ()
  "Toggle between wrap display and standard line-up."
  (interactive)
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (setq dired-image-thumbnail-wrap-display (not dired-image-thumbnail-wrap-display))
  (dired-image-thumbnail-refresh)
  (message "Wrap display: %s" (if dired-image-thumbnail-wrap-display "ON" "OFF")))

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
  (define-key image-dired-thumbnail-mode-map (kbd "R") #'dired-image-thumbnail-toggle-recursive)
  (define-key image-dired-thumbnail-mode-map (kbd "w") #'dired-image-thumbnail-toggle-wrap)
  (define-key image-dired-thumbnail-mode-map (kbd "r") #'dired-image-thumbnail-refresh))

;; Automatically set up keys when loaded
;;;###autoload
(with-eval-after-load 'image-dired
  (dired-image-thumbnail-setup-keys))

(provide 'dired-image-thumbnail)
;;; dired-image-thumbnail.el ends here

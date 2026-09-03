;;; dired-image-thumbnail.el --- Enhanced workflow for image-dired -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Dyer

;; Author: James Dyer
;; Version: 2.6.0
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
;; - Enhanced header line: Shows current image info, directory location,
;;   sort/filter status
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
;;   s   - Sort (completing-read: dired/name/date/size/reverse)
;;   /   - Filter (completing-read: name/size/clear)
;;   g   - Refresh display
;;   n/p, f/b - Next/previous image (with auto-display when enabled)
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
;;   z   - Insert subdirs (recursive) into the associated dired buffer
;;   ?   - Transient menu
;;

;;; Code:

(require 'image-dired)
(require 'image-dired-util)
(require 'image)
(require 'dired)
(require 'cl-lib)
(require 'subr-x)                       ; string-empty-p, string-trim

(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function w32-shell-execute "w32fns.c"
                  (operation document &optional parameters show-flag))
(declare-function dired-image-thumbnail-transient-setup-keys
                  "dired-image-thumbnail-transient")

;; This package builds on a number of internal (double-dash) `image-dired'
;; functions.  They are stable across the supported Emacs versions (28.1+)
;; but are not part of image-dired's public API, so they are declared here
;; both to document the dependency and to satisfy the byte-compiler.
(declare-function image-dired--line-up-with-method "image-dired")
(declare-function image-dired--thumb-update-marks "image-dired")
(declare-function image-dired--update-header-line "image-dired")

;;; Customization

(defgroup dired-image-thumbnail nil
  "Enhanced workflow for image-dired."
  :group 'image-dired
  :prefix "dired-image-thumbnail-")

(defcustom dired-image-thumbnail-sort-by 'dired
  "Default sorting criteria for thumbnails."
  :type '(choice (const :tag "Dired Order" dired)
                 (const :tag "Name" name)
                 (const :tag "Date modified" date)
                 (const :tag "Size" size)
                 (const :tag "Dimensions" dimensions))
  :safe #'symbolp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-sort-order 'ascending
  "Default sort order for thumbnails."
  :type '(choice (const :tag "Ascending" ascending)
                 (const :tag "Descending" descending))
  :safe #'symbolp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-wrap-display nil
  "Whether to wrap thumbnails to fill the buffer width.
When non-nil, thumbnails flow naturally and wrap based on window width.
When nil, the standard `image-dired' line-up method is used."
  :type 'boolean
  :safe #'booleanp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-square-thumbnails nil
  "Whether to crop thumbnails to uniform squares for a tidier grid.
When non-nil, each thumbnail is scaled to fill and center-cropped to
a uniform square of `image-dired-thumb-size' pixels so that all
thumbnails have the same dimensions and the grid lines up neatly.
This requires ImageMagick (`mogrify' or `magick').  Existing
thumbnails are cropped on the next refresh; use `G' (hard refresh) to
regenerate them from scratch."
  :type 'boolean
  :safe #'booleanp
  :group 'dired-image-thumbnail)

(defface dired-image-thumbnail-current-thumbnail
  '((t (:box (:line-width 5))))
  "Face used to highlight the currently selected thumbnail.
The outline is thick by default (5 pixels).  Its colour follows the
active theme: the background colour of the `highlight' face is
used, so the outline looks like the theme's own highlight, just
thicker.  Customise this face to change the width, or give :box an
explicit :color to pin a fixed colour.  Set
`dired-image-thumbnail-highlight-current-thumbnail' to nil to
disable the highlight entirely."
  :group 'dired-image-thumbnail)

(defface dired-image-thumbnail-header-info
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white"))
    (t nil))
  "Face used for the whole thumbnail header line.
Uses a maximum-contrast foreground colour -- black on light
backgrounds, white on dark ones -- so the information stays
clearly readable on most themes, independently of how the theme
styles the image-dired header faces.  Unspecified attributes
inherit from the `header-line' face.  Customize this face to
adjust the colours."
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-highlight-current-thumbnail t
  "Whether to highlight the currently selected thumbnail.
When non-nil, the thumbnail at point is outlined using the
`dired-image-thumbnail-current-thumbnail' face, making the selection
clear regardless of the current theme."
  :type 'boolean
  :safe #'booleanp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-highlight-cursor t
  "Whether to colour the cursor in the thumbnail buffer.
When non-nil, the cursor is recoloured to match the
`dired-image-thumbnail-current-thumbnail' highlight, making it easier
to spot.  The colour is derived from that face's :box attribute, so
customising the face also changes the cursor.  Applied buffer-locally,
so only thumbnail buffers are affected."
  :type 'boolean
  :safe #'booleanp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-window-layout 'left-right
  "Window layout used when launching `dired-image-thumbnail'.

  `thumb-only'  - Only show the thumbnail buffer in a single window (default).
  `left-right'  - Thumbnails on the left, image on the right.
  `right-left'  - Image on the left, thumbnails on the right.
  `top-bottom'  - Thumbnails on top, image on the bottom.
  `bottom-top'  - Image on top, thumbnails on the bottom.
  nil           - Do not manage windows; use Emacs default placement
                  or your own `display-buffer-alist' rules.

The thumbnail/image size ratio is controlled by
`dired-image-thumbnail-window-ratio'."
  :type '(choice (const :tag "Thumbnails only (single window)" thumb-only)
                 (const :tag "Thumbnails left, image right" left-right)
                 (const :tag "Image left, thumbnails right" right-left)
                 (const :tag "Thumbnails top, image bottom" top-bottom)
                 (const :tag "Image top, thumbnails bottom" bottom-top)
                 (const :tag "Manual (use display-buffer-alist)" nil))
  :safe #'symbolp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-window-ratio 0.7
  "Fraction of the frame given to the thumbnail buffer.
The image buffer gets the remainder.  Only used when
`dired-image-thumbnail-window-layout' is non-nil."
  :type 'float
  :safe #'numberp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-auto-display-on-navigate t
  "Whether to automatically display full-size image when navigating thumbnails.
When non-nil, pressing `n` or `p` in the thumbnail buffer automatically
updates the image display buffer, and marking a file advances to the
next thumbnail and displays it.  When nil, navigation and marking only
move point, avoiding the cost of decoding each image -- useful when
marking many files for a batch operation such as rotation.  Press RET
or C-<return> to view the full-size image.
Toggle interactively with `dired-image-thumbnail-toggle-auto-display'."
  :type 'boolean
  :safe #'booleanp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-auto-accept nil
  "Whether to skip confirmation for file actions like deletion.
When non-nil, actions that normally ask for confirmation (like
deleting files) will proceed without prompting."
  :type 'boolean
  :safe #'booleanp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-image-extensions
  '("jpg" "jpeg" "png" "gif" "bmp" "tiff" "tif" "webp" "svg" "ico" "heic" "heif")
  "List of image file extensions to recognise."
  :type '(repeat string)
  :safe (lambda (v) (and (listp v) (cl-every #'stringp v)))
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-minimum-file-size 100
  "Minimum file size in bytes for an image to be included.
Files smaller than this are assumed to be corrupt or empty and
are silently excluded from the thumbnail display.  Set to 0 to
disable this check."
  :type 'natnum
  :safe #'natnump
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-validate-headers t
  "Whether to check image file magic bytes before including them.
When non-nil, each candidate file is opened briefly to verify
that its first few bytes match a known image format signature.
This catches files that have an image extension but contain
garbage or are truncated.  The check reads only the first 12
bytes per file, so the overhead is small.  Set to nil if you
trust all files in your directories or want maximum speed."
  :type 'boolean
  :safe #'booleanp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-external-editor nil
  "External program used to open images with \\`W'.
When nil, the system default application is used via `xdg-open'
on Linux, `open' on macOS, or `start' on Windows."
  :type '(choice (const :tag "System default" nil)
                 (string :tag "Program name"))
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-display-quality 'faster
  "Display quality when navigating thumbnails with n/p.
Controls the trade-off between image quality and navigation speed.

  `full'   - Original resolution via `image-dired-display-image'.
             Slowest, but pixel-perfect.
  `high'   - Scaled to fit the display window (1:1 window pixels).
  `fast'   - Half the window dimensions, JPEG quality 60.
  `faster' - Quarter the window dimensions, JPEG quality 40.
  `draft'  - 1/8 the window dimensions, JPEG quality 25.  Very fast,
             visibly soft; two neighbours are pre-generated instead
             of one.

Preview files are optimally compressed per mode and the previews of
neighbouring images are pre-generated while idle, so navigating with
follow mode on usually shows the next image instantly.  Displayed
previews are kept in the image cache, so revisiting an image costs
nothing.

Interactively select the quality with
`dired-image-thumbnail-select-display-quality' (bound to `Q`),
which takes effect immediately.  Customising this variable takes
effect on the next n/p keypress."
  :type '(choice (const :tag "Full resolution (slowest)" full)
                 (const :tag "High - window size" high)
                 (const :tag "Fast - 1/2 window" fast)
                 (const :tag "Faster - 1/4 window (default)" faster)
                 (const :tag "Draft - 1/8 window (fastest)" draft))
:safe #'symbolp
   :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-suppress-display-lockfiles t
  "Whether to suppress Emacs lock files when displaying full-size images.
When non-nil, no `.#filename' lock files are created on image files
visited via `image-dired-display-image' (the standard `image-dired'
display path used at `full' quality).
This prevents stale lock-file residue from accumulating in image
directories, especially after abnormal Emacs exit.  Set to nil if
you want the standard Emacs locking behaviour."
  :type 'boolean
  :safe #'booleanp
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

(defvar-local dired-image-thumbnail--recursive nil
  "Non-nil if thumbnails include images from subdirectories.")

(defvar-local dired-image-thumbnail--marked-count nil
  "Cached count of marked images.  Nil means it needs recomputation.")

(defvar-local dired-image-thumbnail--lineup-width nil
  "Width in columns of the thumbnail window at the last line-up.
When showing the full-size image changes the window layout (and so
this width), the thumbnails are refreshed so they re-align and the
columns fit the new window width.")

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

(defun dired-image-thumbnail--prop-search (pos forward)
  "Find the nearest `original-file-name' property near POS.
Search forward when FORWARD is non-nil, otherwise backward.  Jumps
between property-change boundaries (O(runs)) rather than scanning one
character at a time.  Returns the property value, or nil if none."
  (let ((p pos)
        (found (get-text-property pos 'original-file-name)))
    (while (and (not found)
                (if forward (< p (point-max)) (> p (point-min))))
      (setq p (if forward
                  (next-single-property-change p 'original-file-name nil (point-max))
                (previous-single-property-change p 'original-file-name nil (point-min))))
      (setq found (get-text-property p 'original-file-name)))
    found))

(defun dired-image-thumbnail--nearest-image-original-file-name (&optional pos)
  "Return the `original-file-name' property at POS, or the nearest one.
POS defaults to point.  If no thumbnail is exactly at POS, search
backward first, then forward, for the closest position carrying the
property."
  (let ((pos (or pos (point))))
    (or (get-text-property pos 'original-file-name)
        (dired-image-thumbnail--prop-search pos nil)
        (dired-image-thumbnail--prop-search pos t))))

(defun dired-image-thumbnail--start-identify-process (file)
  "Start an async process to get dimensions for FILE."
  (let ((proc-buf (generate-new-buffer " *dired-image-thumb-identify*"))
        (thumb-buf (current-buffer))
        (file-attr file))
    (make-process
     :name "dired-image-thumb-identify"
     :buffer proc-buf
     :command (list "identify" "-format" "%w %h" (expand-file-name file))
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (unwind-protect
             (when (and (zerop (process-exit-status proc))
                        (buffer-live-p (process-buffer proc)))
               (with-current-buffer (process-buffer proc)
                 (goto-char (point-min))
                 (let* ((line (buffer-substring-no-properties (point-min) (point-max)))
                        (nums (split-string line)))
                   (when (and (= (length nums) 2)
                              (string-match-p "^[0-9]+$" (car nums))
                              (string-match-p "^[0-9]+$" (cadr nums)))
                     (let ((w (string-to-number (car nums)))
                           (h (string-to-number (cadr nums))))
                       (when (buffer-live-p thumb-buf)
                         (with-current-buffer thumb-buf
                           (puthash file-attr (cons w h) dired-image-thumbnail--dimension-cache)
                           (remhash file-attr dired-image-thumbnail--dimension-pending)))
                       (dolist (b (buffer-list))
                         (with-current-buffer b
                           (when (derived-mode-p 'image-dired-thumbnail-mode)
                             (image-dired--update-header-line)))))))))
           (when (buffer-live-p (process-buffer proc))
             (kill-buffer (process-buffer proc)))))))))

;;; Utility functions

(defun dired-image-thumbnail--valid-header-p (file)
  "Return non-nil if FILE begins with a recognised image magic signature.
Reads only the first 12 bytes.  Recognised formats: JPEG, PNG, GIF,
BMP, TIFF, WEBP, ICO, HEIC/HEIF.  SVG is accepted without a byte
check since it is XML text."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (if (equal ext "svg")
        t
      (condition-case nil
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file nil 0 12)
            (let ((bytes (buffer-string)))
              (when (>= (length bytes) 2)
                (let ((b0 (aref bytes 0))
                      (b1 (aref bytes 1)))
                  (cond
                   ;; JPEG: FF D8
                   ((and (= b0 #xFF) (= b1 #xD8)) t)
                   ;; PNG: 89 50 4E 47
                   ((and (>= (length bytes) 4)
                         (= b0 #x89) (= b1 #x50)
                         (= (aref bytes 2) #x4E) (= (aref bytes 3) #x47))
                    t)
                   ;; GIF: "GIF8"
                   ((and (>= (length bytes) 4)
                         (= b0 ?G) (= b1 ?I)
                         (= (aref bytes 2) ?F) (= (aref bytes 3) ?8))
                    t)
                   ;; BMP: "BM"
                   ((and (= b0 ?B) (= b1 ?M)) t)
                   ;; TIFF: "II" (little-endian) or "MM" (big-endian)
                   ((and (>= (length bytes) 4)
                         (or (and (= b0 #x49) (= b1 #x49)
                                  (= (aref bytes 2) #x2A) (= (aref bytes 3) #x00))
                             (and (= b0 #x4D) (= b1 #x4D)
                                  (= (aref bytes 2) #x00) (= (aref bytes 3) #x2A))))
                    t)
                   ;; WEBP: "RIFF" + 4 bytes + "WEBP"
                   ((and (>= (length bytes) 12)
                         (= b0 ?R) (= b1 ?I)
                         (= (aref bytes 2) ?F) (= (aref bytes 3) ?F)
                         (= (aref bytes 8) ?W) (= (aref bytes 9) ?E)
                         (= (aref bytes 10) ?B) (= (aref bytes 11) ?P))
                    t)
                   ;; ICO: 00 00 01 00
                   ((and (>= (length bytes) 4)
                         (= b0 #x00) (= b1 #x00)
                         (= (aref bytes 2) #x01) (= (aref bytes 3) #x00))
                    t)
                   ;; HEIC/HEIF: bytes 4-11 contain "ftyp" for ISO BMFF
                   ((and (>= (length bytes) 8)
                         (= (aref bytes 4) ?f) (= (aref bytes 5) ?t)
                         (= (aref bytes 6) ?y) (= (aref bytes 7) ?p))
                    t))))))
        (file-error nil)))))

(defun dired-image-thumbnail--image-p (file)
  "Return non-nil if FILE is a valid image file.
Checks extension, minimum file size, and optionally magic bytes."
  (and (file-regular-p file)
       (member (downcase (or (file-name-extension file) ""))
               dired-image-thumbnail-image-extensions)
       (or (null dired-image-thumbnail-minimum-file-size)
           (zerop dired-image-thumbnail-minimum-file-size)
           (let ((attrs (file-attributes file)))
             (and attrs
                  (>= (file-attribute-size attrs)
                      dired-image-thumbnail-minimum-file-size))))
       (or (not dired-image-thumbnail-validate-headers)
           (dired-image-thumbnail--valid-header-p file))))

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

(defun dired-image-thumbnail--get-dired-marked-set ()
  "Return a hash set of all marked files in the associated dired buffer.
This collects all marks in a single pass through the dired buffer."
  (let ((marked (make-hash-table :test 'equal)))
    (when (and dired-image-thumbnail--dired-buffer
               (buffer-live-p dired-image-thumbnail--dired-buffer))
      (with-current-buffer dired-image-thumbnail--dired-buffer
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (image-dired-dired-file-marked-p)
              (let ((file (dired-get-filename nil t)))
                (when file
                  (puthash file t marked))))
            (forward-line 1)))))
    marked))

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
  "Count the number of marked images.
The result is cached in `dired-image-thumbnail--marked-count' and only
recomputed when the cache has been invalidated (see
`dired-image-thumbnail--invalidate-marked-count'), so the header line
can be updated on every navigation without re-scanning the dired buffer."
  (or dired-image-thumbnail--marked-count
      (setq dired-image-thumbnail--marked-count
            (if dired-image-thumbnail--current-images
                (let ((marked-set (dired-image-thumbnail--get-dired-marked-set))
                      (count 0))
                  (dolist (file dired-image-thumbnail--current-images)
                    (when (gethash file marked-set)
                      (setq count (1+ count))))
                  count)
              0))))

(defun dired-image-thumbnail--invalidate-marked-count (&rest _)
  "Invalidate the cached marked count in the current thumbnail buffer.
Installed as `:after' advice on `image-dired--thumb-update-marks', which
is the common choke point for all mark changes (both ours and native
image-dired commands)."
  (when (derived-mode-p 'image-dired-thumbnail-mode)
    (setq dired-image-thumbnail--marked-count nil)))

;;; Sorting functions

(defun dired-image-thumbnail--sort-images (images)
  "Sort IMAGES according to current sort settings."
  (let* ((sort-by (or dired-image-thumbnail--sort-by dired-image-thumbnail-sort-by))
         (sort-order (or dired-image-thumbnail--sort-order dired-image-thumbnail-sort-order))
         (sorted
          (pcase sort-by
            ('dired (copy-sequence images))
            ('name
             (sort (copy-sequence images)
                   (lambda (a b)
                     (string< (downcase (file-name-nondirectory a))
                              (downcase (file-name-nondirectory b))))))
            ('date
             (let ((decorated (mapcar (lambda (f)
                                        (cons (file-attribute-modification-time (file-attributes f)) f))
                                      images)))
               (mapcar #'cdr (sort decorated (lambda (a b) (time-less-p (car a) (car b)))))))
            ('size
             (let ((decorated (mapcar (lambda (f)
                                        (cons (or (file-attribute-size (file-attributes f)) 0) f))
                                      images)))
               (mapcar #'cdr (sort decorated (lambda (a b) (< (car a) (car b)))))))
            (_ (copy-sequence images)))))
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
  "Apply current sort and filter settings and refresh display.
The actual filtering and sorting is performed by
`dired-image-thumbnail-refresh', so this only ensures the buffer is
initialised before refreshing."
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (dired-image-thumbnail-refresh))

;;; Initialization for standard image-dired

(defun dired-image-thumbnail--initialize-buffer ()
  "Initialize dired-image-thumbnail variables in the current thumbnail buffer.
This is called via hook when entering `image-dired-thumbnail-mode'."
  ;; Keep the current-thumbnail highlight in step with point movement.
  (add-hook 'post-command-hook #'dired-image-thumbnail--update-current-highlight nil t)
  ;; Colour the cursor to match, buffer-locally.
  (dired-image-thumbnail--setup-cursor)
  ;; Keep image-dired's mark-and-advance display in step with auto-display.
  (dired-image-thumbnail--disable-marking-shows-next)
  ;; Skip if already initialized and has images
  (unless (and dired-image-thumbnail--all-images
               dired-image-thumbnail--dired-buffer)
    ;; Small delay to ensure image-dired has started inserting properties
    ;; though we prefer finding the dired buffer through other means if possible
    (let ((images nil)
          (dired-buf dired-image-thumbnail--dired-buffer)
          (source-dir dired-image-thumbnail--source-dir))
      
      ;; If not explicitly passed, try to find from text properties.
      ;; Jump between property-change boundaries (O(runs)) rather than
      ;; scanning one character at a time.
      (unless dired-buf
        (save-excursion
          (goto-char (point-min))
          (let ((pos (point-min)) found)
            (while (and (not found) pos (< pos (point-max)))
              (when-let ((buf (get-text-property pos 'associated-dired-buffer)))
                (setq dired-buf buf)
                (setq found t))
              (setq pos (next-single-property-change
                         pos 'associated-dired-buffer nil (point-max)))))))

      ;; If still not found, look for any live dired buffer that might be relevant
      (unless dired-buf
        (let ((buffers (buffer-list)))
          (while (and (not dired-buf) buffers)
            (let ((buf (pop buffers)))
              (with-current-buffer buf
                (when (and (derived-mode-p 'dired-mode)
                           (boundp 'dired-image-thumbnail--source-dir)
                           (equal default-directory (buffer-local-value 'default-directory (current-buffer))))
                  (setq dired-buf buf)))))))

      ;; Jump between property-change boundaries (O(thumbnails)) rather
      ;; than scanning one character at a time.
      (save-excursion
        (goto-char (point-min))
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (when-let ((file (get-text-property pos 'original-file-name)))
              (when (dired-image-thumbnail--image-p file)
                (push file images))
              (unless source-dir
                (setq source-dir (file-name-directory file))))
            (setq pos (next-single-property-change
                       pos 'original-file-name nil (point-max))))))

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

(defun dired-image-thumbnail--format-directory (file)
  "Return an abbreviated directory location for the header line.
Uses the directory containing FILE when known, otherwise falls
back to the buffer's source directory.  Returns an empty string
when neither is available."
  (abbreviate-file-name
   (or (and file (not (string-empty-p file))
            (file-name-directory (expand-file-name file)))
       dired-image-thumbnail--source-dir
       "")))

(defun dired-image-thumbnail--format-properties-string (orig-fun buf file image-count props comment)
  "Advice around `image-dired-format-properties-string' for the header line.
ORIG-FUN is the original function.  BUF, FILE, IMAGE-COUNT, PROPS, and
COMMENT are passed to the original function.  When
`dired-image-thumbnail--all-images' is set, return our enhanced header
line.  Otherwise, fall back to the original function."
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
                               (propertize (format " [%d marked]" marked-count)
                                           'face 'dired-image-thumbnail-header-info)
                             ""))
             (rel-name (dired-image-thumbnail--relative-name file))
             (dir (dired-image-thumbnail--format-directory file))
             (size (dired-image-thumbnail--format-file-size file))
             (dimensions (dired-image-thumbnail--format-image-dimensions file))
             (quality (symbol-name dired-image-thumbnail-display-quality))
             (layout-info (if dired-image-thumbnail-square-thumbnails
                              " [square]" " [natural]")))
        (concat
         "  "
         (propertize dir 'face 'dired-image-thumbnail-header-info)
         "  "
         (propertize rel-name 'face 'dired-image-thumbnail-header-info)
         "  "
         (propertize image-count 'face 'dired-image-thumbnail-header-info)
         "  "
         (propertize size 'face 'dired-image-thumbnail-header-info)
         "  "
         (propertize dimensions 'face 'dired-image-thumbnail-header-info)
         "  "
         (propertize sort-info 'face 'dired-image-thumbnail-header-info)
         "  "
         (propertize (format "[%s]" quality) 'face 'dired-image-thumbnail-header-info)
         (propertize layout-info 'face 'dired-image-thumbnail-header-info)
         (if (string-empty-p filter-info)
             ""
           (propertize (format "  %s" filter-info)
                       'face 'dired-image-thumbnail-header-info))
         marked-info))
    ;; Fall back to original function
    (funcall orig-fun buf file image-count props comment)))

(defun dired-image-thumbnail--display-image-no-lock (orig-fun &rest args)
  "Advice around `image-dired-display-image' to suppress lock file creation.
ORIG-FUN is the original function; ARGS are its arguments.  When
`dired-image-thumbnail-suppress-display-lockfiles' is non-nil, bind
`create-lockfiles' to nil so that visiting the image file does not
create a `.#filename' lock symlink.  Image files are displayed
read-only, so locking serves no purpose and stale lock files can
accumulate as residue in image directories."
  (if dired-image-thumbnail-suppress-display-lockfiles
      (let ((create-lockfiles nil))
        (apply orig-fun args))
    (apply orig-fun args)))

;;; Display functions

;; Thumbnail generation in image-dired is asynchronous: jobs are queued
;; and run by subprocesses, so a freshly queued thumbnail file may not
;; exist yet when we want to crop it.
(defvar image-dired-queue)
(defvar image-dired-queue-active-jobs)

(defun dired-image-thumbnail--wait-for-thumbnails (&optional seconds)
  "Wait for image-dired's asynchronous thumbnail queue to drain.
image-dired generates thumbnails asynchronously (jobs are queued and
run by subprocesses), so a freshly queued thumbnail file may not
exist yet.  Wait, keeping Emacs responsive, until `image-dired-queue'
is empty and `image-dired-queue-active-jobs' is zero, or until
SECONDS (default 120) elapse.  Returns non-nil if the queue drained."
  (let ((waited 0)
        (limit (or seconds 120)))
    (while (and (or image-dired-queue (> image-dired-queue-active-jobs 0))
                (< waited limit))
      (sit-for 0.05)
      (setq waited (+ waited 0.05)))
    (and (null image-dired-queue) (= image-dired-queue-active-jobs 0))))

(defun dired-image-thumbnail--crop-thumb-to-square (thumb-file)
  "Crop THUMB-FILE in place to a uniform square using ImageMagick.
The target size is `image-dired--thumb-size' (or
`image-dired-thumb-size', falling back to the display size or 128
if either is nil).  Uses -thumbnail with
the `^' flag to scale-to-fill, then center-crops to an exact square,
so every thumbnail has the same dimensions for a tidy grid.
If the file is already the target size, missing, or ImageMagick is
unavailable, do nothing."
  (when (file-exists-p thumb-file)
    (let ((target (or (and (fboundp 'image-dired--thumb-size)
                           (image-dired--thumb-size))
                      (and (numberp dired-image-thumbnail--display-size)
                           dired-image-thumbnail--display-size)
                      (and (numberp image-dired-thumb-size)
                           image-dired-thumb-size)
                      128))
          (size (ignore-errors (image-size (create-image thumb-file) t))))
      (when (and size (> (car size) 0) (> (cdr size) 0)
                 (not (and (= (car size) target)
                           (= (cdr size) target))))
        (let ((mogrify-cmd (cond
                             ((executable-find "mogrify") "mogrify")
                             ((executable-find "magick") "magick")
                             (t nil))))
          (when mogrify-cmd
            (let* ((thumb-spec (format "%dx%d^" target target))
                   (extent-spec (format "%dx%d" target target))
                   (args (if (string= mogrify-cmd "magick")
                             (list "mogrify" "-thumbnail" thumb-spec
                                   "-gravity" "center"
                                   "-extent" extent-spec thumb-file)
                           (list "-thumbnail" thumb-spec
                                 "-gravity" "center"
                                 "-extent" extent-spec thumb-file))))
              (apply #'call-process mogrify-cmd nil nil nil args))))))))

(defvar-local dired-image-thumbnail--current-overlay nil
  "Overlay highlighting the currently selected thumbnail.")

(defun dired-image-thumbnail--outline-box ()
  "Return the :box specification for the current-thumbnail outline.
The line width comes from the :box attribute of
`dired-image-thumbnail-current-thumbnail' (default 3), so
customising the face adjusts the thickness.  The colour is the
explicit :color of that :box when set, so customising the face
still works; otherwise the background colour of the `highlight'
face is used, so the outline matches the active theme -- the
theme's own highlight, just thicker.  Falls back to orange on
light backgrounds and yellow on dark ones when no usable colour
is available."
  (let* ((box (face-attribute 'dired-image-thumbnail-current-thumbnail :box))
         (width (let ((w (and (consp box) (plist-get box :line-width))))
                  (cond ((numberp w) (abs w))
                        ((consp w) w)
                        (t 5))))
         (explicit (cond ((stringp box) box)
                         ((consp box) (plist-get box :color))))
         (color (or (and (stringp explicit) explicit)
                    (let ((bg (ignore-errors
                                (face-attribute 'highlight :background nil t))))
                      (cond ((stringp bg) bg)
                            ((eq (frame-parameter nil 'background-mode) 'dark)
                             "yellow")
                            (t "dark orange"))))))
    (list :line-width width :color color)))

(defun dired-image-thumbnail--update-current-highlight ()
  "Outline the thumbnail at point with `dired-image-thumbnail-current-thumbnail'.
The highlight is an overlay on the character carrying the image, so it
is redrawn as point moves.  The outline colour follows the active
theme (see `dired-image-thumbnail--outline-box').  Installed on
`post-command-hook' in thumbnail buffers and called directly after
a refresh."
  (when (derived-mode-p 'image-dired-thumbnail-mode)
    ;; Drop the previous overlay.  This also clears a stale overlay left
    ;; behind by `erase-buffer' during a refresh.
    (when (overlayp dired-image-thumbnail--current-overlay)
      (delete-overlay dired-image-thumbnail--current-overlay)
      (setq dired-image-thumbnail--current-overlay nil))
    (when (and dired-image-thumbnail-highlight-current-thumbnail
               (get-text-property (point) 'original-file-name)
               (< (point) (point-max)))
      (let ((ov (make-overlay (point) (1+ (point)) nil t)))
        (overlay-put ov 'face
                     (list 'dired-image-thumbnail-current-thumbnail
                           (list :box (dired-image-thumbnail--outline-box))))
        (setq dired-image-thumbnail--current-overlay ov)))))

(defun dired-image-thumbnail--cursor-remap ()
  "Return a `face-remapping-alist' entry colouring the cursor like the outline.
Uses the colour resolved by `dired-image-thumbnail--outline-box',
so the cursor always matches the current-thumbnail highlight.
Returns nil if no colour is available."
  (let ((color (plist-get (dired-image-thumbnail--outline-box) :color)))
    (when (stringp color)
      `(cursor . (:background ,color)))))

(defun dired-image-thumbnail--setup-cursor ()
  "Recolour the cursor, buffer-locally, to match the current-thumbnail highlight."
  (let ((remap (and dired-image-thumbnail-highlight-cursor
                    (dired-image-thumbnail--cursor-remap))))
    (setq-local face-remapping-alist
                (if remap
                    (cons remap (assq-delete-all 'cursor face-remapping-alist))
                  (assq-delete-all 'cursor face-remapping-alist)))))

(defun dired-image-thumbnail-refresh (&optional preferred-target)
  "Refresh the thumbnail display with current images.
If PREFERRED-TARGET is provided, attempt to move point to that file
after refreshing. Otherwise, try to maintain position on the current file."
  (interactive)
  ;; Initialize if not already done.  FORCE a scan if all-images is nil.
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--re-scan-internal))
  (when dired-image-thumbnail--all-images
    (let ((current-file (or preferred-target (image-dired-original-file-name)))
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
      ;; Ensure valid thumbnail geometry variables: these can be nil if
      ;; they were bound to nil before image-dired loaded (the defcustom
      ;; does not repair a non-void nil).  image-dired's own line-up
      ;; functions use them in arithmetic unguarded (e.g.
      ;; (* 2 image-dired-thumb-relief) in image-dired-line-up-dynamic),
      ;; which signals `wrong-type-argument' -- so repair them
      ;; buffer-locally before any thumbnail work.
      (unless (numberp image-dired-thumb-size)
        (setq-local image-dired-thumb-size
                    (or (and (fboundp 'image-dired--thumb-size)
                             (image-dired--thumb-size))
                        128)))
      (unless (numberp image-dired-thumb-relief)
        (setq-local image-dired-thumb-relief 2))
      (unless (numberp image-dired-thumb-margin)
        (setq-local image-dired-thumb-margin 2))
      (unless (numberp image-dired-thumbs-per-row)
        (setq-local image-dired-thumbs-per-row 3))
      ;; Temporarily override thumb size if needed
      (let ((standard-size image-dired-thumb-size))
        (when (and display-size (numberp standard-size)
                   (/= display-size standard-size))
          (setq-local image-dired-thumb-size display-size))
        ;; Insert thumbnails using image-dired's standard function
        ;; This ensures proper marking support
        (when dired-image-thumbnail-square-thumbnails
          (clear-image-cache))
        ;; Pre-count thumbnails that need work (creation and cropping
        ;; counted as separate items so the total matches the number of
        ;; progress updates exactly) and show a progress bar only when
        ;; there is actual work.  An explicit running counter is passed
        ;; to `progress-reporter-update': relying on the nil-increment
        ;; behaviour breaks in Emacs 30 once the value reaches
        ;; max-value (nil is then passed through to
        ;; `progress-reporter-do-update', signalling
        ;; wrong-type-argument).
         (let* ((work-needed 0)
                (work-done 0)
                (queued 0)
                (progress nil))
           (dolist (file dired-image-thumbnail--current-images)
             (let ((thumb-file (image-dired-thumb-name file)))
               (when (not (file-exists-p thumb-file))
                 (setq work-needed (1+ work-needed)))
               (when dired-image-thumbnail-square-thumbnails
                 (setq work-needed (1+ work-needed)))))
           (when (> work-needed 0)
             (setq progress (make-progress-reporter
                             (format "Generating %d thumbnail%s..."
                                     work-needed
                                     (if (= work-needed 1) "" "s"))
                             0 work-needed)))
           ;; Phase 1: queue generation for missing thumbnails.
           (dolist (file dired-image-thumbnail--current-images)
             (let ((thumb-file (image-dired-thumb-name file)))
               (unless (file-exists-p thumb-file)
                 (image-dired-create-thumb file thumb-file)
                 (setq queued (1+ queued))
                 (setq work-done (1+ work-done))
                 (when progress (progress-reporter-update progress work-done)))))
           ;; Phase 2: image-dired generates thumbnails asynchronously,
           ;; so wait for the queue to drain before cropping.  Without
           ;; this a freshly queued thumbnail does not exist yet and the
           ;; crop is silently skipped, leaving non-square thumbnails on
           ;; a first run with an empty cache.
           (when (and dired-image-thumbnail-square-thumbnails (> queued 0))
             (dired-image-thumbnail--wait-for-thumbnails))
           ;; Phase 3: crop to uniform squares (when enabled).
           (when dired-image-thumbnail-square-thumbnails
             (dolist (file dired-image-thumbnail--current-images)
               (dired-image-thumbnail--crop-thumb-to-square
                (image-dired-thumb-name file))
               (setq work-done (1+ work-done))
               (when progress (progress-reporter-update progress work-done))))
           ;; Phase 4: insert with all three required arguments.
           (dolist (file dired-image-thumbnail--current-images)
             (image-dired-insert-thumbnail
              (image-dired-thumb-name file) file dired-buf))
           (when progress (progress-reporter-done progress))))
      ;; Line up
      (if dired-image-thumbnail-wrap-display
          (progn
            (setq-local word-wrap t)
            (setq-local truncate-lines nil))
        (image-dired--line-up-with-method))
      ;; Remember the thumbnail window width at line-up time so that a
      ;; later display can detect a layout change and re-align.
      (setq dired-image-thumbnail--lineup-width
            (when-let ((win (get-buffer-window nil t)))
              (window-body-width win)))
      ;; Restore mark display
      (image-dired--thumb-update-marks)
      ;; Restore position before updating header line, so point is on a
      ;; valid thumbnail when the header line reads the file at point.
      (if current-file
          ;; Jump between property-change boundaries (O(thumbnails)) rather
          ;; than scanning one character at a time.
          (let ((pos (point-min))
                (found nil))
            (while (and (not found) pos (< pos (point-max)))
              (if (equal (get-text-property pos 'original-file-name) current-file)
                  (setq found pos)
                (setq pos (next-single-property-change
                           pos 'original-file-name nil (point-max)))))
            (goto-char (or found (point-min))))
         (goto-char (point-min)))
       (image-dired--update-header-line)
       ;; Re-apply the current-thumbnail highlight: `erase-buffer' above
       ;; removed the previous overlay.
       (dired-image-thumbnail--update-current-highlight))))

(defun dired-image-thumbnail-hard-refresh ()
  "Refresh thumbnails by clearing the cache and reloading.
This deletes the contents of `image-dired-dir' and then calls
`dired-image-thumbnail-refresh'."
  (interactive)
  (unless (file-directory-p image-dired-dir)
    (make-directory image-dired-dir t))
  (when (or dired-image-thumbnail-auto-accept
            (yes-or-no-p (format "Deep refresh: Clear all thumbnails in %s? " image-dired-dir)))
    (message "Clearing thumbnail cache...")
    ;; Delete all files in image-dired-dir
    (let ((files (directory-files image-dired-dir t directory-files-no-dot-files-regexp)))
      (dolist (file files)
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file))))
    (dired-image-thumbnail-refresh)
    (message "Thumbnail cache cleared and buffer refreshed.")))

(defun dired-image-thumbnail-invalidate-dimensions ()
  "Clear the dimension cache and re-query dimensions for all visible images.
Useful after an external tool has resized images on disk."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'image-dired-thumbnail-mode)
                 (bound-and-true-p dired-image-thumbnail--dimension-cache))
        (clrhash dired-image-thumbnail--dimension-cache)
        (clrhash dired-image-thumbnail--dimension-pending)
        (when dired-image-thumbnail--current-images
          (dolist (file dired-image-thumbnail--current-images)
            (dired-image-thumbnail--get-image-dimensions file)))
        (image-dired--update-header-line)))))
(defun dired-image-thumbnail-invalidate-files (files)
  "Invalidate dimension cache for the specific list of FILES.
FILES should be a list of expanded file names."
  (let ((files (mapcar #'expand-file-name files)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'image-dired-thumbnail-mode)
                   (bound-and-true-p dired-image-thumbnail--dimension-cache))
          (dolist (f files)
            (remhash f dired-image-thumbnail--dimension-cache)
            (remhash f dired-image-thumbnail--dimension-pending))
          ;; Only re-query if the files are actually in this buffer
          (dolist (f files)
            (when (member f dired-image-thumbnail--current-images)
              (dired-image-thumbnail--get-image-dimensions f)))
          (image-dired--update-header-line))))))

(defun dired-image-thumbnail-refresh-current-display ()
  "Refresh the full-size image display if it's active.
Updates the `image-dired-display-image-buffer' based on the image at point
in the thumbnail buffer."
  (let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
    (when (and thumb-buf (buffer-live-p thumb-buf))
      (with-current-buffer thumb-buf
        (when (derived-mode-p 'image-dired-thumbnail-mode)
          (dired-image-thumbnail--display-this))))))

(defun dired-image-thumbnail-hide-display ()
  "Hide the full-size image display window and kill its buffer.
Killing the buffer (rather than burying it) releases the image data
from memory and removes any lock file on the visited image."
  (interactive)
  (let ((buf (get-buffer image-dired-display-image-buffer)))
    (when (and buf (buffer-live-p buf))
      (let ((win (get-buffer-window buf)))
        (when win
          (delete-window win))
        (kill-buffer buf)))))

(defun dired-image-thumbnail-re-scan (&optional preferred-target)
  "Re-scan disk for images in the current thumbnail buffer and refresh.
Useful after files have been renamed or added externally.
If PREFERRED-TARGET is provided, move point there after refresh."
  (interactive)
  (message "Dired-Image-Thumbnail: Re-scanning buffer %s..." (buffer-name))
  (when (derived-mode-p 'image-dired-thumbnail-mode)
    (dired-image-thumbnail--re-scan-internal)
    (dired-image-thumbnail-refresh preferred-target)))

(defun dired-image-thumbnail--re-scan-internal ()
  "Internal function to re-populate `all-images' from disk."
  (when-let ((source-dir (and (boundp 'dired-image-thumbnail--source-dir)
                                dired-image-thumbnail--source-dir)))
    (setq dired-image-thumbnail--all-images
          (dired-image-thumbnail--find-images
           source-dir
           (and (boundp 'dired-image-thumbnail--recursive)
                dired-image-thumbnail--recursive)))))

(defun dired-image-thumbnail-refresh-all (&optional rename-alist)
  "Re-scan and refresh all `dired-image-thumbnail' buffers.
If RENAME-ALIST is provided, it should be an alist mapping old
filenames to new filenames.  Each buffer will attempt to maintain
its point position if the file at point was renamed."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'image-dired-thumbnail-mode)
        (let* ((raw-file (image-dired-original-file-name))
               (old-file (and raw-file (expand-file-name raw-file)))
               (new-file (and old-file (cdr (assoc old-file rename-alist)))))
          (dired-image-thumbnail-re-scan new-file))))))

(defun dired-image-thumbnail-sort-by-dired ()
  "Sort thumbnails by Dired buffer order."
  (interactive)
  (setq dired-image-thumbnail--sort-by 'dired)
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sorted by Dired order"))

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
  "Select sort criteria with `completing-read'.
Choose dired order, name, date or size, or reverse the current
sort order."
  (interactive)
  (let ((choice (completing-read "Sort by: "
                                 '("dired" "name" "date" "size" "reverse")
                                 nil t)))
    (pcase choice
      ("dired" (dired-image-thumbnail-sort-by-dired))
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
     ((string-match "\\`\\([0-9.]+\\)g\\'" str)
      (* (string-to-number (match-string 1 str)) 1073741824))
     ((string-match "\\`\\([0-9.]+\\)m\\'" str)
      (* (string-to-number (match-string 1 str)) 1048576))
     ((string-match "\\`\\([0-9.]+\\)k\\'" str)
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
  "Select filter criteria with `completing-read'.
Choose filtering by name or size range, or clear all filters."
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
    (if (> dired-image-thumbnail--display-size image-dired-thumb-size)
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

(defun dired-image-thumbnail--current-images-set ()
  "Return a hash set of expanded names of all current (visible) images."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (file dired-image-thumbnail--current-images)
      (puthash (expand-file-name file) t set))
    set))

(defun dired-image-thumbnail-mark ()
  "Mark the current thumbnail and, with follow on, show the next one.
The follow display goes through the fast preview pipeline.  Vanilla
`image-dired-marking-shows-next' is disabled because its own
advance display decodes the full original image, which made
marking feel slow."
  (interactive)
  (image-dired-mark-thumb-original-file)
  (dired-image-thumbnail--mark-follow))

(defun dired-image-thumbnail-unmark ()
  "Unmark the current thumbnail and, with follow on, show the next one.
See `dired-image-thumbnail-mark'."
  (interactive)
  (image-dired-unmark-thumb-original-file)
  (dired-image-thumbnail--mark-follow))

(defun dired-image-thumbnail--mark-follow ()
  "Repaint the mark immediately, then follow.
Vanilla marking always advances to the next image; when
`dired-image-thumbnail-auto-display-on-navigate' is non-nil the
newly-current image is displayed through the fast preview
pipeline."
  (sit-for 0)
  (when dired-image-thumbnail-auto-display-on-navigate
    (dired-image-thumbnail--display-this)))

(defun dired-image-thumbnail-mark-all ()
  "Mark all visible images in the thumbnail buffer."
  (interactive)
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (when (and dired-image-thumbnail--current-images
             dired-image-thumbnail--dired-buffer
             (buffer-live-p dired-image-thumbnail--dired-buffer))
    ;; Single pass over the dired buffer (O(n) rather than O(n^2)).
    (let ((targets (dired-image-thumbnail--current-images-set)))
      (with-current-buffer dired-image-thumbnail--dired-buffer
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((file (dired-get-filename nil t)))
              (if (and file (gethash (expand-file-name file) targets))
                  ;; `dired-mark' marks this line and advances one line.
                  (dired-mark 1)
                (forward-line 1)))))))
    ;; Update all thumbnail marks using image-dired's function
    (image-dired--thumb-update-marks)
    (message "Marked all %d images" (length dired-image-thumbnail--current-images))))

(defun dired-image-thumbnail-toggle-all-marks ()
  "Toggle mark on all visible images."
  (interactive)
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (when (and dired-image-thumbnail--current-images
             dired-image-thumbnail--dired-buffer
             (buffer-live-p dired-image-thumbnail--dired-buffer))
    ;; Single pass over the dired buffer (O(n) rather than O(n^2)).
    (let ((targets (dired-image-thumbnail--current-images-set)))
      (with-current-buffer dired-image-thumbnail--dired-buffer
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((file (dired-get-filename nil t)))
              (cond
               ((not (and file (gethash (expand-file-name file) targets)))
                (forward-line 1))
               ((image-dired-dired-file-marked-p)
                (dired-unmark 1))
               (t
                (dired-mark 1)))))))))
  ;; Update all thumbnail marks using image-dired's function
  (image-dired--thumb-update-marks)
  (message "%d images now marked" (dired-image-thumbnail--count-marked)))

;;; File operations

(defun dired-image-thumbnail-move (target-dir)
  "Move the marked images, or the image at point, into TARGET-DIR.
When images are marked in the associated Dired buffer those are moved,
otherwise the image at point is.  TARGET-DIR is created if it does not
exist (confirming first, see `dired-image-thumbnail-auto-accept').
Files already present at the destination are overwritten only after
confirmation.  The thumbnail display and the associated Dired buffer
are refreshed afterwards."
  (interactive
   (list (read-directory-name "Move to directory: "
                              (or dired-image-thumbnail--source-dir
                                  default-directory))))
  (let* ((files (dired-image-thumbnail-get-marked))
         (target (file-name-as-directory (expand-file-name target-dir))))
    (unless files
      (user-error "No images to move"))
    (unless (file-directory-p target)
      (if (or dired-image-thumbnail-auto-accept
              (yes-or-no-p (format "Create directory %s? " target)))
          (make-directory target t)
        (user-error "Aborted")))
    (let ((moved 0))
      (dolist (file files)
        (let ((dest (expand-file-name (file-name-nondirectory file) target)))
          (when (or (not (file-exists-p dest))
                    dired-image-thumbnail-auto-accept
                    (yes-or-no-p (format "%s exists; overwrite? " dest)))
            (rename-file file dest t)
            (setq moved (1+ moved))
            (setq dired-image-thumbnail--current-images
                  (remove file dired-image-thumbnail--current-images))
            (setq dired-image-thumbnail--all-images
                  (remove file dired-image-thumbnail--all-images)))))
      ;; Refresh dired buffer
      (when (and dired-image-thumbnail--dired-buffer
                 (buffer-live-p dired-image-thumbnail--dired-buffer))
        (with-current-buffer dired-image-thumbnail--dired-buffer
          (revert-buffer)))
      (dired-image-thumbnail-refresh)
      (message "Moved %d image%s to %s"
               moved (if (= moved 1) "" "s") target))))

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
                  (let ((marked-set (dired-image-thumbnail--get-dired-marked-set)))
                    (seq-filter (lambda (file) (gethash file marked-set))
                                dired-image-thumbnail--current-images)))))
    (or marked
        (when-let ((file (dired-image-thumbnail--nearest-image-original-file-name)))
          (list file)))))

(defun dired-image-thumbnail-delete-marked ()
  "Delete marked images (or image at point if none marked)."
  (interactive)
  (let ((files (dired-image-thumbnail-get-marked)))
    (unless files
      (user-error "No images to delete"))
    (when (or dired-image-thumbnail-auto-accept
              (yes-or-no-p (format "Delete %d image(s)? " (length files))))
      (dolist (file files)
        (delete-file file t)
        (setq dired-image-thumbnail--current-images
              (remove file dired-image-thumbnail--current-images))
        (setq dired-image-thumbnail--all-images
              (remove file dired-image-thumbnail--all-images)))
      ;; Refresh dired buffer
      (when (and dired-image-thumbnail--dired-buffer
                 (buffer-live-p dired-image-thumbnail--dired-buffer))
        (with-current-buffer dired-image-thumbnail--dired-buffer
          (revert-buffer)))
      (dired-image-thumbnail-refresh)
      (message "Deleted %d image(s)" (length files)))))

(defun dired-image-thumbnail-open-external ()
  "Open the image at point in an external editor.
Uses `dired-image-thumbnail-external-editor' if set, otherwise
the system default application."
  (interactive)
  (if-let ((file (dired-image-thumbnail--nearest-image-original-file-name)))
      (let ((program dired-image-thumbnail-external-editor)
            (expanded (expand-file-name file)))
        (if program
            (start-process "dit-external" nil program expanded)
          (cond
           ((eq system-type 'gnu/linux)
            (start-process "dit-external" nil "xdg-open" expanded))
           ((eq system-type 'darwin)
            (start-process "dit-external" nil "open" expanded))
           ((memq system-type '(windows-nt cygwin ms-dos))
            (w32-shell-execute "open" expanded))
           (t (start-process "dit-external" nil "xdg-open" expanded))))
        (message "Opened %s externally" (file-name-nondirectory file)))
    (message "No image at point")))

(defun dired-image-thumbnail-delete ()
  "Delete the image at or near point."
  (interactive)
  (if-let ((file (dired-image-thumbnail--nearest-image-original-file-name)))
      (when (or dired-image-thumbnail-auto-accept
                (yes-or-no-p (format "Delete %s? " (file-name-nondirectory file))))
        ;; Find the next image to move to after deletion
        (let ((index (cl-position file dired-image-thumbnail--current-images :test #'equal)))
          (delete-file file t)
          (setq dired-image-thumbnail--current-images
                (remove file dired-image-thumbnail--current-images))
          (setq dired-image-thumbnail--all-images
                (remove file dired-image-thumbnail--all-images))
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

(defun dired-image-thumbnail-toggle-square-thumbnails ()
  "Toggle square thumbnail cropping.
When enabled, thumbnails are center-cropped to squares for a tidier grid."
  (interactive)
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (setq dired-image-thumbnail-square-thumbnails
        (not dired-image-thumbnail-square-thumbnails))
  (dired-image-thumbnail-refresh)
  (message "Square thumbnails: %s"
            (if dired-image-thumbnail-square-thumbnails "ON" "OFF")))

;;; Main entry point

(defun dired-image-thumbnail--find-subdirs (directory &optional max-depth)
  "Return a list of all subdirectories under DIRECTORY.
Does not include DIRECTORY itself.  Optional MAX-DEPTH limits recursion
\(nil means unlimited, 1 means direct children only)."
  (let ((subdirs nil)
        (dirs-to-process (list (cons directory 0))))
    (while dirs-to-process
      (let* ((item (pop dirs-to-process))
             (current-dir (car item))
             (current-depth (cdr item)))
        ;; The "^[^.]" match already excludes "." and ".." (and other dotfiles).
        (dolist (file (directory-files current-dir t "^[^.]" t))
          (when (file-directory-p file)
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
  (cl-some (lambda (file)
             (and (not (file-directory-p file))
                  (dired-image-thumbnail--image-p file)))
           (directory-files directory t "^[^.]" t)))

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

This works with inserted subdirectories - use \\`i' (`dired-maybe-insert-subdir')
to insert subdirectories before calling this command to include images from
those subdirectories. See `dired-image-thumbnail-insert-subdir-recursive'
for a helper to insert all subdirectories at once.

This function calls vanilla `image-dired' which triggers our hooks for
enhanced features like sorting and filtering."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (let ((dired-buf (current-buffer))
        (source-dir default-directory)
        ;; More than one entry in `dired-subdir-alist' means subdirectories
        ;; have been inserted, so images may live below `source-dir'.
        (recursive (> (length dired-subdir-alist) 1)))
    ;; Store state for our hooks to use
    (setq-local dired-image-thumbnail--source-dir source-dir)
    (setq-local dired-image-thumbnail--dired-buffer dired-buf)
    
    ;; Call vanilla image-dired which will trigger our hooks and enhancements
    (call-interactively 'image-dired)
    
    ;; Now refresh the thumbnail buffer with our enhancements
    ;; We need to find the thumbnail buffer first
    (when-let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
      (with-current-buffer thumb-buf
        ;; Reset state so initialization re-scans from the new dired buffer
        (setq dired-image-thumbnail--all-images nil)
        (setq dired-image-thumbnail--current-images nil)
        (setq dired-image-thumbnail--dired-buffer dired-buf)
        (setq dired-image-thumbnail--source-dir source-dir)
        (setq dired-image-thumbnail--recursive recursive)
        (setq dired-image-thumbnail--filter-name nil)
        (setq dired-image-thumbnail--filter-size-min nil)
        (setq dired-image-thumbnail--filter-size-max nil))
      ;; Refresh display-buffer rules so they track the current layout/ratio
      ;; custom values, then apply the layout BEFORE refresh so that line-up
      ;; sees the correct (narrower) window width for column calculation.
      (dired-image-thumbnail-setup-display-buffer)
      (dired-image-thumbnail--apply-layout)
      (with-current-buffer thumb-buf
        (dired-image-thumbnail-refresh)
        (goto-char (point-min))
        ;; Show an initial preview of the first image when follow
        ;; (auto-display) is enabled, so stepping through starts at once.
        (when dired-image-thumbnail-auto-display-on-navigate
          (dired-image-thumbnail--display-this))))))

(defun dired-image-thumbnail--subdir-target-buffer ()
  "Return the dired buffer that subdirectory commands should act on.
That is the current buffer when in `dired-mode', otherwise the
associated dired buffer of the current thumbnail buffer.  Signals a
user-error when neither is available."
  (cond ((derived-mode-p 'dired-mode)
         (current-buffer))
        ((and (derived-mode-p 'image-dired-thumbnail-mode)
              (buffer-live-p dired-image-thumbnail--dired-buffer))
         dired-image-thumbnail--dired-buffer)
        (t (user-error "Not in a dired buffer"))))

;;;###autoload
(defun dired-image-thumbnail-insert-subdir-recursive (&optional max-depth)
  "Insert all subdirectories recursively into the current dired buffer.
Optional MAX-DEPTH limits recursion depth (nil means unlimited).
This makes images in subdirectories visible to `dired-image-thumbnail'.
When run from a thumbnail buffer, the associated dired buffer is used.

Note: This can be slow for directories with many subdirectories.
Consider using `dired-image-thumbnail-insert-image-subdirs' instead,
which only inserts subdirectories that contain images."
  (interactive "P")
  (with-current-buffer (dired-image-thumbnail--subdir-target-buffer)
    (let* ((depth (if max-depth (prefix-numeric-value max-depth) nil))
           (subdirs (dired-image-thumbnail--find-subdirs default-directory depth)))
      (if subdirs
          (progn
            (message "Inserting %d subdirectories..." (length subdirs))
            (dired-image-thumbnail--insert-subdirs subdirs)
            (message "Inserted %d subdirectories" (length subdirs)))
        (message "No subdirectories found")))))

;;;###autoload
(defun dired-image-thumbnail-insert-image-subdirs (&optional max-depth)
  "Insert only subdirectories that contain image files.
Optional MAX-DEPTH limits recursion depth (nil means unlimited).
This is more efficient than `dired-image-thumbnail-insert-subdir-recursive'
for directories with many non-image subdirectories.
When run from a thumbnail buffer, the associated dired buffer is used."
  (interactive "P")
  (with-current-buffer (dired-image-thumbnail--subdir-target-buffer)
    (let* ((depth (if max-depth (prefix-numeric-value max-depth) nil))
           (subdirs (dired-image-thumbnail--find-image-subdirs default-directory depth)))
      (if subdirs
          (progn
            (message "Inserting %d subdirectories with images..." (length subdirs))
            (dired-image-thumbnail--insert-subdirs subdirs)
            (message "Inserted %d subdirectories" (length subdirs)))
        (message "No subdirectories with images found")))))

;;;###autoload
(defun dired-image-thumbnail-kill-all-subdirs ()
  "Remove all inserted subdirectories from the current dired buffer.
This returns the view to just the top-level directory.
When run from a thumbnail buffer, the associated dired buffer is used."
  (interactive)
  (with-current-buffer (dired-image-thumbnail--subdir-target-buffer)
    (let ((count 0))
      (save-excursion
        (goto-char (point-max))
        ;; Work backwards to avoid position issues
        (while (dired-get-subdir)
          (dired-kill-subdir)
          (setq count (1+ count))))
      (if (> count 0)
          (message "Removed %d subdirectories" count)
        (message "No subdirectories to remove")))))

;;; Keymaps

;;;###autoload
(defun dired-image-thumbnail-setup-keys ()
  "Add dired-image-thumbnail keybindings to `image-dired-thumbnail-mode-map'.
If `image-dired-thumbnail-mode-map' is not a valid keymap (e.g. on
some Emacs builds where `image-dired' does not define it at load
time), a warning is displayed and no keybindings are installed."
  (if (not (keymapp image-dired-thumbnail-mode-map))
      (display-warning
       '(dired-image-thumbnail setup-keys)
       "`image-dired-thumbnail-mode-map' is not a keymap; \
keybindings will not be installed.  This can happen when `image-dired'\
 is not fully loaded.  Try (require 'image-dired) before loading\
 `dired-image-thumbnail'.")
    (define-key image-dired-thumbnail-mode-map (kbd "s") #'dired-image-thumbnail-sort)
    (define-key image-dired-thumbnail-mode-map (kbd "S") #'dired-image-thumbnail-sort)
    (define-key image-dired-thumbnail-mode-map (kbd "/") #'dired-image-thumbnail-filter)
    (define-key image-dired-thumbnail-mode-map (kbd "\\") #'dired-image-thumbnail-filter)
    (define-key image-dired-thumbnail-mode-map (kbd "#") #'dired-image-thumbnail-toggle-square-thumbnails)
    (define-key image-dired-thumbnail-mode-map (kbd "g") #'dired-image-thumbnail-refresh)
    (define-key image-dired-thumbnail-mode-map (kbd "G") #'dired-image-thumbnail-hard-refresh)
    (define-key image-dired-thumbnail-mode-map (kbd "+") #'dired-image-thumbnail-increase-size)
    (define-key image-dired-thumbnail-mode-map (kbd "-") #'dired-image-thumbnail-decrease-size)
    ;; Marking
    (define-key image-dired-thumbnail-mode-map (kbd "m") #'dired-image-thumbnail-mark)
    (define-key image-dired-thumbnail-mode-map (kbd "u") #'dired-image-thumbnail-unmark)
    (define-key image-dired-thumbnail-mode-map (kbd "M") #'dired-image-thumbnail-mark-all)
    (define-key image-dired-thumbnail-mode-map (kbd "t") #'dired-image-thumbnail-toggle-all-marks)
    ;; File operations
    (define-key image-dired-thumbnail-mode-map (kbd "v") #'dired-image-thumbnail-move)
    (define-key image-dired-thumbnail-mode-map (kbd "d") #'dired-image-thumbnail-goto-dired)
    (define-key image-dired-thumbnail-mode-map (kbd "D") #'dired-image-thumbnail-delete)
    (define-key image-dired-thumbnail-mode-map (kbd "C-d") #'dired-image-thumbnail-delete-and-next)
    (define-key image-dired-thumbnail-mode-map (kbd "x") #'dired-image-thumbnail-delete-marked)
    ;; Enhanced navigation (auto-display checked at runtime)
    (define-key image-dired-thumbnail-mode-map (kbd "n") #'dired-image-thumbnail-next-image)
    (define-key image-dired-thumbnail-mode-map (kbd "p") #'dired-image-thumbnail-previous-image)
    ;; vi-style navigation aliases
    (define-key image-dired-thumbnail-mode-map (kbd "f") #'dired-image-thumbnail-next-image)
    (define-key image-dired-thumbnail-mode-map (kbd "b") #'dired-image-thumbnail-previous-image)
    ;; Auto-display toggle (a)
    (define-key image-dired-thumbnail-mode-map (kbd "F") #'dired-image-thumbnail-toggle-auto-display)
    ;; Display quality
    (define-key image-dired-thumbnail-mode-map (kbd "Q") #'dired-image-thumbnail-select-display-quality)
    ;; External
    (define-key image-dired-thumbnail-mode-map (kbd "W") #'dired-image-thumbnail-open-external)
    ;; Subdirectories (insertion is normally done from dired, e.g. C-t z)
    (define-key image-dired-thumbnail-mode-map (kbd "z") #'dired-image-thumbnail-insert-subdir-recursive)
    ;; Other
    (when (fboundp 'dired-image-thumbnail-transient)
      (define-key image-dired-thumbnail-mode-map (kbd "?") #'dired-image-thumbnail-transient))))

;;; Fast image display

(defun dired-image-thumbnail--quality-scale ()
  "Return the scale factor for `dired-image-thumbnail-display-quality'."
  (pcase dired-image-thumbnail-display-quality
    ('high  1.0)
    ('fast  0.5)
    ('faster 0.25)
    ('draft 0.125)
    (_ nil)))

(defvar dired-image-thumbnail--preview-dir nil
  "Temporary directory for preview images.")

(defun dired-image-thumbnail--preview-dir ()
  "Return the temporary directory for preview images, creating it if needed."
  (unless (and dired-image-thumbnail--preview-dir
               (file-directory-p dired-image-thumbnail--preview-dir))
    (setq dired-image-thumbnail--preview-dir
          (make-temp-file "dired-image-preview-" t)))
  dired-image-thumbnail--preview-dir)

(defun dired-image-thumbnail-clear-preview-cache ()
  "Delete the temporary preview directory and all its contents."
  (when (and (boundp 'dired-image-thumbnail--preview-dir)
             dired-image-thumbnail--preview-dir
             (file-directory-p dired-image-thumbnail--preview-dir))
    (delete-directory dired-image-thumbnail--preview-dir t)
    (setq dired-image-thumbnail--preview-dir nil)))

(defun dired-image-thumbnail--jpeg-p (file)
  "Return non-nil if FILE is a JPEG."
  (member (downcase (or (file-name-extension file) ""))
          '("jpg" "jpeg")))

(defun dired-image-thumbnail--djpeg-scale (quality-scale)
  "Return the best djpeg DCT scale fraction for QUALITY-SCALE.
djpeg supports 1/1, 1/2, 1/4, 1/8.  Maps the quality scale factor
directly to the nearest djpeg fraction."
  (cond ((<= quality-scale 0.125) "1/8")
        ((<= quality-scale 0.25)  "1/4")
        ((<= quality-scale 0.5)   "1/2")
        (t                         "1/1")))

(defun dired-image-thumbnail--preview-path (file width)
  "Return the cache path for a preview of FILE decoded at WIDTH pixels."
  (expand-file-name
   (concat (sha1 (concat file
                         (number-to-string width)
                         (format-time-string
                          "%s"
                          (file-attribute-modification-time
                           (file-attributes file)))))
           ".jpg")
   (dired-image-thumbnail--preview-dir)))

(defun dired-image-thumbnail--preview-quality ()
  "Return the JPEG encode quality for the current display quality.
Lower display qualities use lower encode quality, so preview files
are smaller and quicker to load and decode."
  (pcase dired-image-thumbnail-display-quality
    ('fast 60)
    ('faster 40)
    ('draft 25)
    (_ 50)))

(defun dired-image-thumbnail--make-preview (file width height)
  "Create a preview of FILE at WIDTH x HEIGHT pixels.
Returns the path to the preview file.
For JPEGs, uses djpeg/cjpeg with DCT scaling (very fast).
For other formats, uses magick/convert with -thumbnail.
The encode quality follows `dired-image-thumbnail-display-quality',
so lower quality modes produce smaller, quicker-loading files."
  (let ((preview-path (dired-image-thumbnail--preview-path file width)))
    (unless (file-exists-p preview-path)
      (let ((expanded (expand-file-name file))
            (quality (number-to-string (dired-image-thumbnail--preview-quality))))
        (if (and (dired-image-thumbnail--jpeg-p file)
                 (executable-find "djpeg")
                 (executable-find "cjpeg"))
            ;; Fast path: djpeg DCT scaling + cjpeg (skips full decode).
            ;; Use an intermediate temp file instead of a shell pipeline so
            ;; this works portably without relying on /bin/sh or cmd.exe.
            (let ((scale-str (dired-image-thumbnail--djpeg-scale
                              (dired-image-thumbnail--quality-scale)))
                  (temp-file (make-temp-file "dired-image-preview-djpeg-")))
              (unwind-protect
                  (progn
                    (call-process "djpeg" nil (list :file temp-file) nil
                                  "-scale" scale-str expanded)
                    (call-process "cjpeg" nil (list :file preview-path) nil
                                  "-quality" quality temp-file))
                (when (file-exists-p temp-file)
                  (delete-file temp-file))))
          ;; Fallback: magick/convert -thumbnail
          (let ((magick (or (executable-find "magick")
                            (executable-find "convert"))))
            (when magick
              (call-process magick nil nil nil
                            expanded
                            "-thumbnail" (format "%dx%d" width height)
                            "-quality" quality
                            preview-path))))))
    (if (file-exists-p preview-path)
        preview-path
      file)))

(defvar dired-image-thumbnail-display-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-d") #'dired-image-thumbnail-delete-image-and-next)
    map)
  "Keymap for the fast full-size image display buffer.")

(define-derived-mode dired-image-thumbnail-display-mode special-mode "DIT-Image"
  "Major mode for the `dired-image-thumbnail' full-size image display buffer.
Used by the fast scaled-display path so that bindings such as \\`C-d'
are available even though the buffer is not file-visiting.")

(defun dired-image-thumbnail--display-image-fast (file)
  "Display FILE scaled according to `dired-image-thumbnail-display-quality'.
For `high' quality, loads the file directly with window-fitting constraints.
For lower qualities, produces a small preview via an external tool so that
Emacs never decodes the full image."
  (setq file (expand-file-name file))
  (unless (file-exists-p file)
    (error "No such file: %s" file))
  (let* ((scale (dired-image-thumbnail--quality-scale))
         (buf (get-buffer-create image-dired-display-image-buffer))
         (cur-win (selected-window))
         (display-win (or (get-buffer-window buf)
                          (progn
                            (display-buffer buf)
                            (get-buffer-window buf))))
         (win-width (or (and display-win (window-body-width display-win t)) 800))
         (win-height (or (and display-win (window-body-height display-win t)) 600))
         (decode-w (max 1 (truncate (* win-width scale))))
         (decode-h (max 1 (truncate (* win-height scale))))
         ;; For high quality, load original; otherwise make a small preview
         (display-file (if (>= scale 1.0)
                           file
                         (dired-image-thumbnail--make-preview file decode-w decode-h)))
         ;; Load the (possibly small) file and fit to window
         (img (create-image display-file nil nil
                            :max-width win-width
                            :max-height win-height)))
    (with-current-buffer buf
      ;; If this buffer was previously used by image-dired-display-image
      ;; to visit a file, sever the file visit before modifying the
      ;; buffer.  Without this, erase-buffer triggers Emacs's lazy
      ;; locking (prepare_to_modify_buffer_1 in insdel.c) which creates
      ;; a .#filename lock symlink on the previously-displayed image.
      ;; Clearing buffer-file-name / buffer-file-truename prevents the
      ;; lock and also avoids a modified-buffer prompt on kill.
      (when (buffer-file-name)
        (set-buffer-modified-p nil)
        (set-visited-file-name nil))
      ;; Use special-mode for a clean read-only buffer with q to quit.
      ;; Do NOT use image-dired-image-mode or image-mode here — they
      ;; set up image-fit-to-window timers that expect a file-visiting
      ;; buffer and fail on our manually inserted image descriptor.
      (unless (derived-mode-p 'dired-image-thumbnail-display-mode)
        (dired-image-thumbnail-display-mode))
      (let ((inhibit-read-only t)
            (create-lockfiles nil))
        (erase-buffer)
        (insert-image img)
        (goto-char (point-min)))
      (setq cursor-type nil))
    (when display-win
      (set-window-buffer display-win buf))
    (select-window cur-win)
    (dired-image-thumbnail--queue-prefetch)))

(defvar dired-image-thumbnail--prefetch-timer nil
  "Idle timer used to pre-generate preview images around the current one.")

(defun dired-image-thumbnail--decode-dimensions ()
  "Return (WIDTH . HEIGHT) to decode previews at for the display window."
  (let* ((scale (or (dired-image-thumbnail--quality-scale) 0.5))
         (win (get-buffer-window image-dired-display-image-buffer t))
         (win-width (or (and win (window-body-width win t)) 800))
         (win-height (or (and win (window-body-height win t)) 600)))
    (cons (max 1 (truncate (* win-width scale)))
          (max 1 (truncate (* win-height scale))))))

(defun dired-image-thumbnail--queue-prefetch ()
  "Queue idle-time pre-generation of previews around the current image."
  (when dired-image-thumbnail--prefetch-timer
    (cancel-timer dired-image-thumbnail--prefetch-timer))
  (setq dired-image-thumbnail--prefetch-timer
        (run-with-idle-timer 0.3 nil #'dired-image-thumbnail--prefetch-neighbors)))

(defun dired-image-thumbnail--prefetch-neighbors ()
  "Pre-generate previews for the images adjacent to the current one.
Runs on an idle timer so that navigation in follow mode usually
lands on images whose preview already exists and can be shown
instantly.  Generation stops early when the current image changes
while it runs."
  (let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
    (when (and thumb-buf (buffer-live-p thumb-buf))
      (with-current-buffer thumb-buf
        (when (and (derived-mode-p 'image-dired-thumbnail-mode)
                   (let ((scale (dired-image-thumbnail--quality-scale)))
                     (and scale (< scale 1.0))))
          (let* ((file (image-dired-original-file-name))
                 (idx (and file
                           (cl-position file dired-image-thumbnail--current-images
                                        :test #'equal)))
                 (offsets (if (eq dired-image-thumbnail-display-quality 'draft)
                              '(1 -1 2 -2)
                            '(1 -1)))
                 (dims (dired-image-thumbnail--decode-dimensions))
                 (width (car dims))
                 (height (cdr dims)))
            (when idx
              (dolist (offset offsets)
                (let ((nb (and (>= (+ idx offset) 0)
                               (nth (+ idx offset)
                                    dired-image-thumbnail--current-images))))
                  (when (and nb (file-exists-p nb)
                             (equal (image-dired-original-file-name) file)
                             (not (file-exists-p
                                   (dired-image-thumbnail--preview-path
                                    nb width))))
                    (ignore-errors
                      (dired-image-thumbnail--make-preview
                       nb width height))))))))))))

(defun dired-image-thumbnail--maybe-realign ()
  "Refresh thumbnails when the thumbnail window width has changed.
Showing the full-size image (follow mode) can open or narrow the
thumbnail window, in which case the display is refreshed so the
thumbnails re-align and the columns fit the new window width.
Does nothing in wrap display mode, where thumbnails wrap to the
window width automatically."
  (when (and (derived-mode-p 'image-dired-thumbnail-mode)
             (null dired-image-thumbnail-wrap-display))
    (let ((win (get-buffer-window nil t)))
      (when (and win
                 (or (null dired-image-thumbnail--lineup-width)
                     (/= (window-body-width win)
                         dired-image-thumbnail--lineup-width)))
        (dired-image-thumbnail-refresh)))))

(defun dired-image-thumbnail--display-this ()
  "Display the current thumbnail's image.
Uses fast scaled display unless quality is `full'."
  (if (dired-image-thumbnail--quality-scale)
      (let ((file (image-dired-original-file-name)))
        (cond ((not (image-dired-image-at-point-p))
               (message "No thumbnail at point"))
              ((not file)
               (message "No original file name found"))
              (t
               (dired-image-thumbnail--display-image-fast file))))
    (image-dired-display-this))
  (dired-image-thumbnail--maybe-realign))

;;; Enhanced navigation and deletion

(defun dired-image-thumbnail-select-display-quality ()
  "Select the display quality with `completing-read'.
Choose among `full', `high', `fast', `faster' and `draft'; the
choice takes effect immediately and refreshes the displayed image."
  (interactive)
  (setq dired-image-thumbnail-display-quality
        (intern (completing-read
                 "Display quality: "
                 '("full" "high" "fast" "faster" "draft")
                 nil t)))
  (message "Display quality: %s" dired-image-thumbnail-display-quality)
  (image-dired--update-header-line)
  (dired-image-thumbnail--display-this))

(defun dired-image-thumbnail--disable-marking-shows-next ()
  "Disable vanilla `image-dired-marking-shows-next' buffer-locally.
Vanilla marking always advances to the next image, and with this
variable non-nil it also displays it through the full-image
display path, bypassing the fast preview pipeline, which made
marking take seconds.  The package's mark and unmark commands
handle the follow display instead."
  (when (boundp 'image-dired-marking-shows-next)
    (setq-local image-dired-marking-shows-next nil)))

(defun dired-image-thumbnail-toggle-auto-display ()
  "Toggle automatic display of the full-size image while navigating.
When enabled, moving to the next/previous thumbnail updates the image
display buffer automatically.  When disabled, navigation and marking
only move point, so you can step through, mark or delete many
thumbnails without paying the cost of decoding each image."
  (interactive)
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (setq dired-image-thumbnail-auto-display-on-navigate
        (not dired-image-thumbnail-auto-display-on-navigate))
  (dired-image-thumbnail--disable-marking-shows-next)
  (when dired-image-thumbnail-auto-display-on-navigate
    (dired-image-thumbnail--display-this))
  (message "Follow (auto-display on navigate): %s"
           (if dired-image-thumbnail-auto-display-on-navigate "ON" "OFF")))

(defun dired-image-thumbnail-next-image ()
  "Move to next thumbnail and optionally display full-size image.
When `dired-image-thumbnail-auto-display-on-navigate' is non-nil,
the full-size image is automatically displayed."
  (interactive)
  (image-dired-forward-image)
  (when dired-image-thumbnail-auto-display-on-navigate
    (dired-image-thumbnail--display-this)))

(defun dired-image-thumbnail-previous-image ()
  "Move to previous thumbnail and optionally display full-size image.
When `dired-image-thumbnail-auto-display-on-navigate' is non-nil,
the full-size image is automatically displayed."
  (interactive)
  (image-dired-backward-image)
  (when dired-image-thumbnail-auto-display-on-navigate
    (dired-image-thumbnail--display-this)))

(defun dired-image-thumbnail-delete-and-next ()
  "Delete current image file and move to next thumbnail.
This permanently deletes the file from disk and removes its thumbnail."
  (interactive)
  (let ((file-name (image-dired-original-file-name)))
    (when (and file-name
               (or dired-image-thumbnail-auto-accept
                   (y-or-n-p (format "Delete %s? " (file-name-nondirectory file-name)))))
      (delete-file file-name t)
      (setq dired-image-thumbnail--current-images
            (remove file-name dired-image-thumbnail--current-images))
      (setq dired-image-thumbnail--all-images
            (remove file-name dired-image-thumbnail--all-images))
      (image-dired-delete-char)
      ;; Respect the auto-display setting: when off, just move to the
      ;; next thumbnail without decoding and displaying it.
      (when (and (not (eobp))
                 dired-image-thumbnail-auto-display-on-navigate)
        (dired-image-thumbnail--display-this))
      (message "Deleted %s" file-name))))

(defun dired-image-thumbnail-delete-image-and-next ()
  "Delete current image displayed in the image-dired display buffer.
Gets the current file from the thumbnail buffer's text properties,
since the display buffer is not a file-visiting buffer."
  (interactive)
  (let ((current-file
         (or (buffer-file-name)
             (when-let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
               (with-current-buffer thumb-buf
                 (image-dired-original-file-name))))))
    (when (and current-file
               (or dired-image-thumbnail-auto-accept
                   (y-or-n-p (format "Delete %s? " (file-name-nondirectory current-file)))))
      ;; Update internal lists in the thumbnail buffer
      (when-let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
        (with-current-buffer thumb-buf
          (setq dired-image-thumbnail--current-images
                (remove current-file dired-image-thumbnail--current-images))
          (setq dired-image-thumbnail--all-images
                (remove current-file dired-image-thumbnail--all-images))
          (image-dired-forward-image)
          ;; Respect the auto-display setting.
          (when dired-image-thumbnail-auto-display-on-navigate
            (dired-image-thumbnail--display-this))))
      (delete-file current-file t)
      (message "Deleted %s" current-file))))

;;; Window layout management

(defun dired-image-thumbnail--apply-layout ()
  "Set up the thumbnail and image windows.
The arrangement follows `dired-image-thumbnail-window-layout'.  Called
from `dired-image-thumbnail' after buffers have been created."
  (when-let ((layout dired-image-thumbnail-window-layout)
             (thumb-buf (get-buffer image-dired-thumbnail-buffer)))
    (delete-other-windows)
    (if (eq layout 'thumb-only)
        (switch-to-buffer thumb-buf)
      (let* ((ratio (or dired-image-thumbnail-window-ratio 0.4))
             (horizontal (memq layout '(left-right right-left)))
             (thumb-first (memq layout '(left-right top-bottom)))
             (img-buf (get-buffer-create image-dired-display-image-buffer))
             (first-buf (if thumb-first thumb-buf img-buf))
             (second-buf (if thumb-first img-buf thumb-buf))
             (size (if horizontal
                       (round (* (frame-width) (if thumb-first ratio (- 1.0 ratio))))
                     (round (* (frame-height) (if thumb-first ratio (- 1.0 ratio)))))))
        (switch-to-buffer first-buf)
        (if horizontal
            (split-window-right size)
          (split-window-below size))
        (other-window 1)
        (switch-to-buffer second-buf)
        ;; Leave focus on the thumbnail buffer
        (select-window (get-buffer-window thumb-buf))))))

(defun dired-image-thumbnail-display-at-direction (buffer alist)
  "Display BUFFER in the direction specified in ALIST, reusing existing windows.
This is more aggressive than `display-buffer-in-direction' as it will
take over an existing window in that direction even if it's visiting
another buffer, which is ideal for the thumbnail/image split layout."
  (let* ((direction (cdr (assoc 'direction alist)))
         (target-win (window-in-direction direction)))
    (if (and target-win (window-live-p target-win))
        (progn
          (window--display-buffer buffer target-win 'reuse alist)
          target-win)
      (display-buffer-in-direction buffer alist))))

(defun dired-image-thumbnail-setup-display-buffer ()
  "Configure `display-buffer-alist' rules for thumbnail and image buffers.
Only adds rules when `dired-image-thumbnail-window-layout' is non-nil,
so that `display-buffer' respects the layout for subsequent pop-to-buffer
calls (e.g. when the image window is reused during navigation).

Any rules previously installed by this function are removed first, so it
is idempotent and can be re-run to track changes to
`dired-image-thumbnail-window-layout' / `-window-ratio'."
  ;; Drop any rules we installed earlier so re-running picks up current
  ;; custom values rather than stacking stale entries.
  (setq display-buffer-alist
        (seq-remove (lambda (entry)
                      (member (car-safe entry)
                              '("\\*image-dired\\*"
                                "\\*image-dired-display-image\\*")))
                    display-buffer-alist))
  (when dired-image-thumbnail-window-layout
    (let* ((layout dired-image-thumbnail-window-layout)
           (horizontal (memq layout '(left-right right-left)))
           (thumb-first (memq layout '(left-right top-bottom)))
           (ratio (or dired-image-thumbnail-window-ratio 0.4))
           (thumb-dir (if thumb-first
                          (if horizontal 'left 'above)
                        (if horizontal 'right 'below)))
           (img-dir (if thumb-first
                        (if horizontal 'right 'below)
                      (if horizontal 'left 'above)))
           (thumb-size (if horizontal
                          `(window-width . ,ratio)
                        `(window-height . ,ratio)))
           (img-size (if horizontal
                        `(window-width . ,(- 1.0 ratio))
                       `(window-height . ,(- 1.0 ratio)))))
      (add-to-list 'display-buffer-alist
                   `("\\*image-dired\\*"
                     display-buffer-in-direction
                     (direction . ,thumb-dir)
                     (window . root)
                     ,thumb-size))
      (unless (eq layout 'thumb-only)
        (add-to-list 'display-buffer-alist
                     `("\\*image-dired-display-image\\*"
                       (display-buffer-reuse-window
                        dired-image-thumbnail-display-at-direction)
                       (direction . ,img-dir)
                       (window . root)
                       ,img-size))))))

;;;###autoload
(with-eval-after-load 'image-dired
  (dired-image-thumbnail-setup-keys)
  (dired-image-thumbnail-setup-display-buffer)
  (advice-add 'image-dired-format-properties-string :around #'dired-image-thumbnail--format-properties-string)
  ;; Invalidate the cached marked count whenever marks change.
  (advice-add 'image-dired--thumb-update-marks :after #'dired-image-thumbnail--invalidate-marked-count)
  ;; Suppress lock files on image files visited for display (read-only).
  (advice-add 'image-dired-display-image :around #'dired-image-thumbnail--display-image-no-lock)
  ;; Hook to initialize our variables when entering thumbnail mode
  (add-hook 'image-dired-thumbnail-mode-hook #'dired-image-thumbnail--initialize-buffer)
  ;; Scope C-d to the image-dired display buffer only
  (when (keymapp image-dired-display-image-mode-map)
    (define-key image-dired-display-image-mode-map (kbd "C-d") #'dired-image-thumbnail-delete-image-and-next)))

;; Clean up the temporary preview directory when Emacs exits.
(add-hook 'kill-emacs-hook #'dired-image-thumbnail-clear-preview-cache)

(defun dired-image-thumbnail-unload-function ()
  "Tear down advice and hooks installed by dired-image-thumbnail.
Called by `unload-feature'.  Returns nil so standard unloading proceeds."
  (when dired-image-thumbnail--prefetch-timer
    (cancel-timer dired-image-thumbnail--prefetch-timer)
    (setq dired-image-thumbnail--prefetch-timer nil))
  (advice-remove 'image-dired-format-properties-string
                 #'dired-image-thumbnail--format-properties-string)
  (advice-remove 'image-dired--thumb-update-marks
                 #'dired-image-thumbnail--invalidate-marked-count)
  (advice-remove 'image-dired-display-image
                 #'dired-image-thumbnail--display-image-no-lock)
  (remove-hook 'image-dired-thumbnail-mode-hook
               #'dired-image-thumbnail--initialize-buffer)
  (remove-hook 'kill-emacs-hook #'dired-image-thumbnail-clear-preview-cache)
  ;; Remove the current-thumbnail highlight from any thumbnail buffers.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'image-dired-thumbnail-mode)
        (remove-hook 'post-command-hook
                     #'dired-image-thumbnail--update-current-highlight t)
        (when (overlayp dired-image-thumbnail--current-overlay)
          (delete-overlay dired-image-thumbnail--current-overlay))
        (kill-local-variable 'face-remapping-alist))))
  (dired-image-thumbnail-clear-preview-cache)
  ;; Drop the display-buffer rules we installed.
  (setq display-buffer-alist
        (seq-remove (lambda (entry)
                      (member (car-safe entry)
                              '("\\*image-dired\\*"
                                "\\*image-dired-display-image\\*")))
                    display-buffer-alist))
  nil)

;; Load transient menu support if available
(when (require 'dired-image-thumbnail-transient nil t)
  (dired-image-thumbnail-transient-setup-keys))

(provide 'dired-image-thumbnail)
;;; dired-image-thumbnail.el ends here

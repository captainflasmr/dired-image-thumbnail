;;; dired-image-thumbnail.el --- Enhanced workflow for image-dired -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Dyer

;; Author: James Dyer
;; Version: 2.6.0
;; Package-Requires: ((emacs "29.1"))
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
;; functions introduced with the image-dired rewrite in Emacs 29.1.  They
;; are not part of image-dired's public API, so they are declared here
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
  "Default sorting criteria for thumbnails.
The choice is remembered per directory in that directory's
`.dir-locals.el' (see `dired-image-thumbnail-save-dir-settings'),
so revisiting a directory restores its sort order."
  :type '(choice (const :tag "Dired Order" dired)
                 (const :tag "Name" name)
                 (const :tag "Date modified" date)
                 (const :tag "Size" size))
  :safe #'symbolp
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-sort-order 'ascending
  "Default sort order for thumbnails.
The choice is remembered per directory in that directory's
`.dir-locals.el' (see `dired-image-thumbnail-save-dir-settings'),
so revisiting a directory restores its sort order."
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
  '((t (:box (:line-width -5))))
  "Face used to highlight the currently selected thumbnail.
The outline is thick by default (5 pixels) and is drawn inside the
thumbnail, so highlighting a different thumbnail while navigating
never changes the thumbnail sizes or re-flows the grid.  Its colour
follows the active theme: the background colour of the `highlight'
face is used, so the outline looks like the theme's own highlight,
just thicker.  Customise this face to change the width, or give
:box an explicit :color to pin a fixed colour.  Set
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

(defcustom dired-image-thumbnail-window-ratio 0.6
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
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-display-quality 'faster
  "Display quality when navigating thumbnails with n/p.
Controls the trade-off between image quality and navigation speed,
from 1 (best) to 5 (fastest):

   1 `full'   - Original resolution via `image-dired-display-image'.
                Slowest, but pixel-perfect.
   2 `high'   - Scaled to fit the display window (1:1 window pixels).
   3 `fast'   - Half the window dimensions, JPEG quality 60.
   4 `faster' - Quarter the window dimensions, JPEG quality 40.
   5 `draft'  - 1/8 the window dimensions, JPEG quality 25.  Very fast,
                visibly soft; two neighbours are pre-generated instead
                of one.

Lower qualities decode less data but are still scaled to fit the
display window, so they look softer rather than smaller.

Preview files are optimally compressed per mode and the previews of
neighbouring images are pre-generated while idle, so navigating with
follow mode on usually shows the next image instantly.  Displayed
previews are kept in the image cache, so revisiting an image costs
nothing.

Interactively select the quality with
`dired-image-thumbnail-select-display-quality' (bound to `Q'),
which takes effect immediately.  Customising this variable takes
effect on the next n/p keypress.

The choice is remembered per directory in that directory's
`.dir-locals.el' (see `dired-image-thumbnail-save-dir-settings'),
so directories with typically small images (e.g. screenshots) can
keep `high' while photo collections use a faster mode."
  :type '(choice (const :tag "1 - Full resolution (slowest)" full)
                 (const :tag "2 - High - window size" high)
                 (const :tag "3 - Fast - 1/2 window" fast)
                 (const :tag "4 - Faster - 1/4 window (default)" faster)
                 (const :tag "5 - Draft - 1/8 window (fastest)" draft))
:safe #'symbolp
   :group 'dired-image-thumbnail)

(defconst dired-image-thumbnail--quality-choices
  '((full   "1" "1 - full: original resolution (slowest, pixel-perfect)")
    (high   "2" "2 - high: window size 1:1 (sharp fit)")
    (fast   "3" "3 - fast: half size, JPEG 60")
    (faster "4" "4 - faster: quarter size, JPEG 40")
    (draft  "5" "5 - draft: eighth size, JPEG 25 (fastest)"))
  "Numbered display-quality choices, best (1) to fastest (5).
Each entry is (SYMBOL NUMBER DESCRIPTION).  The symbols are the
stored values; the numbers and descriptions are presentation only,
so existing customisations and `.dir-locals.el' entries keep
working.  Used by `dired-image-thumbnail-select-display-quality'.")

(defun dired-image-thumbnail--filter-plist-p (value)
  "Return non-nil if VALUE is nil or a valid filter plist.
A valid plist has the form (:name REGEXP :size-min BYTES
:size-max BYTES), where each value may be nil."
  (or (null value)
      (and (listp value)
           (zerop (mod (length value) 2))
           (let ((name (plist-get value :name))
                 (size-min (plist-get value :size-min))
                 (size-max (plist-get value :size-max)))
             (and (or (null name) (stringp name))
                  (or (null size-min) (natnump size-min))
                  (or (null size-max) (natnump size-max)))))))

(defcustom dired-image-thumbnail-default-filter nil
  "Default name/size filter, as a plist or nil for no filter.
The plist has the form (:name REGEXP :size-min BYTES :size-max
BYTES), where each value may be nil.  This is normally managed
automatically: changing the filter in a thumbnail buffer stores
the value in that directory's `.dir-locals.el' (see
`dired-image-thumbnail-save-dir-settings'), and opening the
directory restores it."
  :type '(choice (const :tag "No filter" nil)
                 (sexp :tag "Filter plist (:name REGEXP :size-min BYTES :size-max BYTES)"))
  :safe #'dired-image-thumbnail--filter-plist-p
  :group 'dired-image-thumbnail)

(defcustom dired-image-thumbnail-save-dir-settings t
  "Whether to remember quality, sort and filter choices per directory.
When non-nil, changing the display quality (with
`dired-image-thumbnail-select-display-quality'), the sort order or
the filter in a thumbnail buffer writes the choice into that
directory's `.dir-locals.el' (under the nil class, merged with any
existing entries), and opening the directory restores it.  Only
the package's own variables are ever written; other entries are
left untouched.  Set to nil to keep such choices session-local."
  :type 'boolean
  :safe #'booleanp
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

(defvar-local dired-image-thumbnail--display-quality nil
  "Display quality override for this buffer.
Nil means follow the global `dired-image-thumbnail-display-quality'.
Set by `dired-image-thumbnail-select-display-quality' and by the
per-directory settings in `.dir-locals.el' (see
`dired-image-thumbnail-save-dir-settings').")

(defvar-local dired-image-thumbnail--filter-name nil
  "Current name filter regexp.")

(defvar-local dired-image-thumbnail--filter-size-min nil
  "Minimum size filter in bytes.")

(defvar-local dired-image-thumbnail--filter-size-max nil
  "Maximum size filter in bytes.")

(defvar-local dired-image-thumbnail--display-size nil
  "Current display size for thumbnails (for zoom).
Nil means follow image-dired's own thumbnail size, so the existing
thumbnail cache can be displayed without regeneration.")

(defvar-local dired-image-thumbnail--dimension-cache (make-hash-table :test 'equal)
  "Cache for image dimensions keyed by file name.")

(defvar-local dired-image-thumbnail--dimension-pending (make-hash-table :test 'equal)
  "Files pending dimension calculation.
Value is `queued' while waiting for a free process slot and
`running' once a process has been started.")

(defvar-local dired-image-thumbnail--recursive nil
  "Non-nil if thumbnails include images from subdirectories.")

(defvar-local dired-image-thumbnail--thumbs-generated-at nil
  "Thumbnail size at which the current cached thumb files were generated.
Cached thumb files are shown at their natural size, so when the
display size changes the files must be regenerated at the new
size for the resize to be visible.  During a deferred resize this
holds the target size while regeneration is in progress.")

(defvar-local dired-image-thumbnail--marked-count nil
  "Cached count of marked images.  Nil means it needs recomputation.")

(defvar-local dired-image-thumbnail--image-index (make-hash-table :test 'equal)
  "Map from file name to its index in `dired-image-thumbnail--current-images'.
Kept in sync by `dired-image-thumbnail--rebuild-image-index'.")

(defvar-local dired-image-thumbnail--thumb-attempts (make-hash-table :test 'equal)
  "Hash of thumbnail-creation attempts per image, keyed by file name.
Caps retries so a permanently failing image cannot cause an endless
queue-and-refresh cycle.")

(defvar-local dired-image-thumbnail--thumb-queued (make-hash-table :test 'equal)
  "Files queued for thumbnail generation in the current cycle.
Prevents the same file being queued more than once while thumbnails
regenerate in the background, and lets the queue poll detect when a
queued thumbnail has appeared on disk.")

(defvar-local dired-image-thumbnail--resize-pending nil
  "Non-nil while cached thumbnails are being regenerated at a new size.
During this time the current thumbnails stay on screen and the
display is only refreshed once the new-size thumbnails are ready.")

(defvar-local dired-image-thumbnail--lineup-width nil
  "Width in columns of the thumbnail window at the last line-up.
When showing the full-size image changes the window layout (and so
this width), the thumbnails are refreshed so they re-align and the
columns fit the new window width.")

(defvar dired-image-thumbnail--identify-cached-command 'unchecked
  "Cached command used to query image dimensions.
A list (PROGRAM ARGS...) or nil when no suitable ImageMagick
command is available.  The symbol `unchecked' means the search
has not been performed yet.")

(defun dired-image-thumbnail--identify-command ()
  "Return the command that queries image dimensions, or nil.
Uses `identify' when available, falling back to `magick identify'."
  (when (eq dired-image-thumbnail--identify-cached-command 'unchecked)
    (setq dired-image-thumbnail--identify-cached-command
          (cond ((executable-find "identify") '("identify"))
                ((executable-find "magick") '("magick" "identify"))
                (t nil))))
  dired-image-thumbnail--identify-cached-command)

(defvar dired-image-thumbnail--identify-max-processes 4
  "Maximum number of concurrent dimension-query processes.")

(defvar dired-image-thumbnail--identify-running 0
  "Number of dimension-query processes currently running.")

(defvar dired-image-thumbnail--identify-queue nil
  "Queue of (BUFFER . FILE) dimension queries waiting for a process slot.")

(defun dired-image-thumbnail--pump-identify-queue ()
  "Start queued dimension queries, up to the concurrency limit."
  (while (and dired-image-thumbnail--identify-queue
              (< dired-image-thumbnail--identify-running
                 dired-image-thumbnail--identify-max-processes))
    (let* ((item (pop dired-image-thumbnail--identify-queue))
           (buf (car item))
           (file (cdr item)))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (eq (gethash file dired-image-thumbnail--dimension-pending)
                       'queued)))
        (setq dired-image-thumbnail--identify-running
              (1+ dired-image-thumbnail--identify-running))
        (with-current-buffer buf
          (puthash file 'running dired-image-thumbnail--dimension-pending)
          (dired-image-thumbnail--start-identify-process file))))))

(defun dired-image-thumbnail--get-image-dimensions (file)
  "Get dimensions of image FILE as (width . height), or (0 . 0) if unknown.
If not cached, queue an async process (`identify') to fill the cache."
  (or (gethash file dired-image-thumbnail--dimension-cache)
      (progn
        (unless (or (gethash file dired-image-thumbnail--dimension-pending)
                    (null (dired-image-thumbnail--identify-command)))
          (puthash file 'queued dired-image-thumbnail--dimension-pending)
          (setq dired-image-thumbnail--identify-queue
                (nconc dired-image-thumbnail--identify-queue
                       (list (cons (current-buffer) file))))
          (dired-image-thumbnail--pump-identify-queue))
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

(defun dired-image-thumbnail--property-positions (prop)
  "Return buffer positions where text property PROP is set, in order.
Jumps between property-change boundaries (O(runs)) rather than
scanning one character at a time."
  (let ((pos (point-min))
        (positions nil))
    (while (and pos (< pos (point-max)))
      (when (get-text-property pos prop)
        (push pos positions))
      (setq pos (next-single-property-change pos prop nil (point-max))))
    (nreverse positions)))

(defun dired-image-thumbnail--property-values (prop)
  "Return the values of text property PROP at each set position, in order."
  (mapcar (lambda (pos) (get-text-property pos prop))
          (dired-image-thumbnail--property-positions prop)))

(defun dired-image-thumbnail--position-of-file (file)
  "Return the buffer position of the thumbnail for FILE, or nil."
  (let ((pos (point-min))
        (found nil))
    (while (and (not found) pos (< pos (point-max)))
      (when (equal (get-text-property pos 'original-file-name) file)
        (setq found pos))
      (setq pos (next-single-property-change
                 pos 'original-file-name nil (point-max))))
    found))

(defun dired-image-thumbnail--start-identify-process (file)
  "Start an async process to get dimensions for FILE."
  (let ((proc-buf (generate-new-buffer " *dired-image-thumb-identify*"))
        (thumb-buf (current-buffer))
        (file-attr file))
    (condition-case nil
        (make-process
         :name "dired-image-thumb-identify"
         :buffer proc-buf
         :command (append (dired-image-thumbnail--identify-command)
                          (list "-format" "%w %h\n" (expand-file-name file)))
         :noquery t
         :sentinel
         (lambda (proc _event)
           (when (eq (process-status proc) 'exit)
             (unwind-protect
                 (when (and (zerop (process-exit-status proc))
                            (buffer-live-p (process-buffer proc)))
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (let* ((line (buffer-substring-no-properties
                                   (point-min) (line-end-position)))
                            (nums (split-string line)))
                       (when (and (= (length nums) 2)
                                  (string-match-p "^[0-9]+$" (car nums))
                                  (string-match-p "^[0-9]+$" (cadr nums)))
                         (let ((w (string-to-number (car nums)))
                               (h (string-to-number (cadr nums))))
                           (when (buffer-live-p thumb-buf)
                             (with-current-buffer thumb-buf
                               (puthash file-attr (cons w h) dired-image-thumbnail--dimension-cache)))
                           (dolist (b (buffer-list))
                             (with-current-buffer b
                               (when (derived-mode-p 'image-dired-thumbnail-mode)
                                 (image-dired--update-header-line)))))))))
               (when (buffer-live-p (process-buffer proc))
                 (kill-buffer (process-buffer proc)))
               (when (buffer-live-p thumb-buf)
                 (with-current-buffer thumb-buf
                   (remhash file-attr dired-image-thumbnail--dimension-pending))))
             (setq dired-image-thumbnail--identify-running
                   (max 0 (1- dired-image-thumbnail--identify-running)))
             (dired-image-thumbnail--pump-identify-queue))))
      (file-error
       (kill-buffer proc-buf)
       (when (buffer-live-p thumb-buf)
         (with-current-buffer thumb-buf
           (remhash file dired-image-thumbnail--dimension-pending)))
       (setq dired-image-thumbnail--identify-running
             (max 0 (1- dired-image-thumbnail--identify-running)))
       (dired-image-thumbnail--pump-identify-queue)))))

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

(defun dired-image-thumbnail--rebuild-image-index ()
  "Rebuild the file-to-index hash from `dired-image-thumbnail--current-images'."
  (clrhash dired-image-thumbnail--image-index)
  (let ((index 0))
    (dolist (file dired-image-thumbnail--current-images)
      (puthash file index dired-image-thumbnail--image-index)
      (setq index (1+ index)))))

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
Installed as `:after' advice on `image-dired--thumb-update-marks', the
common choke point for bulk mark changes (both ours and native
image-dired commands).  The header line is refreshed right away so
the `[N marked]' segment reflects the change immediately."
  (when (derived-mode-p 'image-dired-thumbnail-mode)
    (setq dired-image-thumbnail--marked-count nil)
    (image-dired--update-header-line)))

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
             (let ((decorated (mapcar (lambda (f)
                                        (cons (or (file-attribute-modification-time
                                                   (file-attributes f))
                                                  0)
                                              f))
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
               (when-let ((attrs (file-attributes file)))
                 (let ((size (file-attribute-size attrs)))
                   (and (or (null dired-image-thumbnail--filter-size-min)
                            (>= size dired-image-thumbnail--filter-size-min))
                        (or (null dired-image-thumbnail--filter-size-max)
                            (<= size dired-image-thumbnail--filter-size-max))))))
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

;;; Per-directory settings (.dir-locals.el)

(defconst dired-image-thumbnail--dir-setting-vars
  '(dired-image-thumbnail-display-quality
    dired-image-thumbnail-sort-by
    dired-image-thumbnail-sort-order
    dired-image-thumbnail-default-filter)
  "Variables managed in a directory's `.dir-locals.el' (nil class).
Only these variables are ever read or written there by this
package; any other entries are left untouched.")

(defun dired-image-thumbnail--valid-dir-setting-p (var value)
  "Return non-nil if VALUE is acceptable for directory setting VAR.
The file is read explicitly (Emacs safety checks are bypassed),
so values are validated strictly here before being applied."
  (pcase var
    ('dired-image-thumbnail-display-quality
     (memq value '(full high fast faster draft)))
    ('dired-image-thumbnail-sort-by
     (memq value '(dired name date size)))
    ('dired-image-thumbnail-sort-order
     (memq value '(ascending descending)))
    ('dired-image-thumbnail-default-filter
     (dired-image-thumbnail--filter-plist-p value))
    (_ nil)))

(defun dired-image-thumbnail--read-dir-settings (&optional dir)
  "Return managed settings from DIR's `.dir-locals.el'.
DIR defaults to `dired-image-thumbnail--source-dir'.  Only the nil
class is consulted.  The result is an alist of (VAR . VALUE) for
the variables in `dired-image-thumbnail--dir-setting-vars' that
are present with valid values; invalid or absent entries are
skipped.  Nothing is read when `enable-local-variables' is nil.
Never signals an error."
  (let ((dir (or dir dired-image-thumbnail--source-dir))
        (result nil))
    (when (and dir enable-local-variables)
      (let ((file (expand-file-name dir-locals-file dir)))
        (when (file-readable-p file)
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents file)
                (let* ((all (read (current-buffer)))
                       (entry (and (listp all) (assq nil all))))
                  (dolist (var dired-image-thumbnail--dir-setting-vars)
                    (let ((cell (assq var (cdr entry))))
                      (when cell
                        (when (dired-image-thumbnail--valid-dir-setting-p
                               var (cdr cell))
                          (push (cons var (cdr cell)) result)))))))
            (error nil)))))
    (nreverse result)))

(defun dired-image-thumbnail--write-dir-locals (dir settings)
  "Merge SETTINGS ((VAR . VALUE) ...) into DIR's `.dir-locals.el'.
SETTINGS entries are stored under the nil class; every other class
and variable is preserved, as are any leading comment lines.  A
short header comment is added when the file is created."
  (let* ((file (expand-file-name dir-locals-file dir))
         (new-file (not (file-exists-p file)))
         (prefix (if new-file
                     ";;; Directory Local Variables\n;;; For more information see (info \"(emacs) Directory Variables\")\n\n"
                   (condition-case nil
                       (with-temp-buffer
                         (insert-file-contents file)
                         (goto-char (point-min))
                         (while (looking-at-p "[ \t]*\\(;\\|$\\)")
                           (forward-line 1))
                         (let ((s (buffer-substring-no-properties (point-min) (point))))
                           (if (string-empty-p s)
                               ";;; Directory Local Variables\n;;; For more information see (info \"(emacs) Directory Variables\")\n\n"
                             s)))
                     (error ""))))
         (all (if new-file
                  nil
                (condition-case nil
                    (with-temp-buffer
                      (insert-file-contents file)
                      (let ((sexp (read (current-buffer))))
                        (and (listp sexp) sexp)))
                  (error nil))))
         (entry (assq nil all)))
    (unless entry
      (setq entry (cons nil nil))
      (push entry all))
    (dolist (kv settings)
      (let ((cell (assq (car kv) (cdr entry))))
        (if cell
            (setcdr cell (cdr kv))
          (setcdr entry (append (cdr entry) (list (cons (car kv) (cdr kv))))))))
    (with-temp-file file
      (insert prefix)
      (unless (string-suffix-p "\n" prefix)
        (insert "\n"))
      (pp all (current-buffer)))))

(defun dired-image-thumbnail--save-dir-settings (settings)
  "Remember SETTINGS ((VAR . VALUE) ...) in the source directory.
The values are merged into the `.dir-locals.el' file in
`dired-image-thumbnail--source-dir'.  Does nothing when
`dired-image-thumbnail-save-dir-settings' is nil, when
`enable-local-variables' is nil, when there is no source
directory, or when the file is not writable.  Never signals an
error; failures are reported with `message'."
  (when (and dired-image-thumbnail-save-dir-settings
             enable-local-variables
             dired-image-thumbnail--source-dir
             settings)
    (let* ((dir (expand-file-name dired-image-thumbnail--source-dir))
           (file (expand-file-name dir-locals-file dir)))
      (if (not (file-writable-p file))
          (message "Directory settings not saved (not writable): %s" file)
        (condition-case err
            (progn
              (dired-image-thumbnail--write-dir-locals dir settings)
              (message "Remembered %s for %s"
                       (mapconcat (lambda (kv)
                                    (format "%s=%s" (car kv) (cdr kv)))
                                  settings ", ")
                       (abbreviate-file-name dir)))
          (error (message "Could not save directory settings: %s"
                          (error-message-string err))))))))

(defun dired-image-thumbnail--apply-dir-settings ()
  "Apply per-directory quality, sort and filter settings.
Reads the `.dir-locals.el' file in
`dired-image-thumbnail--source-dir' and initialises the
buffer-local state from it, falling back to the global defaults
for anything not stored.  Directories with typically small images
(e.g. screenshots) can thus keep a higher display quality while
photo collections use a faster mode."
  (let ((settings (dired-image-thumbnail--read-dir-settings)))
    (setq dired-image-thumbnail--display-quality
          (cdr (assq 'dired-image-thumbnail-display-quality settings)))
    (setq dired-image-thumbnail--sort-by
          (or (cdr (assq 'dired-image-thumbnail-sort-by settings))
              dired-image-thumbnail-sort-by))
    (setq dired-image-thumbnail--sort-order
          (or (cdr (assq 'dired-image-thumbnail-sort-order settings))
              dired-image-thumbnail-sort-order))
    (let ((filter (cdr (assq 'dired-image-thumbnail-default-filter settings))))
      (setq dired-image-thumbnail--filter-name
            (let ((name (plist-get filter :name)))
              (and (stringp name) (not (string-empty-p name)) name)))
      (setq dired-image-thumbnail--filter-size-min
            (let ((v (plist-get filter :size-min)))
              (and (natnump v) v)))
      (setq dired-image-thumbnail--filter-size-max
            (let ((v (plist-get filter :size-max)))
              (and (natnump v) v))))))

(defun dired-image-thumbnail--save-current-filter ()
  "Persist the current name/size filter to the source directory."
  (dired-image-thumbnail--save-dir-settings
   (list (cons 'dired-image-thumbnail-default-filter
               (if (or dired-image-thumbnail--filter-name
                       dired-image-thumbnail--filter-size-min
                       dired-image-thumbnail--filter-size-max)
                   (list :name dired-image-thumbnail--filter-name
                         :size-min dired-image-thumbnail--filter-size-min
                         :size-max dired-image-thumbnail--filter-size-max)
                 nil)))))

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
This is called via hook when entering `image-dired-thumbnail-mode'
and by `dired-image-thumbnail--display-thumbs-advice' after
`image-dired-display-thumbs', so that starting from plain
`image-dired' also sets up the enhanced workflow."
  ;; Keep the current-thumbnail highlight in step with point movement.
  (add-hook 'post-command-hook #'dired-image-thumbnail--update-current-highlight nil t)
  ;; Colour the cursor to match, buffer-locally.
  (dired-image-thumbnail--setup-cursor)
  ;; Show the display quality in the mode line.
  (dired-image-thumbnail--setup-mode-line)
  ;; Any pre-existing thumbnail cache was generated at image-dired's
  ;; own thumb size, so record it as the baseline for resize
  ;; regeneration.
  (when (and (null dired-image-thumbnail--thumbs-generated-at)
             (numberp image-dired-thumb-size))
    (setq dired-image-thumbnail--thumbs-generated-at image-dired-thumb-size))
  ;; Keep image-dired's mark-and-advance display in step with auto-display.
  (dired-image-thumbnail--disable-marking-shows-next)
  ;; Find the Dired buffer associated with the thumbnails currently in
  ;; the buffer (set by `image-dired-insert-thumbnail').  Jump between
  ;; property-change boundaries (O(thumbnails)) rather than scanning one
  ;; character at a time.
  (let ((found-buf (car (dired-image-thumbnail--property-values
                         'associated-dired-buffer)))
        (here (expand-file-name default-directory))
        (images nil)
        (dired-buf dired-image-thumbnail--dired-buffer)
        (source-dir dired-image-thumbnail--source-dir))
    ;; Skip when already initialized for the Dired buffer now displayed,
    ;; so session sort/filter choices survive repeated displays.  A
    ;; different Dired buffer (or a fresh buffer) means the thumbnail
    ;; buffer was re-populated, so state is rebuilt below.
    (unless (and dired-image-thumbnail--all-images
                 (buffer-live-p dired-image-thumbnail--dired-buffer)
                 (or (null found-buf)
                     (eq found-buf dired-image-thumbnail--dired-buffer)))
      (setq dired-buf (or dired-buf found-buf))
      ;; If the properties did not identify a Dired buffer, fall back
      ;; to a live one visiting this thumbnail buffer's directory.
      (unless dired-buf
        (let ((buffers (buffer-list)))
          (while (and (not dired-buf) buffers)
            (let ((buf (pop buffers)))
              (when (and (buffer-live-p buf)
                         (with-current-buffer buf
                           (and (derived-mode-p 'dired-mode)
                                (equal (expand-file-name default-directory)
                                       here))))
                (setq dired-buf buf))))))

      ;; Collect the thumbnails currently in the buffer, in order.
      (dolist (file (dired-image-thumbnail--property-values
                     'original-file-name))
        (when (dired-image-thumbnail--image-p file)
          (push file images))
        (unless source-dir
          (setq source-dir (file-name-directory file))))

      ;; Get source-dir from dired buffer if available
      (when (and dired-buf (buffer-live-p dired-buf))
        (with-current-buffer dired-buf
          (unless source-dir
            (setq source-dir dired-image-thumbnail--source-dir))))

      (when images
        (setq dired-image-thumbnail--all-images (nreverse images))
        (setq dired-image-thumbnail--current-images dired-image-thumbnail--all-images)
        (dired-image-thumbnail--rebuild-image-index)
        (setq dired-image-thumbnail--resize-pending nil)
        (clrhash dired-image-thumbnail--thumb-queued)
        (setq dired-image-thumbnail--dired-buffer dired-buf)
        (setq dired-image-thumbnail--source-dir (or source-dir default-directory))
        (setq dired-image-thumbnail--sort-by dired-image-thumbnail-sort-by)
        (setq dired-image-thumbnail--sort-order dired-image-thumbnail-sort-order)
        ;; Restore any quality, sort and filter choices remembered
        ;; for this directory (covers plain `image-dired' entry; the
        ;; `dired-image-thumbnail' command applies them explicitly).
        (dired-image-thumbnail--apply-dir-settings)))))

(defun dired-image-thumbnail--display-thumbs-advice (&rest _)
  "Initialize the thumbnail buffer after `image-dired-display-thumbs'.
`image-dired-display-thumbs' leaves the associated Dired buffer
current, so switch to the thumbnail buffer explicitly."
  (when-let ((buf (get-buffer image-dired-thumbnail-buffer)))
    (with-current-buffer buf
      (when (derived-mode-p 'image-dired-thumbnail-mode)
        (dired-image-thumbnail--initialize-buffer)))))

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
              (count-info (let ((pos (and file
                                          (gethash file
                                                   dired-image-thumbnail--image-index))))
                            (if pos
                                (format "%d/%d"
                                        (1+ pos)
                                        (length dired-image-thumbnail--current-images))
                              image-count)))
             (rel-name (dired-image-thumbnail--relative-name file))
             (dir (dired-image-thumbnail--format-directory file))
             (size (dired-image-thumbnail--format-file-size file))
             (dimensions (dired-image-thumbnail--format-image-dimensions file)))
        (concat
         " "
         (if (> marked-count 0)
             (propertize (format "[%d marked] " marked-count)
                         'face 'dired-image-thumbnail-header-info)
           "")
         (propertize dir 'face 'dired-image-thumbnail-header-info)
         " "
         (propertize rel-name 'face 'dired-image-thumbnail-header-info)
         " "
         (propertize count-info 'face 'dired-image-thumbnail-header-info)
         " "
         (propertize size 'face 'dired-image-thumbnail-header-info)
         " "
         (propertize dimensions 'face 'dired-image-thumbnail-header-info)
         " "
         (propertize sort-info 'face 'dired-image-thumbnail-header-info)
         (if (string-empty-p filter-info)
             ""
           (propertize (format " %s" filter-info)
                       'face 'dired-image-thumbnail-header-info))))
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

(defun dired-image-thumbnail--thumbnails-busy-p ()
  "Return non-nil while image-dired has queued or running thumbnail jobs."
  (or image-dired-queue (> image-dired-queue-active-jobs 0)))

(defun dired-image-thumbnail--square-thumb-name (file)
  "Return the cache path of the square-cropped variant of FILE's thumbnail."
  (concat (image-dired-thumb-name file) ".square"))

(defun dired-image-thumbnail--square-thumb-stale-p (file)
  "Return non-nil if the square variant of FILE's thumbnail is missing
or older than the natural thumbnail it is derived from."
  (let ((natural (image-dired-thumb-name file))
        (square (dired-image-thumbnail--square-thumb-name file)))
    (or (not (file-exists-p square))
        (not (file-exists-p natural))
        (file-newer-than-file-p natural square))))

(defun dired-image-thumbnail--derive-square-thumb (file)
  "Create the square-cropped variant of FILE's thumbnail.
The variant is a copy of the natural cached thumbnail, center-cropped
in place; the natural file itself is never modified, so toggling
between square and natural thumbnails is instantaneous."
  (let ((natural (image-dired-thumb-name file))
        (square (dired-image-thumbnail--square-thumb-name file)))
    (when (file-exists-p natural)
      (copy-file natural square t)
      (dired-image-thumbnail--crop-thumb-to-square square))))

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
`dired-image-thumbnail-current-thumbnail' (default -5, i.e. five
pixels drawn inside the thumbnail so the grid never re-flows), so
customising the face adjusts the thickness.  The colour is the
explicit :color of that :box when set, so customising the face
still works; otherwise the background colour of the `highlight'
face is used, so the outline matches the active theme -- the
theme's own highlight, just thicker.  Falls back to orange on
light backgrounds and yellow on dark ones when no usable colour
is available."
  (let* ((box (face-attribute 'dired-image-thumbnail-current-thumbnail :box))
         (width (let ((w (and (consp box) (plist-get box :line-width))))
                  (cond ((numberp w) w)
                        ((consp w) w)
                        (t -5))))
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
                    (dired-image-thumbnail--cursor-remap)))
        ;; Copy before deleting so the global `face-remapping-alist'
        ;; (which may not yet have a buffer-local binding here) is never
        ;; destructively modified by `assq-delete-all'.
        (base (copy-sequence face-remapping-alist)))
    (setq-local face-remapping-alist
                (if remap
                    (cons remap (assq-delete-all 'cursor base))
                  (assq-delete-all 'cursor base)))))

(defun dired-image-thumbnail--ensure-thumb-geometry ()
  "Repair nil thumbnail geometry variables buffer-locally.
These can be nil if they were bound to nil before image-dired
loaded (the defcustom does not repair a non-void nil).
image-dired's own line-up functions use them in arithmetic
unguarded (e.g. (* 2 image-dired-thumb-relief) in
`image-dired-line-up-dynamic'), which signals
`wrong-type-argument' -- so repair them before any thumbnail work."
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
    (setq-local image-dired-thumbs-per-row 3)))

(cl-defstruct (dired-image-thumbnail--work-state
               (:constructor dired-image-thumbnail--make-work-state))
  "Accumulator for thumbnail creation/cropping work during a refresh."
  (needed 0)
  (done 0)
  (queued 0)
  (progress nil))

(defun dired-image-thumbnail--work-report (state)
  "Record one finished unit of thumbnail work in STATE.
An explicit running counter is passed to `progress-reporter-update':
relying on the nil-increment behaviour breaks in Emacs 30 once the
value reaches max-value (nil is then passed through to
`progress-reporter-do-update', signalling `wrong-type-argument')."
  (when (dired-image-thumbnail--work-state-progress state)
    (progress-reporter-update
     (dired-image-thumbnail--work-state-progress state)
     (cl-incf (dired-image-thumbnail--work-state-done state)))))

(defun dired-image-thumbnail--count-thumbnail-work (state)
  "Count the thumbnail work needed in STATE and start a progress reporter.
Creation and cropping are counted as separate items so the total
matches the number of progress updates exactly.  The reporter is
created only when there is actual work."
  (dolist (file dired-image-thumbnail--current-images)
    (when (not (file-exists-p (image-dired-thumb-name file)))
      (cl-incf (dired-image-thumbnail--work-state-needed state)))
    (when (and dired-image-thumbnail-square-thumbnails
               (dired-image-thumbnail--square-thumb-stale-p file))
      (cl-incf (dired-image-thumbnail--work-state-needed state))))
  (let ((needed (dired-image-thumbnail--work-state-needed state)))
    (when (> needed 0)
      (setf (dired-image-thumbnail--work-state-progress state)
            (make-progress-reporter
             (format "Generating %d thumbnail%s..." needed
                     (if (= needed 1) "" "s"))
             0 needed)))))

(defun dired-image-thumbnail--queue-missing-thumbnails (state)
  "Queue generation of missing natural thumbnails, reporting work in STATE.
Files whose creation failed repeatedly are skipped so a permanently
broken image cannot cause an endless queue/refresh cycle.  The retry
budget is restored as soon as a thumbnail exists, so an image that was
rewritten externally (for example by `transmute') can always be
regenerated again.  Files are queued at most once per cycle (see
`dired-image-thumbnail--thumb-queued'), so progressive refreshes never
duplicate queued jobs."
  (dolist (file dired-image-thumbnail--current-images)
    (let ((thumb-file (image-dired-thumb-name file)))
      (if (file-exists-p thumb-file)
          (remhash file dired-image-thumbnail--thumb-attempts)
        (when (and (< (gethash file dired-image-thumbnail--thumb-attempts 0) 3)
                   (not (gethash file dired-image-thumbnail--thumb-queued)))
          (image-dired-create-thumb file thumb-file)
          (puthash file t dired-image-thumbnail--thumb-queued)
          (puthash file
                   (1+ (gethash file dired-image-thumbnail--thumb-attempts 0))
                   dired-image-thumbnail--thumb-attempts)
          (cl-incf (dired-image-thumbnail--work-state-queued state))
          (dired-image-thumbnail--work-report state))))))

(defun dired-image-thumbnail--derive-square-thumbnails (state)
  "Derive square variants for thumbnails already on disk, reporting STATE.
The natural cached files are never modified, so toggling between
square and natural only switches which cached file set is displayed
-- no regeneration, no blank buffer."
  (when dired-image-thumbnail-square-thumbnails
    (dolist (file dired-image-thumbnail--current-images)
      (when (and (file-exists-p (image-dired-thumb-name file))
                 (dired-image-thumbnail--square-thumb-stale-p file))
        (dired-image-thumbnail--derive-square-thumb file)
        (dired-image-thumbnail--work-report state)))))

(defun dired-image-thumbnail--insert-thumbnails ()
  "Insert the thumbnails for the active display mode.
Thumbnails that are still generating get a gray placeholder with
the same text properties, so the grid and point stay valid until
the poll refresh swaps in the real images."
  (dolist (file dired-image-thumbnail--current-images)
    (let ((thumb-file (if dired-image-thumbnail-square-thumbnails
                          (dired-image-thumbnail--square-thumb-name file)
                        (image-dired-thumb-name file))))
      (image-dired-insert-thumbnail
       (if (file-exists-p thumb-file)
           thumb-file
         (dired-image-thumbnail--placeholder-file))
       file dired-image-thumbnail--dired-buffer))))

(defun dired-image-thumbnail--apply-display-size ()
  "Apply `dired-image-thumbnail--display-size' and queue cache regeneration.
Cached thumb files are shown at their natural size, so when the
display size changed since they were generated, the stale-sized
files are regenerated in the background (see
`dired-image-thumbnail--regenerate-thumbs').  The cached thumbnails
currently on disk are displayed right away, so the buffer is never
blank while the new sizes are made."
  (let ((standard-size image-dired-thumb-size))
    (when (and dired-image-thumbnail--display-size
               (numberp standard-size)
               (/= dired-image-thumbnail--display-size standard-size))
      (setq-local image-dired-thumb-size dired-image-thumbnail--display-size))
    (when (and (numberp image-dired-thumb-size)
               (numberp dired-image-thumbnail--thumbs-generated-at)
               (/= image-dired-thumb-size
                   dired-image-thumbnail--thumbs-generated-at))
      (dired-image-thumbnail--queue-thumb-regeneration))))

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
      (dired-image-thumbnail--rebuild-image-index)
      (dired-image-thumbnail--ensure-thumb-geometry)
      (dired-image-thumbnail--apply-display-size)
      ;; Populate the buffer: queue missing thumbnails (generated
      ;; asynchronously, so the display never blocks on them), derive
      ;; square variants for those already on disk, and insert everything
      ;; with placeholders for the ones still generating.
      (let ((work (dired-image-thumbnail--make-work-state)))
        (dired-image-thumbnail--count-thumbnail-work work)
        (dired-image-thumbnail--queue-missing-thumbnails work)
        (dired-image-thumbnail--derive-square-thumbnails work)
        (dired-image-thumbnail--insert-thumbnails)
        ;; Forget queued thumbnails that have already appeared, so the
        ;; queue poll only refreshes for genuinely new progress.
        (dired-image-thumbnail--prune-thumb-queued)
        (when (dired-image-thumbnail--work-state-progress work)
          (progress-reporter-done
           (dired-image-thumbnail--work-state-progress work)))
        ;; Once the creation queue has drained a re-refresh inserts the
        ;; thumbnails that finished generating.
        (when (> (dired-image-thumbnail--work-state-queued work) 0)
          (dired-image-thumbnail--arm-queue-poll)))
      ;; Ensure the quality segment is in the mode line (also picks up
      ;; reloaded code in a live session without recreating the buffer;
      ;; a no-op once present).
      (dired-image-thumbnail--setup-mode-line)
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
          (goto-char (or (dired-image-thumbnail--position-of-file current-file)
                         (point-min)))
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
    ;; Clearing the cache is an explicit request to regenerate
    ;; everything, so also restore the retry budget of images whose
    ;; thumbnail creation previously failed.
    (dolist (file dired-image-thumbnail--current-images)
      (remhash file dired-image-thumbnail--thumb-attempts))
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
  "Invalidate caches for the specific list of FILES.
FILES should be a list of expanded file names.  The dimension cache is
cleared and the thumbnail retry budget is restored, so images rewritten
externally are regenerated even if their thumbnails previously failed."
  (let ((files (mapcar #'expand-file-name files)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'image-dired-thumbnail-mode)
                   (bound-and-true-p dired-image-thumbnail--dimension-cache))
          (dolist (f files)
            (remhash f dired-image-thumbnail--dimension-cache)
            (remhash f dired-image-thumbnail--dimension-pending)
            (remhash f dired-image-thumbnail--thumb-attempts))
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

(defconst dired-image-thumbnail--sort-labels
  '((dired . "Dired order")
    (name . "name")
    (date . "date")
    (size . "size"))
  "Display labels for the sort criteria.")

(defun dired-image-thumbnail--set-sort-by (criteria)
  "Sort thumbnails by CRITERIA, persist and refresh."
  (setq dired-image-thumbnail--sort-by criteria)
  (dired-image-thumbnail--save-dir-settings
   (list (cons 'dired-image-thumbnail-sort-by criteria)))
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sorted by %s"
           (or (cdr (assq criteria dired-image-thumbnail--sort-labels))
               (symbol-name criteria))))

(defun dired-image-thumbnail-sort-by-dired ()
  "Sort thumbnails by Dired buffer order."
  (interactive)
  (dired-image-thumbnail--set-sort-by 'dired))

(defun dired-image-thumbnail-sort-by-name ()
  "Sort thumbnails by name."
  (interactive)
  (dired-image-thumbnail--set-sort-by 'name))

(defun dired-image-thumbnail-sort-by-date ()
  "Sort thumbnails by date."
  (interactive)
  (dired-image-thumbnail--set-sort-by 'date))

(defun dired-image-thumbnail-sort-by-size ()
  "Sort thumbnails by size."
  (interactive)
  (dired-image-thumbnail--set-sort-by 'size))

(defun dired-image-thumbnail--set-sort-order (order)
  "Set the sort order to ORDER, persist and refresh."
  (setq dired-image-thumbnail--sort-order order)
  (dired-image-thumbnail--save-dir-settings
   (list (cons 'dired-image-thumbnail-sort-order order)))
  (dired-image-thumbnail--apply-sort-and-filter)
  (message "Sort order: %s" order))

(defun dired-image-thumbnail-sort-reverse ()
  "Reverse current sort order."
  (interactive)
  (dired-image-thumbnail--set-sort-order
   (if (eq dired-image-thumbnail--sort-order 'ascending)
       'descending
     'ascending)))

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

(defun dired-image-thumbnail--apply-filter ()
  "Persist and apply the current filter settings."
  (dired-image-thumbnail--save-current-filter)
  (dired-image-thumbnail--apply-sort-and-filter))

(defun dired-image-thumbnail-filter-by-name (regexp)
  "Filter thumbnails by name matching REGEXP."
  (interactive "sFilter by name (regexp): ")
  (setq dired-image-thumbnail--filter-name
        (if (string-empty-p regexp) nil regexp))
  (dired-image-thumbnail--apply-filter)
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
  (dired-image-thumbnail--apply-filter)
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
  (dired-image-thumbnail--apply-filter)
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

(defun dired-image-thumbnail--base-thumb-size ()
  "Return image-dired's own thumbnail size."
  (or (and (fboundp 'image-dired--thumb-size)
           (image-dired--thumb-size))
      (and (numberp image-dired-thumb-size)
           image-dired-thumb-size)
      128))

(defun dired-image-thumbnail--resize-display ()
  "Apply a display-size change without flashing placeholder thumbnails.
If the cached thumbnails are already at the requested size a normal
refresh is done.  Otherwise the current thumbnails stay on screen and
are regenerated in the background, swapping in the new size only once
the new-size thumbnails are ready."
  (if (or (null dired-image-thumbnail--all-images)
          (null dired-image-thumbnail--thumbs-generated-at)
          (= dired-image-thumbnail--display-size
             dired-image-thumbnail--thumbs-generated-at))
      (dired-image-thumbnail-refresh)
    (message "Regenerating %d thumbnails at %dpx..."
             (length dired-image-thumbnail--current-images)
             dired-image-thumbnail--display-size)
    (dired-image-thumbnail--queue-thumb-regeneration)))

(defun dired-image-thumbnail-increase-size ()
  "Increase thumbnail display size.
When size exceeds the cached thumbnail size, images are scaled from
the original files for crisp display (slower but higher quality)."
  (interactive)
  (let* ((base (dired-image-thumbnail--base-thumb-size))
         (current (or dired-image-thumbnail--display-size base)))
    (setq dired-image-thumbnail--display-size (min 512 (+ current 32)))
    (dired-image-thumbnail--resize-display)
    (if (> dired-image-thumbnail--display-size base)
        (message "Thumbnail size: %d (using original images for quality)"
                 dired-image-thumbnail--display-size)
      (message "Thumbnail size: %d" dired-image-thumbnail--display-size))))

(defun dired-image-thumbnail-decrease-size ()
  "Decrease thumbnail display size."
  (interactive)
  (let* ((base (dired-image-thumbnail--base-thumb-size))
         (current (or dired-image-thumbnail--display-size base)))
    (setq dired-image-thumbnail--display-size (max 32 (- current 32)))
    (dired-image-thumbnail--resize-display)
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
pipeline.  The cached marked count is invalidated and the header
line refreshed, since vanilla marking updates marks at point
without running the bulk update the count cache relies on."
  (setq dired-image-thumbnail--marked-count nil)
  (sit-for 0)
  (when (dired-image-thumbnail--auto-display-p)
    (dired-image-thumbnail--display-this))
  (image-dired--update-header-line))

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

(defun dired-image-thumbnail--drop-from-state (file)
  "Drop FILE from the current and all thumbnail image lists."
  (setq dired-image-thumbnail--current-images
        (remove file dired-image-thumbnail--current-images))
  (setq dired-image-thumbnail--all-images
        (remove file dired-image-thumbnail--all-images))
  (dired-image-thumbnail--rebuild-image-index))

(defun dired-image-thumbnail--revert-dired-buffer ()
  "Revert the associated Dired buffer, if any."
  (when (and dired-image-thumbnail--dired-buffer
             (buffer-live-p dired-image-thumbnail--dired-buffer))
    (with-current-buffer dired-image-thumbnail--dired-buffer
      (revert-buffer))))

(defun dired-image-thumbnail--trash-file (file)
  "Move FILE to the system trash."
  (let ((delete-by-moving-to-trash t))
    (delete-file file t)))

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
            (dired-image-thumbnail--drop-from-state file))))
      ;; Refresh dired buffer
      (dired-image-thumbnail--revert-dired-buffer)
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
        (dired-image-thumbnail--trash-file file)
        (dired-image-thumbnail--drop-from-state file))
      ;; Refresh dired buffer
      (dired-image-thumbnail--revert-dired-buffer)
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
        (let ((index (or (gethash file dired-image-thumbnail--image-index)
                         0)))
          (dired-image-thumbnail--trash-file file)
          (dired-image-thumbnail--drop-from-state file)
          ;; Refresh dired buffer
          (dired-image-thumbnail--revert-dired-buffer)
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
  (setq-local dired-image-thumbnail-square-thumbnails
              (not dired-image-thumbnail-square-thumbnails))
  (dired-image-thumbnail-refresh)
  (message "Square thumbnails: %s"
            (if dired-image-thumbnail-square-thumbnails "ON" "OFF")))

;;; Main entry point

(defun dired-image-thumbnail--find-subdirs (directory &optional max-depth)
  "Return a list of all subdirectories under DIRECTORY.
Does not include DIRECTORY itself.  Optional MAX-DEPTH limits recursion
\(nil means unlimited, 1 means direct children only).  Symbolic links
to directories are included but never followed, so cycles cannot
cause an infinite traversal."
  (let ((subdirs nil)
        (visited (make-hash-table :test 'equal))
        (dirs-to-process (list (cons directory 0))))
    (puthash (file-truename directory) t visited)
    (while dirs-to-process
      (let* ((item (pop dirs-to-process))
             (current-dir (car item))
             (current-depth (cdr item)))
        ;; The "^[^.]" match already excludes "." and ".." (and other dotfiles).
        (dolist (file (directory-files current-dir t "^[^.]" t))
          (when (file-directory-p file)
            (push file subdirs)
            (unless (file-symlink-p file)
              (let ((truename (file-truename file)))
                (unless (gethash truename visited)
                  (puthash truename t visited)
                  ;; Only recurse if we haven't hit max depth
                  (when (or (null max-depth) (< (1+ current-depth) max-depth))
                    (push (cons file (1+ current-depth)) dirs-to-process)))))))))
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
SUBDIRS should be a list of directory paths.  Returns the number
of subdirectories actually inserted; those already present are
left untouched."
  (let ((inserted 0))
    (dolist (subdir subdirs)
      (let ((subdir-path (file-name-as-directory subdir)))
        (condition-case err
            (save-excursion
              ;; Check if this subdir is already inserted.  Dired
              ;; headers are the plain directory name with no trailing
              ;; slash, so compare against `directory-file-name'.
              (goto-char (point-min))
              (unless (re-search-forward
                       (concat "^  "
                               (regexp-quote (directory-file-name subdir-path))
                               ":$")
                       nil t)
                (goto-char (point-max))
                (dired-insert-subdir subdir-path)
                (setq inserted (1+ inserted))))
          (error
           (message "Could not insert subdir %s: %s" subdir-path err)))))
    inserted))

;;;###autoload
(defun dired-image-thumbnail ()
  "Display thumbnails for image files in current dired buffer.
If files are marked, show thumbnails for marked images only.
Otherwise, show thumbnails for all images visible in the dired buffer.

The display quality, sort order and filter remembered for this
directory in its `.dir-locals.el' (see
`dired-image-thumbnail-save-dir-settings') are restored.

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
        (dired-image-thumbnail--rebuild-image-index)
        (setq dired-image-thumbnail--resize-pending nil)
        (clrhash dired-image-thumbnail--thumb-queued)
        (setq dired-image-thumbnail--dired-buffer dired-buf)
        (setq dired-image-thumbnail--source-dir source-dir)
        (setq dired-image-thumbnail--recursive recursive)
        (setq dired-image-thumbnail--filter-name nil)
        (setq dired-image-thumbnail--filter-size-min nil)
        (setq dired-image-thumbnail--filter-size-max nil)
        ;; Restore the quality, sort and filter choices remembered
        ;; for this directory (if any).
        (dired-image-thumbnail--apply-dir-settings))
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
        (when (dired-image-thumbnail--auto-display-p)
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
            (message "Inserted %d subdirectories"
                     (dired-image-thumbnail--insert-subdirs subdirs)))
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
            (message "Inserted %d subdirectories"
                     (dired-image-thumbnail--insert-subdirs subdirs)))
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

(defun dired-image-thumbnail--current-quality ()
  "Return the effective display quality for the current buffer.
This is the buffer-local `dired-image-thumbnail--display-quality'
when set (per-directory choice from `.dir-locals.el' or the last
`dired-image-thumbnail-select-display-quality' selection),
otherwise the global `dired-image-thumbnail-display-quality'."
  (or dired-image-thumbnail--display-quality
      dired-image-thumbnail-display-quality))

(defvar dired-image-thumbnail--mode-line-quality
  '(:eval (dired-image-thumbnail--mode-line-quality-string))
  "Mode-line construct showing the current display quality.
Appended buffer-locally to `mode-line-format' in thumbnail buffers
by `dired-image-thumbnail--setup-mode-line'.")
;; Mode-line symbols only get their `:eval' forms processed when
;; marked risky (otherwise Emacs silently ignores them, so the
;; segment would never display).  Same as built-in
;; `project-mode-line-format'.  The variable is internal and never
;; set from file/dir locals, so this changes no safety behaviour.
(put 'dired-image-thumbnail--mode-line-quality 'risky-local-variable t)

(defun dired-image-thumbnail--mode-line-quality-string ()
  "Return the mode-line quality segment, e.g. \"[Q4 faster]\".
Reflects `dired-image-thumbnail--current-quality', so it follows
`Q' changes and per-directory settings automatically.  Never
signals an error, since mode-line evaluation must not fail."
  (condition-case nil
      (let ((quality (dired-image-thumbnail--current-quality)))
        (propertize (format "[Q%s %s]"
                            (or (nth 1 (assq quality
                                             dired-image-thumbnail--quality-choices))
                                "?")
                            quality)
                    'face 'mode-line-emphasis))
    (error "")))

(defun dired-image-thumbnail--setup-mode-line ()
  "Show the display quality in the thumbnail buffer mode line.
Inserts `dired-image-thumbnail--mode-line-quality' right after the
buffer identification (so it stays visible instead of being cut
off at the row end in narrow windows), or appends it when the
format has no buffer identification.  Runs only once per buffer;
the segment re-evaluates on every redisplay, so it stays in step
without further updates.  Safe to call repeatedly: besides the
mode hook it also runs on every refresh, so reloaded code takes
effect without recreating the buffer."
  (when (and (derived-mode-p 'image-dired-thumbnail-mode)
             (listp mode-line-format))
    (unless (memq 'dired-image-thumbnail--mode-line-quality
                  mode-line-format)
      (setq-local mode-line-format
                  (let ((new nil) (inserted nil))
                    (dolist (el mode-line-format)
                      (push el new)
                      (when (and (not inserted)
                                 (eq el 'mode-line-buffer-identification))
                        (push 'dired-image-thumbnail--mode-line-quality new)
                        (setq inserted t)))
                    (unless inserted
                      (push 'dired-image-thumbnail--mode-line-quality new))
                    (nreverse new))))))

(defun dired-image-thumbnail--quality-scale ()
  "Return the scale factor for the current display quality."
  (pcase (dired-image-thumbnail--current-quality)
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
                          (or (file-attribute-modification-time
                               (file-attributes file))
                              0))))
           ".jpg")
   (dired-image-thumbnail--preview-dir)))

(defun dired-image-thumbnail--preview-quality ()
  "Return the JPEG encode quality for the current display quality.
Lower display qualities use lower encode quality, so preview files
are smaller and quicker to load and decode."
  (pcase (dired-image-thumbnail--current-quality)
    ('fast 60)
    ('faster 40)
    ('draft 25)
    (_ 50)))

(defun dired-image-thumbnail--process-succeeded-p (status)
  "Return non-nil if process STATUS indicates success."
  (and (integerp status) (zerop status)))

(defun dired-image-thumbnail--make-preview (file width height)
  "Create a preview of FILE at WIDTH x HEIGHT pixels.
Returns the path to the preview file.
For JPEGs, uses djpeg/cjpeg with DCT scaling (very fast).
For other formats, uses magick/convert with -thumbnail.
The encode quality follows `dired-image-thumbnail-display-quality',
so lower quality modes produce smaller, quicker-loading files.
The preview is written to a temporary file and only published
after the encoding succeeds and the result decodes, so a failed
or partial preview is never cached."
  (let ((preview-path (dired-image-thumbnail--preview-path file width)))
    (unless (file-exists-p preview-path)
      (let* ((expanded (expand-file-name file))
             (quality (number-to-string (dired-image-thumbnail--preview-quality)))
             (out-temp (make-temp-file
                        (expand-file-name "preview-out-"
                                          (dired-image-thumbnail--preview-dir))
                        nil ".jpg"))
             (ok nil))
        (unwind-protect
            (progn
              (if (and (dired-image-thumbnail--jpeg-p file)
                       (executable-find "djpeg")
                       (executable-find "cjpeg"))
                  ;; Fast path: djpeg DCT scaling + cjpeg (skips full
                  ;; decode).  An intermediate file is used instead of a
                  ;; shell pipeline so this works portably.
                  (let ((scale-str (dired-image-thumbnail--djpeg-scale
                                    (dired-image-thumbnail--quality-scale)))
                        (decoded (make-temp-file "dired-image-preview-djpeg-")))
                    (unwind-protect
                        (when (dired-image-thumbnail--process-succeeded-p
                               (call-process "djpeg" nil (list :file decoded) nil
                                             "-scale" scale-str expanded))
                          (setq ok
                                (dired-image-thumbnail--process-succeeded-p
                                 (call-process "cjpeg" nil (list :file out-temp) nil
                                               "-quality" quality decoded))))
                      (when (file-exists-p decoded)
                        (delete-file decoded))))
                ;; Fallback: magick/convert -thumbnail
                (let ((magick (or (executable-find "magick")
                                  (executable-find "convert"))))
                  (when magick
                    (setq ok
                          (dired-image-thumbnail--process-succeeded-p
                           (call-process magick nil nil nil
                                         expanded
                                         "-thumbnail" (format "%dx%d" width height)
                                         "-quality" quality
                                         out-temp))))))
              (when (and ok
                         (file-exists-p out-temp)
                         (ignore-errors
                           (image-size (create-image out-temp) t)))
                (rename-file out-temp preview-path t)))
          (when (file-exists-p out-temp)
            (delete-file out-temp)))))
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

(defun dired-image-thumbnail--fit-image-spec (display-file win-width win-height)
  "Create an image spec for DISPLAY-FILE scaled to fit WIN-WIDTH x WIN-HEIGHT.
Preserves the aspect ratio, allowing both upscaling and
downscaling, so the preview always fills the window as much as
possible.  Falls back to `:max-width'/`:max-height' (downscale
only) when the native size cannot be determined."
  (let ((native (ignore-errors (image-size (create-image display-file) t))))
    (if (and native
             (consp native)
             (numberp (car native)) (numberp (cdr native))
             (> (car native) 0) (> (cdr native) 0)
             (numberp win-width) (numberp win-height)
             (> win-width 0) (> win-height 0))
        (let* ((iw (float (car native)))
               (ih (float (cdr native)))
               (s (min (/ (float win-width) iw)
                       (/ (float win-height) ih))))
          (if (and (numberp s) (> s 0) (not (= s 1.0)))
              (create-image display-file nil nil
                            :width (max 1 (round (* iw s)))
                            :height (max 1 (round (* ih s))))
            (create-image display-file nil nil
                          :max-width win-width
                          :max-height win-height)))
      (create-image display-file nil nil
                    :max-width win-width
                    :max-height win-height))))

(defun dired-image-thumbnail--display-image-fast (file)
  "Display FILE scaled according to `dired-image-thumbnail-display-quality'.
For `high' quality, loads the file directly with window-fitting constraints.
For lower qualities, produces a small preview via an external tool so that
Emacs never decodes the full image.  In both cases the displayed
image is scaled (up or down) to fit the display window, preserving
the aspect ratio."
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
          ;; Fit the (possibly small) preview to the window, scaling
          ;; up as well as down so e.g. quarter-size `faster'
          ;; previews still fill the display window.
          (img (dired-image-thumbnail--fit-image-spec
                display-file win-width win-height)))
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

(defvar dired-image-thumbnail--thumb-regen-timer nil
  "Idle timer for the deferred thumbnail-cache regeneration.")

(defvar dired-image-thumbnail--thumb-queue-poll-timer nil
  "Timer polling the thumbnail creation queue.
While the queue is busy the buffer is refreshed as thumbnails
appear, and once it drains a final refresh is done, so generated
thumbnails show up without ever blocking the display.")

(defun dired-image-thumbnail--arm-queue-poll ()
  "Poll the thumbnail creation queue, refreshing as thumbnails appear."
  (when dired-image-thumbnail--thumb-queue-poll-timer
    (cancel-timer dired-image-thumbnail--thumb-queue-poll-timer))
  (setq dired-image-thumbnail--thumb-queue-poll-timer
        (run-with-timer 0.3 0.3
                        #'dired-image-thumbnail--poll-thumb-queue)))

(defun dired-image-thumbnail--thumb-queued-appeared-p ()
  "Return non-nil if any queued thumbnail has appeared on disk."
  (catch 'appeared
    (maphash (lambda (file _)
               (when (file-exists-p (image-dired-thumb-name file))
                 (throw 'appeared t)))
             dired-image-thumbnail--thumb-queued)
    nil))

(defun dired-image-thumbnail--prune-thumb-queued ()
  "Forget queued thumbnails that now exist on disk."
  (let (appeared)
    (maphash (lambda (file _)
               (when (file-exists-p (image-dired-thumb-name file))
                 (push file appeared)))
             dired-image-thumbnail--thumb-queued)
    (dolist (file appeared)
      (remhash file dired-image-thumbnail--thumb-queued))))

(defun dired-image-thumbnail--poll-thumb-queue ()
  "Poll the thumbnail creation queue, refreshing as thumbnails appear.
While jobs are running the buffer is refreshed whenever a queued
thumbnail has appeared on disk, so thumbnails \"come in\" instead of
leaving a grid of placeholders until the very end.  During a deferred
resize (`dired-image-thumbnail--resize-pending') only the final
refresh happens, so the current thumbnails stay on screen until the
new size is ready.  Once the queue drains a final refresh is done."
  (let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
    (if (or image-dired-queue (> image-dired-queue-active-jobs 0))
        (when (and thumb-buf (buffer-live-p thumb-buf))
          (with-current-buffer thumb-buf
            (when (and (derived-mode-p 'image-dired-thumbnail-mode)
                       (null dired-image-thumbnail--resize-pending)
                       (dired-image-thumbnail--thumb-queued-appeared-p))
              (dired-image-thumbnail-refresh))))
      (cancel-timer dired-image-thumbnail--thumb-queue-poll-timer)
      (setq dired-image-thumbnail--thumb-queue-poll-timer nil)
      (when (and thumb-buf (buffer-live-p thumb-buf))
        (with-current-buffer thumb-buf
          (when (derived-mode-p 'image-dired-thumbnail-mode)
            (setq dired-image-thumbnail--resize-pending nil)
            (clrhash dired-image-thumbnail--thumb-queued)
            (dired-image-thumbnail-refresh)))))))

(defun dired-image-thumbnail--placeholder-file ()
  "Return the path of a gray placeholder thumbnail image, creating it if needed.
The placeholder is sized to the current thumbnail size and is
inserted for images whose thumbnails are still being generated, so
the grid and point stay valid instead of showing a blank buffer."
  (let* ((size (or (and (numberp image-dired-thumb-size) image-dired-thumb-size)
                   128))
         (file (expand-file-name (format "placeholder-%d.xpm" size)
                                 (dired-image-thumbnail--preview-dir))))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "/* XPM */\nstatic char *p[] = {\n")
        (insert (format "\"%d %d 2 1\",\n" size size))
        (insert "\". c #8a8a8a\",\n")
        (insert "\"o c #5a5a5a\",\n")
        (dotimes (y size)
          (let ((row (cond ((or (= y 0) (= y (1- size)))
                            (make-string size ?o))
                           (t (concat "o" (make-string (- size 2) ?.) "o")))))
            (insert (format "\"%s\"%s\n"
                            row
                            (if (= y (1- size)) "" ",")))))
        (insert "};\n")))
    file))

(defun dired-image-thumbnail--queue-thumb-regeneration ()
  "Queue a background regeneration of thumbnails at the display size.
The stale-sized cached thumbnails stay visible until the
regeneration replaces them, so the thumbnail buffer is never blank
while the new sizes are generated."
  (when dired-image-thumbnail--thumb-regen-timer
    (cancel-timer dired-image-thumbnail--thumb-regen-timer))
  (setq dired-image-thumbnail--thumb-regen-timer
        (run-with-idle-timer 1.0 nil
                             #'dired-image-thumbnail--regenerate-thumbs)))

(defun dired-image-thumbnail--regenerate-thumbs ()
  "Regenerate cached thumbnails at `dired-image-thumbnail--display-size'.
The stale-sized cached files are replaced in the background; the
buffer keeps showing the current thumbnails until the new ones are
ready, so no placeholder thumbnails flash up during a resize."
  (setq dired-image-thumbnail--thumb-regen-timer nil)
  (let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
    (when (and thumb-buf (buffer-live-p thumb-buf))
      (with-current-buffer thumb-buf
        (when (and (derived-mode-p 'image-dired-thumbnail-mode)
                   (numberp dired-image-thumbnail--display-size)
                   (numberp dired-image-thumbnail--thumbs-generated-at)
                   (/= dired-image-thumbnail--display-size
                       dired-image-thumbnail--thumbs-generated-at))
          (if (dired-image-thumbnail--thumbnails-busy-p)
              (dired-image-thumbnail--queue-thumb-regeneration)
            ;; Generate at the new size.  The old (already decoded)
            ;; thumbnails stay on screen until the queue drains, at which
            ;; point the poll refresh swaps them for the new size.
            (setq-local image-dired-thumb-size
                        dired-image-thumbnail--display-size)
            (dolist (file dired-image-thumbnail--current-images)
              (let ((thumb-file (image-dired-thumb-name file)))
                (when (file-exists-p thumb-file)
                  (delete-file thumb-file)))
              (let ((square-file (dired-image-thumbnail--square-thumb-name file)))
                (when (file-exists-p square-file)
                  (delete-file square-file))))
            (setq dired-image-thumbnail--thumbs-generated-at
                  dired-image-thumbnail--display-size)
            (setq dired-image-thumbnail--resize-pending t)
            (dolist (file dired-image-thumbnail--current-images)
              (let ((thumb-file (image-dired-thumb-name file)))
                (puthash file t dired-image-thumbnail--thumb-queued)
                (image-dired-create-thumb file thumb-file)))
            (dired-image-thumbnail--arm-queue-poll)))))))

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
                           (gethash file dired-image-thumbnail--image-index)))
                  (offsets (if (eq (dired-image-thumbnail--current-quality) 'draft)
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

(defun dired-image-thumbnail-select-display-quality (&optional session-only)
  "Select the display quality with `completing-read'.
Qualities are numbered from 1 (best) to 5 (fastest); see
`dired-image-thumbnail--quality-choices'.  Type a number or part
of a description.  The choice takes effect immediately and
refreshes the displayed image.
The choice is remembered in the directory's `.dir-locals.el' (see
`dired-image-thumbnail-save-dir-settings'), so revisiting the
directory restores it.  With prefix argument SESSION-ONLY, change
the quality for this session only without writing it to the
directory."
  (interactive "P")
  (let* ((table (append (mapcar (lambda (c) (cons (nth 1 c) (car c)))
                                dired-image-thumbnail--quality-choices)
                        (mapcar (lambda (c) (cons (nth 2 c) (car c)))
                                dired-image-thumbnail--quality-choices)))
         (current (dired-image-thumbnail--current-quality))
         (default (nth 2 (assq current dired-image-thumbnail--quality-choices)))
         (input (completing-read
                 (format "Display quality (current %s): " current)
                 (mapcar #'car table)
                 nil t nil nil default))
         (quality (cdr (assoc input table))))
    (unless quality
      (user-error "Unknown display quality: %s" input))
    (setq dired-image-thumbnail--display-quality quality)
    (unless session-only
      (dired-image-thumbnail--save-dir-settings
       (list (cons 'dired-image-thumbnail-display-quality quality)))))
  (message "Display quality: %s" (dired-image-thumbnail--current-quality))
  (image-dired--update-header-line)
  (force-mode-line-update)
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

(defun dired-image-thumbnail--auto-display-p ()
  "Return the effective auto-display setting.
Uses the thumbnail buffer's buffer-local value when the current
buffer is not itself a thumbnail buffer, so the `F' toggle applies
to commands run from the image display buffer too."
  (if (derived-mode-p 'image-dired-thumbnail-mode)
      dired-image-thumbnail-auto-display-on-navigate
    (let ((buf (get-buffer image-dired-thumbnail-buffer)))
      (if (buffer-live-p buf)
          (buffer-local-value 'dired-image-thumbnail-auto-display-on-navigate buf)
        dired-image-thumbnail-auto-display-on-navigate))))

(defun dired-image-thumbnail-toggle-auto-display ()
  "Toggle automatic display of the full-size image while navigating.
When enabled, moving to the next/previous thumbnail updates the image
display buffer automatically.  When disabled, navigation and marking
only move point, so you can step through, mark or delete many
thumbnails without paying the cost of decoding each image.
The setting is buffer-local, so it does not change the global
default for other thumbnail buffers."
  (interactive)
  (unless dired-image-thumbnail--all-images
    (dired-image-thumbnail--initialize-buffer))
  (setq-local dired-image-thumbnail-auto-display-on-navigate
              (not (dired-image-thumbnail--auto-display-p)))
  (dired-image-thumbnail--disable-marking-shows-next)
  (when (dired-image-thumbnail--auto-display-p)
    (dired-image-thumbnail--display-this))
  (message "Follow (auto-display on navigate): %s"
           (if (dired-image-thumbnail--auto-display-p) "ON" "OFF")))

(defun dired-image-thumbnail-next-image ()
  "Move to next thumbnail and optionally display full-size image.
When `dired-image-thumbnail-auto-display-on-navigate' is non-nil,
the full-size image is automatically displayed."
  (interactive)
  (image-dired-forward-image)
  (when (dired-image-thumbnail--auto-display-p)
    (dired-image-thumbnail--display-this)))

(defun dired-image-thumbnail-previous-image ()
  "Move to previous thumbnail and optionally display full-size image.
When `dired-image-thumbnail-auto-display-on-navigate' is non-nil,
the full-size image is automatically displayed."
  (interactive)
  (image-dired-backward-image)
  (when (dired-image-thumbnail--auto-display-p)
    (dired-image-thumbnail--display-this)))

(defun dired-image-thumbnail-delete-and-next ()
  "Delete current image file and move to next thumbnail.
This permanently deletes the file from disk and removes its thumbnail."
  (interactive)
  (let ((file-name (image-dired-original-file-name)))
    (when (and file-name
               (or dired-image-thumbnail-auto-accept
                   (y-or-n-p (format "Delete %s? " (file-name-nondirectory file-name)))))
      (dired-image-thumbnail--trash-file file-name)
      (dired-image-thumbnail--drop-from-state file-name)
      (image-dired-delete-char)
      ;; Respect the auto-display setting: when off, just move to the
      ;; next thumbnail without decoding and displaying it.
      (when (and (not (eobp))
                 (dired-image-thumbnail--auto-display-p))
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
    (unless current-file
      (user-error "No image at point"))
    (when (or dired-image-thumbnail-auto-accept
              (y-or-n-p (format "Delete %s? " (file-name-nondirectory current-file))))
      (dired-image-thumbnail--trash-file current-file)
      ;; Update internal lists and remove the thumbnail, locating it by
      ;; file name because point in the display buffer need not match
      ;; point in the thumbnail buffer.
      (when-let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
        (with-current-buffer thumb-buf
          (dired-image-thumbnail--drop-from-state current-file)
          (let ((origin (point))
                (found (dired-image-thumbnail--position-of-file current-file)))
            (if found
                (progn
                  (goto-char found)
                  (image-dired-delete-char)
                  (when (and (not (eobp))
                             (dired-image-thumbnail--auto-display-p))
                    (dired-image-thumbnail--display-this)))
              (goto-char origin)))))
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

(defvar dired-image-thumbnail--display-buffer-entries nil
  "Entries this package installed in `display-buffer-alist'.
Only these exact entries are removed again, so rules added by the
user or by other packages are never touched.")

(defun dired-image-thumbnail-setup-display-buffer ()
  "Configure `display-buffer-alist' rules for thumbnail and image buffers.
Only adds rules when `dired-image-thumbnail-window-layout' is non-nil,
so that `display-buffer' respects the layout for subsequent pop-to-buffer
calls (e.g. when the image window is reused during navigation).

Any rules previously installed by this function are removed first, so it
is idempotent and can be re-run to track changes to
`dired-image-thumbnail-window-layout' / `-window-ratio'."
  ;; Drop exactly the rules we installed earlier so re-running picks up
  ;; current custom values rather than stacking stale entries and
  ;; without clobbering rules installed by the user or other packages.
  (setq display-buffer-alist
        (seq-remove (lambda (entry)
                      (member entry dired-image-thumbnail--display-buffer-entries))
                    display-buffer-alist))
  (setq dired-image-thumbnail--display-buffer-entries nil)
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
                        `(window-height . ,(- 1.0 ratio))))
           (thumb-entry `("\\*image-dired\\*"
                          display-buffer-in-direction
                          (direction . ,thumb-dir)
                          (window . root)
                          ,thumb-size)))
      (push thumb-entry dired-image-thumbnail--display-buffer-entries)
      (add-to-list 'display-buffer-alist thumb-entry)
      (unless (eq layout 'thumb-only)
        (let ((img-entry `("\\*image-dired-display-image\\*"
                           (display-buffer-reuse-window
                            dired-image-thumbnail-display-at-direction)
                           (direction . ,img-dir)
                           (window . root)
                           ,img-size)))
          (push img-entry dired-image-thumbnail--display-buffer-entries)
          (add-to-list 'display-buffer-alist img-entry))))))

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
  ;; Initialize after thumbnails are inserted, so starting from plain
  ;; `image-dired' also picks up the enhanced state.
  (advice-add 'image-dired-display-thumbs :after
              #'dired-image-thumbnail--display-thumbs-advice)
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
  (when dired-image-thumbnail--thumb-regen-timer
    (cancel-timer dired-image-thumbnail--thumb-regen-timer)
    (setq dired-image-thumbnail--thumb-regen-timer nil))
  (when dired-image-thumbnail--thumb-queue-poll-timer
    (cancel-timer dired-image-thumbnail--thumb-queue-poll-timer)
    (setq dired-image-thumbnail--thumb-queue-poll-timer nil))
  (advice-remove 'image-dired-format-properties-string
                 #'dired-image-thumbnail--format-properties-string)
  (advice-remove 'image-dired--thumb-update-marks
                 #'dired-image-thumbnail--invalidate-marked-count)
  (advice-remove 'image-dired-display-image
                 #'dired-image-thumbnail--display-image-no-lock)
  (remove-hook 'image-dired-thumbnail-mode-hook
               #'dired-image-thumbnail--initialize-buffer)
  (advice-remove 'image-dired-display-thumbs
                 #'dired-image-thumbnail--display-thumbs-advice)
  (remove-hook 'kill-emacs-hook #'dired-image-thumbnail-clear-preview-cache)
  ;; Remove the current-thumbnail highlight from any thumbnail buffers.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'image-dired-thumbnail-mode)
        (remove-hook 'post-command-hook
                     #'dired-image-thumbnail--update-current-highlight t)
        (when (overlayp dired-image-thumbnail--current-overlay)
          (delete-overlay dired-image-thumbnail--current-overlay))
        (kill-local-variable 'face-remapping-alist)
        ;; Drop the quality segment from the mode line.
        (when (and (local-variable-p 'mode-line-format)
                   (memq 'dired-image-thumbnail--mode-line-quality
                         mode-line-format))
          (setq mode-line-format
                (remq 'dired-image-thumbnail--mode-line-quality
                      mode-line-format))))))
  (dired-image-thumbnail-clear-preview-cache)
  ;; Drop exactly the display-buffer rules we installed.
  (setq display-buffer-alist
        (seq-remove (lambda (entry)
                      (member entry dired-image-thumbnail--display-buffer-entries))
                    display-buffer-alist))
  (setq dired-image-thumbnail--display-buffer-entries nil)
  nil)

;; Load transient menu support if available
(when (require 'dired-image-thumbnail-transient nil t)
  (dired-image-thumbnail-transient-setup-keys))

(provide 'dired-image-thumbnail)
;;; dired-image-thumbnail.el ends here

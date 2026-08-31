;;; dired-image-thumbnail-transient.el --- Transient menu for dired-image-thumbnail -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Dyer

;; Author: James Dyer
;; Keywords: multimedia, files, dired
;; URL: https://github.com/captainflasmr/dired-image-thumbnail
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

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

;; This file provides a transient menu interface for dired-image-thumbnail.
;; It offers a comprehensive menu activated with C-c . in the thumbnail buffer.
;;
;; To use, add to your dired-image-thumbnail.el:
;;
;;   (require 'dired-image-thumbnail-transient)
;;
;; Or load it separately after dired-image-thumbnail.

;;; Code:

(require 'transient)
(require 'image-dired)

;; Declare variables from dired-image-thumbnail
(defvar dired-image-thumbnail--sort-by)
(defvar dired-image-thumbnail--sort-order)
(defvar dired-image-thumbnail--recursive)
(defvar dired-image-thumbnail--all-images)
(defvar dired-image-thumbnail--current-images)
(defvar dired-image-thumbnail-sort-by)
(defvar dired-image-thumbnail-sort-order)
(defvar dired-image-thumbnail-wrap-display)
(defvar dired-image-thumbnail-square-thumbnails)
(defvar dired-image-thumbnail-auto-accept)
(defvar image-dired-thumbnail-mode-map)

;; Declare functions from dired-image-thumbnail
(declare-function image-dired-display-image "image-dired")
(declare-function dired-image-thumbnail--format-active-filters "dired-image-thumbnail")
(declare-function dired-image-thumbnail-hard-refresh "dired-image-thumbnail")
(declare-function dired-image-thumbnail-sort-by-dired "dired-image-thumbnail")
(declare-function dired-image-thumbnail-sort-by-name "dired-image-thumbnail")
(declare-function dired-image-thumbnail-sort-by-date "dired-image-thumbnail")
(declare-function dired-image-thumbnail-sort-by-size "dired-image-thumbnail")
(declare-function dired-image-thumbnail-sort-reverse "dired-image-thumbnail")
(declare-function dired-image-thumbnail-sort "dired-image-thumbnail")
(declare-function dired-image-thumbnail-filter "dired-image-thumbnail")
(declare-function dired-image-thumbnail-filter-by-name "dired-image-thumbnail")
(declare-function dired-image-thumbnail-filter-by-size "dired-image-thumbnail")
(declare-function dired-image-thumbnail-filter-clear "dired-image-thumbnail")
(declare-function dired-image-thumbnail-mark-all "dired-image-thumbnail")
(declare-function dired-image-thumbnail-toggle-all-marks "dired-image-thumbnail")
(declare-function dired-image-thumbnail-goto-dired "dired-image-thumbnail")
(declare-function dired-image-thumbnail-increase-size "dired-image-thumbnail")
(declare-function dired-image-thumbnail-decrease-size "dired-image-thumbnail")
(declare-function dired-image-thumbnail-toggle-wrap "dired-image-thumbnail")
(declare-function dired-image-thumbnail-refresh "dired-image-thumbnail")
(declare-function dired-image-thumbnail-delete "dired-image-thumbnail")
(declare-function dired-image-thumbnail-delete-marked "dired-image-thumbnail")
(declare-function dired-image-thumbnail-open-external "dired-image-thumbnail")
(declare-function dired-image-thumbnail-help "dired-image-thumbnail")
(declare-function dired-image-thumbnail-mark-region "dired-image-thumbnail")
(declare-function dired-image-thumbnail-find-file "dired-image-thumbnail")
(declare-function dired-image-thumbnail-toggle-square-thumbnails "dired-image-thumbnail")

;;; Predicates

(defun dired-image-thumbnail--in-thumbnail-buffer-p ()
  "Return non-nil in an image-dired thumbnail buffer with our enhancements."
  (and (derived-mode-p 'image-dired-thumbnail-mode)
       (boundp 'dired-image-thumbnail--all-images)))

;;; Commands

(defun dired-image-thumbnail-transient-toggle-auto-accept ()
  "Toggle `dired-image-thumbnail-auto-accept'."
  (interactive)
  (setq dired-image-thumbnail-auto-accept
        (not dired-image-thumbnail-auto-accept))
  (message "Auto-accept: %s"
           (if dired-image-thumbnail-auto-accept "ON" "OFF")))

;;; State description function

(defun dired-image-thumbnail-transient--state-description ()
  "Return a string describing the current state."
  (if (not (dired-image-thumbnail--in-thumbnail-buffer-p))
      "Image Thumbnail Commands"
    (let* ((sort-by (or dired-image-thumbnail--sort-by
                        dired-image-thumbnail-sort-by))
           (sort-order (or dired-image-thumbnail--sort-order
                           dired-image-thumbnail-sort-order))
           (recursive dired-image-thumbnail--recursive)
           (wrap dired-image-thumbnail-wrap-display)
           (square dired-image-thumbnail-square-thumbnails)
           (total (length dired-image-thumbnail--all-images))
           (filtered (length dired-image-thumbnail--current-images))
           (filters (dired-image-thumbnail--format-active-filters)))
      (concat
       (propertize "State: " 'face 'transient-heading)
       (format "Sort: %s %s | "
               (propertize (symbol-name sort-by) 'face 'transient-value)
               (if (eq sort-order 'ascending) "↑" "↓"))
       (format "Images: %s%s | "
               (propertize (number-to-string filtered) 'face 'transient-value)
               (if (= total filtered) "" (format "/%d" total)))
       (format "Recursive: %s | "
               (propertize (if recursive "ON" "OFF")
                           'face (if recursive 'success 'shadow)))
       (format "Wrap: %s | "
               (propertize (if wrap "ON" "OFF")
                           'face (if wrap 'success 'shadow)))
       (format "Square: %s"
               (propertize (if square "ON" "OFF")
                           'face (if square 'success 'shadow)))
       (if (string-empty-p filters)
           ""
         (concat "\n" (propertize "Filters: " 'face 'transient-heading)
                 (propertize filters 'face 'transient-value)))))))

;;; Sort submenu

(transient-define-prefix dired-image-thumbnail-transient-sort ()
  "Sorting commands for image thumbnails."
  ["Sort By"
    ("b" "Dired buffer order" dired-image-thumbnail-sort-by-dired :transient nil)
    ("n" "Name" dired-image-thumbnail-sort-by-name :transient nil)
    ("d" "Date modified" dired-image-thumbnail-sort-by-date :transient nil)
    ("s" "Size" dired-image-thumbnail-sort-by-size :transient nil)]
  ["Order"
   ("r" "Reverse order" dired-image-thumbnail-sort-reverse :transient nil)])

;;; Filter submenu

(transient-define-prefix dired-image-thumbnail-transient-filter ()
  "Filtering commands for image thumbnails."
  ["Filter By"
   ("n" "Name (regexp)" dired-image-thumbnail-filter-by-name :transient nil)
   ("s" "Size range" dired-image-thumbnail-filter-by-size :transient nil)]
  ["Actions"
   ("c" "Clear all filters" dired-image-thumbnail-filter-clear :transient nil)
   ("/" "Clear all filters" dired-image-thumbnail-filter-clear :transient nil)])

;;; Display submenu

(transient-define-prefix dired-image-thumbnail-transient-display ()
  "Display commands for image thumbnails."
  ["Size"
   ("+" "Increase size" dired-image-thumbnail-increase-size :transient t)
   ("-" "Decrease size" dired-image-thumbnail-decrease-size :transient t)]
  ["Toggle"
   ("w" "Wrap mode" dired-image-thumbnail-toggle-wrap :transient nil)
   ("#" "Square thumbnails" dired-image-thumbnail-toggle-square-thumbnails :transient nil)]
  ["Refresh"
   ("r" "Refresh display" dired-image-thumbnail-refresh :transient nil)
   ("g" "Refresh display" dired-image-thumbnail-refresh :transient nil)
   ("G" "Hard refresh (clear cache)" dired-image-thumbnail-hard-refresh :transient nil)])

;;; Main transient menu

;;;###autoload
(transient-define-prefix dired-image-thumbnail-transient ()
  "Transient menu for dired-image-thumbnail."
  [:if dired-image-thumbnail--in-thumbnail-buffer-p]
  [:description dired-image-thumbnail-transient--state-description]
  [["Sorting"
    ("s" "Sort menu..." dired-image-thumbnail-transient-sort :transient nil)
    ("S" "Interactive sort" dired-image-thumbnail-sort :transient nil)]
   ["Filtering"
    ("/" "Filter menu..." dired-image-thumbnail-transient-filter :transient nil)
    ("\\" "Interactive filter" dired-image-thumbnail-filter :transient nil)]
   ["Delete"
    ("D" "Delete current" dired-image-thumbnail-delete :transient nil)
    ("x" "Delete marked" dired-image-thumbnail-delete-marked :transient nil)]]
   [["Marking"
     ("m" "Mark current" image-dired-mark-thumb-original-file :transient nil)
     ("u" "Unmark current" image-dired-unmark-thumb-original-file :transient nil)
     ("M" "Mark all" dired-image-thumbnail-mark-all :transient nil)
     ("U" "Unmark all" image-dired-unmark-all-marks :transient nil)
     ("t" "Toggle all marks" dired-image-thumbnail-toggle-all-marks :transient nil)
     ("B" "Block mark (region)" dired-image-thumbnail-mark-region :transient nil)]
["Display"
    ("r" "Refresh" dired-image-thumbnail-refresh :transient nil)
    ("+" "Larger thumbnails" dired-image-thumbnail-increase-size :transient t)
    ("-" "Smaller thumbnails" dired-image-thumbnail-decrease-size :transient t)
    ("w" "Toggle wrap" dired-image-thumbnail-toggle-wrap :transient nil)
    ("#" "Toggle square" dired-image-thumbnail-toggle-square-thumbnails :transient nil)]
   ["Other"
    ("f" "Open image (find-file)" dired-image-thumbnail-find-file :transient nil)
    ("d" "Go to dired" dired-image-thumbnail-goto-dired :transient nil)
    ("W" "Open externally" dired-image-thumbnail-open-external :transient nil)
     ("A" "Auto-accept" dired-image-thumbnail-transient-toggle-auto-accept
      :transient t)
    ("?" "Help" dired-image-thumbnail-help :transient nil)
    ("q" "Quit menu" transient-quit-one)]])

;;;###autoload
(defun dired-image-thumbnail-transient-setup-keys ()
  "Set up keybindings for the transient menu."
  (when (and (fboundp 'dired-image-thumbnail-transient)
             (keymapp image-dired-thumbnail-mode-map))
    (define-key image-dired-thumbnail-mode-map (kbd "C-c .") #'dired-image-thumbnail-transient)
    (define-key image-dired-thumbnail-mode-map (kbd "?") #'dired-image-thumbnail-transient)))

(provide 'dired-image-thumbnail-transient)
;;; dired-image-thumbnail-transient.el ends here

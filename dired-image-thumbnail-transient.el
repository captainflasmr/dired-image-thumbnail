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
(defvar image-dired-thumbnail-mode-map)

;; Declare functions from dired-image-thumbnail
(declare-function image-dired-display-image "image-dired")
(declare-function dired-image-thumbnail--format-active-filters "dired-image-thumbnail")
(declare-function dired-image-thumbnail-hard-refresh "dired-image-thumbnail")
(declare-function dired-image-thumbnail-sort "dired-image-thumbnail")
(declare-function dired-image-thumbnail-filter "dired-image-thumbnail")
(declare-function dired-image-thumbnail-mark-all "dired-image-thumbnail")
(declare-function dired-image-thumbnail-toggle-all-marks "dired-image-thumbnail")
(declare-function dired-image-thumbnail-goto-dired "dired-image-thumbnail")
(declare-function dired-image-thumbnail-increase-size "dired-image-thumbnail")
(declare-function dired-image-thumbnail-decrease-size "dired-image-thumbnail")
(declare-function dired-image-thumbnail-refresh "dired-image-thumbnail")
(declare-function dired-image-thumbnail-delete "dired-image-thumbnail")
(declare-function dired-image-thumbnail-delete-marked "dired-image-thumbnail")
(declare-function dired-image-thumbnail-delete-and-next "dired-image-thumbnail")
(declare-function dired-image-thumbnail-next-image "dired-image-thumbnail")
(declare-function dired-image-thumbnail-previous-image "dired-image-thumbnail")
(declare-function dired-image-thumbnail-hard-refresh "dired-image-thumbnail")
(declare-function dired-image-thumbnail-select-display-quality "dired-image-thumbnail")
(declare-function dired-image-thumbnail-insert-subdir-recursive "dired-image-thumbnail")
(declare-function dired-image-thumbnail-insert-image-subdirs "dired-image-thumbnail")
(declare-function dired-image-thumbnail-kill-all-subdirs "dired-image-thumbnail")
(declare-function dired-image-thumbnail-open-external "dired-image-thumbnail")
(declare-function dired-image-thumbnail-move "dired-image-thumbnail")
(declare-function dired-image-thumbnail-toggle-square-thumbnails "dired-image-thumbnail")
(declare-function dired-image-thumbnail-toggle-auto-display "dired-image-thumbnail")

;;; Predicates

(defun dired-image-thumbnail--in-thumbnail-buffer-p ()
  "Return non-nil in an image-dired thumbnail buffer with our enhancements."
  (and (derived-mode-p 'image-dired-thumbnail-mode)
       (boundp 'dired-image-thumbnail--all-images)))

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

;;; Display submenu

(transient-define-prefix dired-image-thumbnail-transient-display ()
  "Display commands for image thumbnails."
  ["Size"
   ("+" "Increase size" dired-image-thumbnail-increase-size :transient t)
   ("-" "Decrease size" dired-image-thumbnail-decrease-size :transient t)]
  ["Toggle"
   ("#" "Square thumbnails" dired-image-thumbnail-toggle-square-thumbnails :transient nil)
   ("F" "Follow (auto-display)" dired-image-thumbnail-toggle-auto-display :transient nil)]
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
  [["Navigate"
    ("n" "Next thumbnail" dired-image-thumbnail-next-image :transient t)
    ("p" "Previous thumbnail" dired-image-thumbnail-previous-image :transient t)]
   ["Sort & Filter"
    ("s" "Sort..." dired-image-thumbnail-sort :transient nil)
    ("/" "Filter..." dired-image-thumbnail-filter :transient nil)]
   ["Delete"
    ("D" "Delete current" dired-image-thumbnail-delete :transient nil)
    ("C-d" "Delete, move to next" dired-image-thumbnail-delete-and-next :transient nil)
    ("x" "Delete marked" dired-image-thumbnail-delete-marked :transient nil)]]
  [["Marking"
    ("m" "Mark current" image-dired-mark-thumb-original-file :transient nil)
    ("u" "Unmark current" image-dired-unmark-thumb-original-file :transient nil)
    ("M" "Mark all" dired-image-thumbnail-mark-all :transient nil)
    ("U" "Unmark all" image-dired-unmark-all-marks :transient nil)
    ("t" "Toggle all marks" dired-image-thumbnail-toggle-all-marks :transient nil)]
   ["Display"
    ("g" "Refresh" dired-image-thumbnail-refresh :transient nil)
    ("G" "Hard refresh (clear cache)" dired-image-thumbnail-hard-refresh :transient nil)
    ("+" "Larger thumbnails" dired-image-thumbnail-increase-size :transient t)
    ("-" "Smaller thumbnails" dired-image-thumbnail-decrease-size :transient t)
    ("#" "Toggle square" dired-image-thumbnail-toggle-square-thumbnails :transient nil)
    ("F" "Toggle follow" dired-image-thumbnail-toggle-auto-display :transient nil)
    ("Q" "Select quality" dired-image-thumbnail-select-display-quality :transient nil)]
   ["Subdirs"
    ("i" "Insert image subdirs" dired-image-thumbnail-insert-image-subdirs :transient nil)
    ("I" "Insert subdirs (recursive)" dired-image-thumbnail-insert-subdir-recursive :transient nil)
    ("K" "Kill all subdirs" dired-image-thumbnail-kill-all-subdirs :transient nil)]
   ["Other"
    ("v" "Move to directory" dired-image-thumbnail-move :transient nil)
    ("d" "Go to dired" dired-image-thumbnail-goto-dired :transient nil)
    ("W" "Open externally" dired-image-thumbnail-open-external :transient nil)
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

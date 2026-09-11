;;; dired-image-thumbnail-test.el --- ERT suite for dired-image-thumbnail -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Dyer

;; Author: James Dyer
;; Keywords: multimedia, files, dired

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

;; Batch tests for `dired-image-thumbnail'.  Run with:
;;
;;   emacs -Q --batch -L .. -l dired-image-thumbnail-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; or via test/run-tests.sh.

;;; Code:

(require 'ert)
(require 'dired-image-thumbnail)

;;; Utilities

(defmacro dit--with-temp-dir (dir-var &rest body)
  "Bind DIR-VAR to a fresh temporary directory and run BODY, cleaning up."
  (declare (indent 1))
  `(let ((,dir-var (file-name-as-directory (make-temp-file "dit-" t))))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,dir-var)
         (delete-directory ,dir-var t)))))

;;; Sorting and filtering

(ert-deftest dit-parse-size ()
  (should (= (dired-image-thumbnail--parse-size "1k") 1024))
  (should (= (dired-image-thumbnail--parse-size "2m") (* 2 1048576)))
  (should (= (dired-image-thumbnail--parse-size "1g") 1073741824))
  (should (= (dired-image-thumbnail--parse-size "500") 500)))

(ert-deftest dit-sort-images-name ()
  (let ((dired-image-thumbnail--sort-by 'name)
        (dired-image-thumbnail--sort-order 'ascending))
    (should (equal (dired-image-thumbnail--sort-images
                    '("/tmp/b.jpg" "/tmp/A.jpg" "/tmp/c.jpg"))
                   '("/tmp/A.jpg" "/tmp/b.jpg" "/tmp/c.jpg"))))
  (let ((dired-image-thumbnail--sort-by 'name)
        (dired-image-thumbnail--sort-order 'descending))
    (should (equal (dired-image-thumbnail--sort-images
                    '("/tmp/b.jpg" "/tmp/A.jpg"))
                   '("/tmp/b.jpg" "/tmp/A.jpg")))))

(ert-deftest dit-sort-tolerates-vanished-files ()
  (let ((dired-image-thumbnail--sort-by 'date)
        (dired-image-thumbnail--sort-order 'ascending))
    (should (equal (dired-image-thumbnail--sort-images
                    (list "/tmp/dit-missing-sort.jpg"))
                   (list "/tmp/dit-missing-sort.jpg")))))

(ert-deftest dit-filter-by-name ()
  (let ((dired-image-thumbnail--filter-name "foo")
        (dired-image-thumbnail--filter-size-min nil)
        (dired-image-thumbnail--filter-size-max nil))
    (should (equal (dired-image-thumbnail--filter-images
                    '("/tmp/foo.jpg" "/tmp/bar.jpg"))
                   '("/tmp/foo.jpg")))))

(ert-deftest dit-filter-by-size ()
  (dit--with-temp-dir dir
    (let ((small (expand-file-name "s.jpg" dir))
          (big (expand-file-name "b.jpg" dir))
          (dired-image-thumbnail--filter-name nil)
          (dired-image-thumbnail--filter-size-min 50)
          (dired-image-thumbnail--filter-size-max nil))
      (write-region "aaaa" nil small nil 'silent)
      (write-region (make-string 100 ?x) nil big nil 'silent)
      (should (equal (dired-image-thumbnail--filter-images (list small big))
                     (list big))))))

(ert-deftest dit-filter-drops-vanished-files ()
  (let ((dired-image-thumbnail--filter-name nil)
        (dired-image-thumbnail--filter-size-min 1)
        (dired-image-thumbnail--filter-size-max 1000))
    (should (null (dired-image-thumbnail--filter-images
                   (list "/tmp/dit-missing-filter.jpg"))))))

;;; Property scanning helpers

(ert-deftest dit-property-helpers ()
  (with-temp-buffer
    (insert "  ")
    (add-text-properties (point-min) (1+ (point-min))
                         '(original-file-name "a"))
    (add-text-properties (1+ (point-min)) (point-max)
                         '(original-file-name "b"))
    (should (equal (dired-image-thumbnail--property-positions 'original-file-name)
                   (list (point-min) (1+ (point-min)))))
    (should (equal (dired-image-thumbnail--property-values 'original-file-name)
                   '("a" "b")))
    (should (equal (dired-image-thumbnail--position-of-file "b")
                   (1+ (point-min))))
    (should (null (dired-image-thumbnail--position-of-file "z")))))

;;; State helpers

(ert-deftest dit-image-index ()
  (with-temp-buffer
    (setq dired-image-thumbnail--current-images '("/a.jpg" "/b.jpg" "/c.jpg"))
    (dired-image-thumbnail--rebuild-image-index)
    (should (= (gethash "/a.jpg" dired-image-thumbnail--image-index) 0))
    (should (= (gethash "/c.jpg" dired-image-thumbnail--image-index) 2))
    (should (null (gethash "/z.jpg" dired-image-thumbnail--image-index)))))

(ert-deftest dit-drop-from-state ()
  (with-temp-buffer
    (setq dired-image-thumbnail--current-images '("/a.jpg" "/b.jpg"))
    (setq dired-image-thumbnail--all-images '("/a.jpg" "/b.jpg"))
    (dired-image-thumbnail--rebuild-image-index)
    (dired-image-thumbnail--drop-from-state "/a.jpg")
    (should (equal dired-image-thumbnail--current-images '("/b.jpg")))
    (should (equal dired-image-thumbnail--all-images '("/b.jpg")))
    (should (= (gethash "/b.jpg" dired-image-thumbnail--image-index) 0))))

(ert-deftest dit-header-uses-index ()
  (with-temp-buffer
    (image-dired-thumbnail-mode)
    (setq dired-image-thumbnail--all-images '("/a.jpg" "/b.jpg" "/c.jpg")
          dired-image-thumbnail--current-images '("/a.jpg" "/b.jpg" "/c.jpg")
          dired-image-thumbnail--dired-buffer nil)
    (dired-image-thumbnail--rebuild-image-index)
    (let ((dired-image-thumbnail--identify-cached-command nil))
      (let ((result (dired-image-thumbnail--format-properties-string
                     (lambda (&rest _) "orig") nil "/b.jpg" "99/99" nil nil)))
        (should (string-match-p "2/3" result))))))

(ert-deftest dit-count-thumbnail-work ()
  (with-temp-buffer
    (setq dired-image-thumbnail--current-images '("/tmp/dit-nonexistent-xyz.jpg"))
    (setq dired-image-thumbnail-square-thumbnails nil)
    (let ((state (dired-image-thumbnail--make-work-state)))
      (dired-image-thumbnail--count-thumbnail-work state)
      (should (= (dired-image-thumbnail--work-state-needed state) 1))
      (should (dired-image-thumbnail--work-state-progress state)))))

(ert-deftest dit-work-state ()
  (let ((state (dired-image-thumbnail--make-work-state)))
    (should (= (dired-image-thumbnail--work-state-needed state) 0))
    (cl-incf (dired-image-thumbnail--work-state-needed state))
    (should (= (dired-image-thumbnail--work-state-needed state) 1))))

(ert-deftest dit-ensure-thumb-geometry ()
  (with-temp-buffer
    (setq-local image-dired-thumb-size nil)
    (setq-local image-dired-thumb-relief nil)
    (setq-local image-dired-thumb-margin nil)
    (setq-local image-dired-thumbs-per-row nil)
    (dired-image-thumbnail--ensure-thumb-geometry)
    (should (numberp image-dired-thumb-size))
    (should (numberp image-dired-thumb-relief))
    (should (numberp image-dired-thumb-margin))
    (should (numberp image-dired-thumbs-per-row))))

(ert-deftest dit-base-thumb-size ()
  (should (numberp (dired-image-thumbnail--base-thumb-size))))

(ert-deftest dit-cursor-copy-preserves-global ()
  (let ((saved face-remapping-alist))
    (unwind-protect
        (progn
          (setq face-remapping-alist
                '((mode-line . highlight)
                  (cursor . (:background "red"))))
          (with-temp-buffer
            (image-dired-thumbnail-mode)
            (dired-image-thumbnail--setup-cursor))
          (should (equal face-remapping-alist
                         '((mode-line . highlight)
                           (cursor . (:background "red"))))))
      (setq face-remapping-alist saved))))

(ert-deftest dit-auto-display-p ()
  (with-temp-buffer
    (image-dired-thumbnail-mode)
    (setq-local dired-image-thumbnail-auto-display-on-navigate nil)
    (should (not (dired-image-thumbnail--auto-display-p)))))

;;; Per-directory settings

(ert-deftest dit-filter-plist-p ()
  (should (dired-image-thumbnail--filter-plist-p nil))
  (should (dired-image-thumbnail--filter-plist-p '(:name "x")))
  (should (dired-image-thumbnail--filter-plist-p
           '(:name "x" :size-min 1 :size-max 100)))
  (should (not (dired-image-thumbnail--filter-plist-p '(:name 42))))
  (should (not (dired-image-thumbnail--filter-plist-p '(:name)))))

(ert-deftest dit-valid-dir-setting-p ()
  (should (dired-image-thumbnail--valid-dir-setting-p
           'dired-image-thumbnail-sort-by 'date))
  (should (not (dired-image-thumbnail--valid-dir-setting-p
                'dired-image-thumbnail-sort-by 'dimensions)))
  (should (not (dired-image-thumbnail--valid-dir-setting-p
                'dired-image-thumbnail-display-quality 'bogus)))
  (should (dired-image-thumbnail--valid-dir-setting-p
           'dired-image-thumbnail-default-filter '(:name "foo" :size-min 1))))

(ert-deftest dit-dir-locals-roundtrip ()
  (dit--with-temp-dir dir
    (dired-image-thumbnail--write-dir-locals
     dir '((dired-image-thumbnail-sort-by . date)))
    (should (equal (dired-image-thumbnail--read-dir-settings dir)
                   '((dired-image-thumbnail-sort-by . date))))
    (let ((enable-local-variables nil))
      (should (null (dired-image-thumbnail--read-dir-settings dir))))))

(ert-deftest dit-dir-locals-preserves-other-entries ()
  (dit--with-temp-dir dir
    (with-temp-file (expand-file-name ".dir-locals.el" dir)
      (prin1 '((nil . ((fill-column . 72)))) (current-buffer)))
    (dired-image-thumbnail--write-dir-locals
     dir '((dired-image-thumbnail-sort-by . date)))
    (let* ((all (with-temp-buffer
                  (insert-file-contents (expand-file-name ".dir-locals.el" dir))
                  (read (current-buffer))))
           (entry (cdr (assq nil all))))
      (should (= (cdr (assq 'fill-column entry)) 72))
      (should (eq (cdr (assq 'dired-image-thumbnail-sort-by entry)) 'date)))))

;;; Dimension querying

(ert-deftest dit-identify-guard ()
  (with-temp-buffer
    (let ((dired-image-thumbnail--dimension-cache (make-hash-table :test 'equal))
          (dired-image-thumbnail--dimension-pending (make-hash-table :test 'equal))
          (dired-image-thumbnail--identify-cached-command nil))
      (should (equal (dired-image-thumbnail--get-image-dimensions "/tmp/nope.jpg")
                     '(0 . 0)))
      (should (zerop (hash-table-count
                      dired-image-thumbnail--dimension-pending))))))

;;; Display-buffer rule tagging

(ert-deftest dit-display-buffer-tagging ()
  (let ((saved-entries dired-image-thumbnail--display-buffer-entries)
        (saved-layout dired-image-thumbnail-window-layout)
        (saved-ratio dired-image-thumbnail-window-ratio)
        (saved-alist display-buffer-alist))
    (unwind-protect
        (progn
          (setq dired-image-thumbnail-window-layout 'left-right)
          (setq dired-image-thumbnail-window-ratio 0.6)
          (setq display-buffer-alist '(("\\*image-dired\\*" . user-rule)))
          (dired-image-thumbnail-setup-display-buffer)
          (should (assoc "\\*image-dired\\*" display-buffer-alist))
          (should (= 2 (length dired-image-thumbnail--display-buffer-entries)))
          (dired-image-thumbnail-setup-display-buffer)
          (should (= 2 (length dired-image-thumbnail--display-buffer-entries)))
          (should (assoc "\\*image-dired\\*" display-buffer-alist)))
      (setq dired-image-thumbnail--display-buffer-entries saved-entries)
      (setq dired-image-thumbnail-window-layout saved-layout)
      (setq dired-image-thumbnail-window-ratio saved-ratio)
      (setq display-buffer-alist saved-alist))))

;;; Subdirectory handling

(ert-deftest dit-find-subdirs-cycle ()
  (dit--with-temp-dir root
    (let ((sub (expand-file-name "sub" root)))
      (make-directory sub)
      (make-symbolic-link root (expand-file-name "link" sub) t)
      (should (equal (mapcar #'file-name-nondirectory
                             (dired-image-thumbnail--find-subdirs root))
                     '("sub" "link"))))))

(ert-deftest dit-insert-subdirs-count ()
  (dit--with-temp-dir root
    (let ((sub (expand-file-name "sub" root))
          (dired-buf (dired-noselect root)))
      (make-directory sub)
      (unwind-protect
          (with-current-buffer dired-buf
            (should (= 1 (dired-image-thumbnail--insert-subdirs (list sub))))
            (should (= 0 (dired-image-thumbnail--insert-subdirs (list sub)))))
        (when (buffer-live-p dired-buf)
          (kill-buffer dired-buf))))))

(provide 'dired-image-thumbnail-test)
;;; dired-image-thumbnail-test.el ends here
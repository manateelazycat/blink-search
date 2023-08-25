;;; blink-search-pdf.el --- Search PDF buffer backend   -*- lexical-binding: t; no-byte-compile: t; -*-

;; Filename: blink-search-pdf.el
;; Description: Search PDF buffer backend
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2023, Andy Stewart, all rights reserved.
;; Created: 2023-05-31 08:58:03
;; Version: 0.1
;; Last-Updated: 2023-05-31 08:58:03
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/blink-search-pdf
;; Keywords:
;; Compatibility: GNU Emacs 30.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Search PDF buffer backend
;;

;;; Installation:
;;
;; Put blink-search-pdf.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'blink-search-pdf)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET blink-search-pdf RET
;;

;;; Change log:
;;
;; 2023/05/31
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'blink-search-grep-file)

;;; Code:

(defvar blink-search-pdf-search-paths nil
  "Paths to search for pdf files.")

(defvar blink-search-pdf-backend 'pdf-tools
  "Backend to use for pdf files.")

(defvar blink-search-pdf-preview-idle-time 0.5
  "Time to wait before previewing pdf.")

(defvar blink-search-pdf-preview-timer nil)

(defun blink-search-pdf-pdftool-goto (file page submatches)
  (find-file file)
  (pdf-view-goto-page page)
  (let ((matches (pdf-isearch-search-page submatches)))
    (pdf-isearch-hl-matches (nth 0 matches) matches t)))

(defun blink-search-pdf-do (file page submatches)
  ;;highlight the matches
  (cond
   ((and (eq blink-search-pdf-backend 'pdf-tools) (featurep 'pdf-tools))
    (blink-search-pdf-pdftool-goto file page submatches))
   ((and (eq blink-search-pdf-backend 'eaf-pdf-viewer) (featurep 'eaf-pdf-viewer))
    (eaf-pdf-jump-to-page file page))
   ;; TODO support other pdf backends
   (t (message "Unknown backend %s" blink-search-pdf-backend))))

(defun blink-search-pdf-real-preview (file page submatches)
  (blink-search-select-input-window
   (let ((match-file (blink-search-grep-file-get-match-buffer file)))
     (blink-search-pdf-do file page submatches)
     (unless match-file
       (add-to-list 'blink-search-grep-file-temp-buffers (current-buffer))))))

(defun blink-search-pdf-preview (file page submatches)
  (if blink-search-pdf-preview-timer (cancel-timer blink-search-pdf-preview-timer))
  (setq blink-search-pdf-preview-timer
        (run-with-idle-timer blink-search-pdf-preview-idle-time nil
                             (lambda ()
                               (blink-search-pdf-real-preview file page submatches)))))


(defun blink-search-pdf-clean ()
  (blink-search-grep-file-clean)
  (if blink-search-pdf-preview-timer (cancel-timer blink-search-pdf-preview-timer)))

(provide 'blink-search-pdf)
;;; blink-search-pdf.el ends here

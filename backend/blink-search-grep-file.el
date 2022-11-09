;;; blink-search-grep-file.el --- rg backend for blink-search   -*- lexical-binding: t; -*-

;; Filename: blink-search-grep-file.el
;; Description: rg backend for blink-search
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-11-04 08:44:27
;; Version: 0.1
;; Last-Updated: 2022-11-04 08:44:27
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/blink-search-grep-file
;; Keywords:
;; Compatibility: GNU Emacs 28.2
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
;; rg backend for blink-search
;;

;;; Installation:
;;
;; Put blink-search-grep-file.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'blink-search-grep-file)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET blink-search-grep-file RET
;;

;;; Change log:
;;
;; 2022/11/04
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


;;; Code:

(defvar blink-search-grep-file-temp-buffers nil)

(defun blink-search-grep-file-get-match-buffer (filepath)
  (catch 'find-match
    (dolist (buffer (buffer-list))
      (when (string-equal (buffer-file-name buffer) filepath)
        (throw 'find-match buffer)))
    nil))

(defun blink-search-grep-file-do (file line column)
  (find-file file)
  (ignore-errors
    (goto-line line)
    (blink-search-goto-column column))
  (blink-search-flash-locate))

(defun blink-search-grep-file-preview (file line column)
  (blink-search-select-input-window
   (let ((match-file (blink-search-grep-file-get-match-buffer file)))
     (blink-search-grep-file-do file line column)
     (unless match-file
       (add-to-list 'blink-search-grep-file-temp-buffers (current-buffer))
       ))))

(defun blink-search-grep-file-clean ()
  (dolist (temp-buffer blink-search-grep-file-temp-buffers)
    (kill-buffer temp-buffer))

  (setq blink-search-grep-file-temp-buffers nil))

(provide 'blink-search-grep-file)

;;; blink-search-grep-file.el ends here

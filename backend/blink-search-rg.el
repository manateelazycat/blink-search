;;; blink-search-rg.el --- rg backend for blink-search   -*- lexical-binding: t; -*-

;; Filename: blink-search-rg.el
;; Description: rg backend for blink-search
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-11-04 08:44:27
;; Version: 0.1
;; Last-Updated: 2022-11-04 08:44:27
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/blink-search-rg
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
;; Put blink-search-rg.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'blink-search-rg)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET blink-search-rg RET
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

(defun blink-search-rg-do (file line column)
  (find-file file)
  (goto-line line)
  (blink-search-goto-column column)
  (recenter)
  (blink-search-flash-line))

(defun blink-search-rg-preview (file line column)
  (blink-search-preview
   (blink-search-rg-do file line column)
   ))

(provide 'blink-search-rg)

;;; blink-search-rg.el ends here

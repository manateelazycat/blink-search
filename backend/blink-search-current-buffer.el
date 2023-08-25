;;; blink-search-current-buffer.el --- current buffer backend for blink-search   -*- lexical-binding: t; no-byte-compile: t; -*-

;; Filename: blink-search-current-buffer.el
;; Description: current buffer backend for blink-search
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-11-04 08:46:22
;; Version: 0.1
;; Last-Updated: 2022-11-04 08:46:22
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/blink-search-current-buffer
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
;; current buffer backend for blink-search
;;

;;; Installation:
;;
;; Put blink-search-current-buffer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'blink-search-current-buffer)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET blink-search-current-buffer RET
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

(defun blink-search-init-current-buffer ()
  (when (blink-search-epc-live-p blink-search-epc-process)
    (blink-search-call-async "search_init_current_buffer"
                             (buffer-name blink-search-start-buffer)
                             (blink-search-encode-string
                              (with-current-buffer blink-search-start-buffer
                                (buffer-string))))))

(defun blink-search-encode-string (str)
  "Encode string STR with UTF-8 coding using Base64."
  (base64-encode-string (encode-coding-string str 'utf-8)))

(defun blink-search-current-buffer-preview (buffer line column)
  (blink-search-select-input-window
   (blink-search-current-buffer-do buffer line column)))

(defun blink-search-current-buffer-do (buffer line column)
  (switch-to-buffer buffer)
  (goto-line line)
  (blink-search-goto-column column)
  (blink-search-flash-locate))

(add-to-list 'blink-search-start-update-list #'blink-search-init-current-buffer t)

(provide 'blink-search-current-buffer)

;;; blink-search-current-buffer.el ends here

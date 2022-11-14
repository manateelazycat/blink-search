;;; blink-search-icon.el --- Icon for blink-search

;; Filename: blink-search-icon.el
;; Description: Icon for blink-search
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 21:41:25
;; Version: 0.1
;; Last-Updated: 2022-06-07 21:41:25
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/blink-search-icon
;; Keywords:
;; Compatibility: GNU Emacs 28.1
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
;; Icon for blink-search
;;

;;; Installation:
;;
;; Put blink-search-icon.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'blink-search-icon)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET blink-search-icon RET
;;

;;; Change log:
;;
;; 2022/06/07
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
(require 'svg)

;;; Code:

(defvar blink-search-icon-collections
  '(("bootstrap" . "https://icons.getbootstrap.com/icons/%s.svg")
    ("material" . "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
    ("octicons" . "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
    ("boxicons" . "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))

(defvar blink-search-icon-alist
  `(("Buffer List" . ("material" "list-box-outline" "#98c807"))
    ("Elisp Symbol" . ("material" "variable" "#da1884"))
    ("Recent File" . ("material" "timer-marker-outline" "#d1de3f"))
    ("EAF Browser History" . ("material" "google-chrome" "#0abf53"))
    ("Google Suggest" . ("material" "google" "#7ac143"))
    ("Find File" . ("material" "text-box-search-outline" "#b84592"))
    ("Grep File" . ("material" "folder-search-outline" "#ff6c5f"))
    ("Current File" . ("material" "code-greater-than-or-equal" "#00b2a9"))
    ("IMenu" . ("material" "file-tree-outline" "#e04646"))
    ("Common Directory" . ("material" "folder-marker-outline" "#ff9900"))
    (t . ("material" "file-find-outline" "#90cef1"))))

(defvar blink-search-icon-cache (make-hash-table :test 'equal))
(defvar blink-search-icon-dir (expand-file-name "icons" (file-name-directory load-file-name)))
(defvar blink-search-icon-width 4)

(defun blink-search-icon-filepath (collection name)
  (concat (file-name-as-directory blink-search-icon-dir) (format "%s_%s.svg" collection name)))

;;;###autoload
(defun blink-search-icon-fetch-all ()
  (interactive)
  (dolist (icon blink-search-icon-alist)
    (let* ((collection (nth 0 (cdr icon)))
           (name (nth 1 (cdr icon)))
           (url (format (cdr (assoc collection blink-search-icon-collections)) name))
           (filename (blink-search-icon-filepath collection name)))
      (with-temp-buffer
        (url-insert-file-contents url)
        (write-region (point-min) (point-max) filename)))))

(defun blink-search-icon-parse (collection name)
  (with-temp-buffer
    (insert-file-contents (blink-search-icon-filepath collection name))
    (xml-parse-region (point-min) (point-max))))

(defun blink-search-icon-convert-to-svg-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun blink-search-icon (collection name &optional fg-color bg-color zoom)
  (let* ((root (blink-search-icon-parse collection name))

         ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar 'string-to-number (split-string viewbox)))
         (view-x (nth 0 viewbox))
         (view-y (nth 1 viewbox))
         (view-width (nth 2 viewbox))
         (view-height (nth 3 viewbox))

         ;; Set icon size (in pixels) to 4x1 characters
         (svg-width  (* (window-font-width)  blink-search-icon-width))
         (svg-height (* (window-font-height) 1))

         ;; Zoom the icon by using integer factor only
         (zoom (max 1 (truncate (or zoom 1))))
         (svg-width  (* svg-width zoom))
         (svg-height (* svg-height zoom))

         (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))
         (fg-color (blink-search-icon-convert-to-svg-color
                    (or (when (facep fg-color)
                          (face-foreground fg-color nil t))
                        fg-color (face-attribute 'default :foreground))))
         (bg-color (blink-search-icon-convert-to-svg-color
                    (or (when (facep bg-color)
                          (face-background bg-color nil t))
                        bg-color "transparent")))
         (svg (svg-create svg-width svg-height
                          :viewBox svg-viewbox
                          :stroke-width 0
                          :fill fg-color)))
    (svg-rectangle svg
                   view-x view-y view-width view-height
                   :fill bg-color)

    (dolist (item (xml-get-children (car root) 'path))
      (let* ((attrs (xml-node-attributes item))
             (path (cdr (assoc 'd attrs)))
             (fill (or (cdr (assoc 'fill attrs)) fg-color)))
        (svg-node svg 'path :d path :fill fill)))
    (svg-image svg :ascent 'center :scale 1)))

(defun blink-search-icon-build (collection name fg-color)
  (if (and (display-graphic-p)
           blink-search-enable-icon
           (image-type-available-p 'svg))
      (let* ((icon-key (format "%s_%s" collection name))
             (icon-text (gethash icon-key blink-search-icon-cache)))
        (unless icon-text
          (setq icon-text (propertize
                           (apply #'concat (make-list blink-search-icon-width "-"))
                           'display (blink-search-icon collection name fg-color)))
          (puthash icon-key icon-text blink-search-icon-cache))
        icon-text)
    ""))

(provide 'blink-search-icon)

;;; blink-search-icon.el ends here

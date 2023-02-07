;;; blink-search-elisp-symbol.el --- Elisp symbol backend   -*- lexical-binding: t; -*-

;; Filename: blink-search-elisp-symbol.el
;; Description: Elisp symbol backend
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-11-04 08:38:34
;; Version: 0.1
;; Last-Updated: 2022-11-04 08:38:34
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/blink-search-elisp-symbol
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
;; Elisp symbol backend
;;

;;; Installation:
;;
;; Put blink-search-elisp-symbol.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'blink-search-elisp-symbol)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET blink-search-elisp-symbol RET
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

(defvar blink-search-elisp-symbol-timer nil)
(defvar blink-search-elisp-symbol-size 0)

(defvar blink-search-elisp-parse-depth 100)
(defvar blink-search-elisp-parse-limit 30)
(defvar blink-search-elisp-var-binding-regexp
  "\\_<\\(?:cl-\\)?\\(?:def\\(?:macro\\|subst\\|un\\)\\|l\\(?:ambda\\|e\\(?:\\(?:xical-le\\)?t\\)\\)\\)\\*?\\_>")
(defvar blink-search-elisp-var-binding-regexp-1
  "\\_<\\(?:cl-\\)?\\(?:do\\(?:list\\|times\\)\\)\\*?\\_>")

(defcustom blink-search-elisp-symbol-update-idle 5
  "The idle seconds to update elisp symbols."
  :type 'float
  :group 'blink-search)

(defun blink-search-elisp-symbol-update ()
  "We need synchronize elisp symbols to Python side when idle."
  (let* ((symbols (append (blink-search-elisp-local-symbols)
                          (blink-search-elisp-global-symbols)))
         (symbols-size (length symbols)))
    ;; Only synchronize when new symbol created.
    (unless (equal blink-search-elisp-symbol-size symbols-size)
      (blink-search-call-async "search_elisp_symbol_update" symbols)
      (setq blink-search-elisp-symbol-size symbols-size))))

(defun blink-search-start-elisp-symbol-update ()
  (blink-search-elisp-symbol-update)

  (unless blink-search-elisp-symbol-timer
    (setq blink-search-elisp-symbol-timer (run-with-idle-timer blink-search-elisp-symbol-update-idle t #'blink-search-elisp-symbol-update))))

(defun blink-search-stop-elisp-symbol-update ()
  (when blink-search-elisp-symbol-timer
    (cancel-timer blink-search-elisp-symbol-timer)
    (setq blink-search-elisp-symbol-timer nil)
    (setq blink-search-elisp-symbol-size 0)))

(defun blink-search-elisp-symbol-do (candidate)
  (let* ((symbol (intern candidate)))
    (cond ((commandp symbol)
           (call-interactively symbol))
          ((or (functionp symbol)
               (macrop symbol))
           (describe-function symbol))
          ((facep symbol)
           (customize-face symbol))
          ((custom-variable-p symbol)
           (customize-option symbol))
          (t
           (describe-variable symbol)))))

(defun blink-search-elisp-global-symbols ()
  (all-completions ""
                   obarray
                   (lambda (symbol)
                     (or (fboundp symbol)
                         (boundp symbol)
                         (featurep symbol)
                         (facep symbol)))))

(defun blink-search-elisp-local-symbols ()
  (when (or (derived-mode-p 'emacs-lisp-mode)
            (derived-mode-p 'inferior-emacs-lisp-mode)
            (derived-mode-p 'lisp-interaction-mode))
    (let ((regexp "[ \t\n]*\\(\\_<\\(?:\\sw\\|\\s_\\)*\\_>\\)")
          (pos (point))
          res)
      (condition-case nil
          (save-excursion
            (dotimes (_ blink-search-elisp-parse-depth)
              (up-list -1)
              (save-excursion
                (when (eq (char-after) ?\()
                  (forward-char 1)
                  (when (ignore-errors
                          (save-excursion (forward-list)
                                          (<= (point) pos)))
                    (skip-chars-forward " \t\n")
                    (cond
                     ((looking-at blink-search-elisp-var-binding-regexp)
                      (down-list 1)
                      (condition-case nil
                          (dotimes (_ blink-search-elisp-parse-limit)
                            (save-excursion
                              (when (looking-at "[ \t\n]*(")
                                (down-list 1))
                              (when (looking-at regexp)
                                (cl-pushnew (match-string-no-properties 1) res)))
                            (forward-sexp))
                        (scan-error nil)))
                     ((looking-at blink-search-elisp-var-binding-regexp-1)
                      (down-list 1)
                      (when (looking-at regexp)
                        (cl-pushnew (match-string-no-properties 1) res)))))))))
        (scan-error nil))

      res)))

(add-to-list 'blink-search-idle-update-list #'blink-search-start-elisp-symbol-update t)

(provide 'blink-search-elisp-symbol)

;;; blink-search-elisp-symbol.el ends here

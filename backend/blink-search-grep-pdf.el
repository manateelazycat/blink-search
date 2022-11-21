;;; blink-search-grep-pdf.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 royokong
;;
;; Author: royokong <j614023177@icloud.com>
;; Maintainer: royokong <j614023177@icloud.com>
;; Created: November 21, 2022
;; Modified: November 21, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/royokong/blink-search-grep-pdf
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'blink-search-grep-file)

(defvar blink-search-grep-pdf-search-paths nil
  "Paths to search for pdf files.")

(defvar blink-search-grep-pdf-backend 'pdf-tools
  "Backend to use for pdf files.")

(defvar blink-search-preview-pdf-idle-time 0.5
  "Time to wait before previewing pdf.")

(defvar blink-search-preview-pdf-timer nil)

(defun blink-search-init-grep-pdf ()
  (when (blink-search-epc-live-p blink-search-epc-process)
    (blink-search-call-async "search_init_grep_pdf"
                             blink-search-grep-pdf-search-path)))

(defun blink-search-grep-pdf-pdftool-goto (file page submatches)
  (find-file file)
  (pdf-view-goto-page page)
  (let ((matches (pdf-isearch-search-page submatches)))
    (pdf-isearch-hl-matches (nth 0 matches) matches t)))

(defun blink-search-grep-pdf-do (file page submatches)
  ;;highlight the matches
  (cond
   ((and (eq blink-search-grep-pdf-backend 'pdf-tools) (featurep 'pdf-tools))
    (blink-search-grep-pdf-pdftool-goto file page submatches))
   ((and (eq blink-search-grep-pdf-backend 'eaf-pdf-viewer) (featurep 'eaf-pdf-viewer))
    (eaf-pdf-jump-to-page file page))
   ;; TODO support other pdf backends
   (t (message "Unknown backend %s" blink-search-grep-pdf-backend))))

(defun blink-search-grep-pdf-real-preview (file page submatches)
  (blink-search-select-input-window
   (let ((match-file (blink-search-grep-file-get-match-buffer file)))
     (blink-search-grep-pdf-do file page submatches)
     (unless match-file
       (add-to-list 'blink-search-grep-file-temp-buffers (current-buffer))))))

(defun blink-search-grep-pdf-preview (file page submatches)
  (if blink-search-preview-pdf-timer (cancel-timer blink-search-preview-pdf-timer))
  (setq blink-search-preview-pdf-timer
        (run-with-idle-timer blink-search-preview-pdf-idle-time nil
                             (lambda ()
                               (blink-search-grep-pdf-real-preview file page submatches)))))


(defun blink-search-grep-pdf-clean ()
  (blink-search-grep-file-clean)
  (if blink-search-preview-pdf-timer (cancel-timer blink-search-preview-pdf-timer)))

(provide 'blink-search-grep-pdf)
;;; blink-search-grep-pdf.el ends here

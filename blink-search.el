;;; blink-search.el --- Blink search  -*- lexical-binding: t; -*-

;; Filename: blink-search.el
;; Description: Blink search
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-10-23 15:23:53 +0800
;; Version: 0.1
;; Last-Updated: 2022-10-23 15:23:53 +0800
;;           By: Andy Stewart
;; URL: https://github.com/manateelazycat/blink-search
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6-dev"))
;;
;; Features that might be required by this library:
;;
;; Please check README
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
;; Blink-Search
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET blink-search RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'blink-search-epc)

(require 'recentf)
(require 'imenu)

(recentf-mode 1)

(defgroup blink-search nil
  "Blink-Search group."
  :group 'applications)

(defvar blink-search-server nil)
(defvar blink-search-python-file (expand-file-name "blink_search.py" (file-name-directory load-file-name)))
(defvar blink-search-server-port nil)
(defvar blink-search-epc-process nil)
(defvar blink-search-internal-process nil)
(defvar blink-search-internal-process-prog nil)
(defvar blink-search-internal-process-args nil)
(defvar blink-search-is-starting nil)
(defvar blink-search-stop-process-hook nil)
(defvar blink-search-window-configuration nil)
(defvar blink-search-start-buffer nil)
(defvar blink-search-input-buffer " *blink search input*")
(defvar blink-search-candidate-buffer " *blink search candidate*")
(defvar blink-search-backend-buffer " *blink search backend*")

(defvar blink-search-elisp-symbol-timer nil)
(defvar blink-search-elisp-symbol-size 0)

(defvar blink-search-recent-file-timer nil)
(defvar blink-search-recent-file-size 0)

(defvar blink-search-candidate-items nil)
(defvar blink-search-candidate-select-index nil)
(defvar blink-search-backend-items nil)
(defvar blink-search-backend-select-index nil)

(defcustom blink-search-common-directory '(("HOME" "~/"))
  "Common directory to search and open."
  :type 'cons)

(defcustom blink-search-name "*blink-search*"
  "Name of Blink-Search buffer."
  :type 'string)

(defcustom blink-search-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run lsp_bridge.py."
  :type 'string)

(defcustom blink-search-enable-debug nil
  "If you got segfault error, please turn this option.
Then Blink-Search will start by gdb, please send new issue with `*blink-search*' buffer content when next crash."
  :type 'boolean)

(defcustom blink-search-enable-log nil
  "Enable this option to print log message in `*blink-search*' buffer, default only print message header."
  :type 'boolean)

(defcustom blink-search-elisp-symbol-update-idle 5
  "The idle seconds to update elisp symbols."
  :type 'float
  :group 'blink-search)

(defcustom blink-search-recent-file-update-idle 4
  "The idle seconds to update recent files."
  :type 'float
  :group 'blink-search)

(defcustom blink-search-flash-line-delay .3
  "How many seconds to flash `blink-search-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'blink-search)

(defface blink-search-select-face
  '()
  "Face used to highlight the currently selected candidate.")

(defface blink-search-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'blink-search)

(defun blink-search--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p blink-search-server)
    (setq blink-search-server
          (blink-search-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (blink-search-epc-define-method mngr 'eval-in-emacs 'blink-search--eval-in-emacs-func)
               (blink-search-epc-define-method mngr 'get-emacs-var 'blink-search--get-emacs-var-func)
               (blink-search-epc-define-method mngr 'get-emacs-vars 'blink-search--get-emacs-vars-func)
               ))))
    (if blink-search-server
        (setq blink-search-server-port (process-contact blink-search-server :service))
      (error "[Blink-Search] blink-search-server failed to start")))
  blink-search-server)

(defun blink-search--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun blink-search--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (symbol-is-exist-p (boundp var-symbol))
         (var-value (if symbol-is-exist-p (symbol-value var-symbol) ""))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (if symbol-is-exist-p (prin1-to-string (booleanp var-value)) "nil")))
    (list var-value var-is-bool)))

(defun blink-search--get-emacs-vars-func (&rest vars)
  (mapcar #'blink-search--get-emacs-var-func vars))

(defun blink-search-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (blink-search-deferred-chain
    (blink-search-epc-call-deferred blink-search-epc-process (read method) args)))

(defun blink-search-get-theme-mode ()
  "Get theme mode, dark or light."
  (prin1-to-string (frame-parameter nil 'background-mode)))

(defun blink-search-color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (apply (lambda (r g b)
           (format "#%02x%02x%02x"
                   (ash r -8)
                   (ash g -8)
                   (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun blink-search-init-colors (&optional force)
  (let* ((is-dark-mode (string-equal (blink-search-get-theme-mode) "dark"))
         (blend-background (if is-dark-mode "#000000" "#AAAAAA"))
         (default-background (face-attribute 'default :background)))
    ;; Make sure menu follow the theme of Emacs.
    (when (or force (equal (face-attribute 'blink-search-select-face :background) 'unspecified))
      (set-face-background 'blink-search-select-face (blink-search-color-blend default-background blend-background 0.6)))
    (when (or force (equal (face-attribute 'blink-search-select-face :foreground) 'unspecified))
      (set-face-foreground 'blink-search-select-face (face-attribute 'font-lock-function-name-face :foreground)))))

(defun blink-search-restart-process ()
  "Stop and restart Blink-Search process."
  (interactive)
  (setq blink-search-is-starting nil)

  (blink-search-kill-process)
  (blink-search-start-process)
  (message "[Blink-Search] Process restarted."))

(defun blink-search-start-process ()
  "Start Blink-Search process if it isn't started."
  (setq blink-search-is-starting t)
  (unless (blink-search-epc-live-p blink-search-epc-process)
    ;; start epc server and set `blink-search-server-port'
    (blink-search--start-epc-server)
    (let* ((blink-search-args (append
                               (list blink-search-python-file)
                               (list (number-to-string blink-search-server-port))
                               )))

      ;; Set process arguments.
      (if blink-search-enable-debug
          (progn
            (setq blink-search-internal-process-prog "gdb")
            (setq blink-search-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" blink-search-python-command) blink-search-args)))
        (setq blink-search-internal-process-prog blink-search-python-command)
        (setq blink-search-internal-process-args blink-search-args))

      ;; Start python process.
      (let ((process-connection-type (not (blink-search--called-from-wsl-on-windows-p))))
        (setq blink-search-internal-process
              (apply 'start-process
                     blink-search-name blink-search-name
                     blink-search-internal-process-prog blink-search-internal-process-args)))
      (set-process-query-on-exit-flag blink-search-internal-process nil))))

(defun blink-search--called-from-wsl-on-windows-p ()
  "Check whether blink-search is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" blink-search-python-command)))

(defun blink-search-kill-process ()
  "Stop Blink-Search process and kill all Blink-Search buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'blink-search-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (blink-search--kill-python-process)

  (blink-search-stop-elisp-symbol-update)
  (blink-search-stop-recent-file-update)
  )

(add-hook 'kill-emacs-hook #'blink-search-kill-process)

(defun blink-search--kill-python-process ()
  "Kill Blink-Search background python process."
  (when (blink-search-epc-live-p blink-search-epc-process)
    ;; Cleanup before exit Blink-Search server process.
    (blink-search-call-async "cleanup")
    ;; Delete Blink-Search server process.
    (blink-search-epc-stop-epc blink-search-epc-process)
    ;; Kill *blink-search* buffer.
    (when (get-buffer blink-search-name)
      (kill-buffer blink-search-name))
    (setq blink-search-epc-process nil)
    (message "[Blink-Search] Process terminated.")))

(defun blink-search--first-start (blink-search-epc-port)
  "Call `blink-search--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq blink-search-epc-process (make-blink-search-epc-manager
                                  :server-process blink-search-internal-process
                                  :commands (cons blink-search-internal-process-prog blink-search-internal-process-args)
                                  :title (mapconcat 'identity (cons blink-search-internal-process-prog blink-search-internal-process-args) " ")
                                  :port blink-search-epc-port
                                  :connection (blink-search-epc-connect "localhost" blink-search-epc-port)
                                  ))
  (blink-search-epc-init-epc-layer blink-search-epc-process)
  (setq blink-search-is-starting nil)

  (blink-search-start-elisp-symbol-update)
  (blink-search-start-recent-file-update)
  (blink-search-buffer-list-update)
  
  (blink-search-init-search-dir)
  (blink-search-init-current-buffer)
  (blink-search-init-imenu)
  (blink-search-start))

(defvar blink-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'blink-search-quit)
    (define-key map (kbd "ESC ESC ESC") 'blink-search-quit)
    (define-key map (kbd "M-h") 'blink-search-quit)
    (define-key map (kbd "C-n") 'blink-search-candidate-select-next)
    (define-key map (kbd "C-p") 'blink-search-candidate-select-prev)
    (define-key map (kbd "M-n") 'blink-search-backend-select-next)
    (define-key map (kbd "M-p") 'blink-search-backend-select-prev)
    (define-key map (kbd "C-m") 'blink-search-do)
    map)
  "Keymap used by `blink-search-mode'.")

(define-derived-mode blink-search-mode text-mode "blink-search"
  ;; Kill all local variables.
  (kill-all-local-variables)
  ;; Switch new mode.
  (setq major-mode 'blink-search-mode)
  (setq mode-name "snails")
  ;; Injection keymap.
  (use-local-map blink-search-mode-map))

(defun blink-search ()
  (interactive)
  (blink-search-init-layout))

(defun blink-search-quit ()
  (interactive)
  (when blink-search-window-configuration
    (set-window-configuration blink-search-window-configuration)
    (setq blink-search-window-configuration nil)

    (setq blink-search-start-buffer nil)))

(defun blink-search-init-layout ()
  (setq blink-search-start-buffer (current-buffer))

  ;; Save window configuration.
  (unless blink-search-window-configuration
    (setq blink-search-window-configuration (current-window-configuration)))

  ;; Init color.
  (blink-search-init-colors)

  ;; Create buffers.
  (with-current-buffer (get-buffer-create blink-search-input-buffer)
    (erase-buffer)
    (blink-search-mode)
    (run-hooks 'blink-search-mode-hook)
    (add-hook 'after-change-functions 'blink-search-monitor-input nil t)

    (blink-search-disable-options nil)

    ;; Avoid background around input string.
    (face-remap-add-relative 'hl-line :background (face-background 'default))

    ;; Set window margin.
    (setq-local left-margin-width 1)
    (setq-local right-margin-width 1))

  (with-current-buffer (get-buffer-create blink-search-candidate-buffer)
    (erase-buffer)
    (blink-search-disable-options t)
    (setq-local truncate-lines t))

  (with-current-buffer (get-buffer-create blink-search-backend-buffer)
    (erase-buffer)
    (blink-search-disable-options t)
    (setq-local truncate-lines t))

  ;; Clean layout.
  (delete-other-windows)

  ;; Show input buffer.
  (split-window)
  (other-window -1)
  (switch-to-buffer blink-search-input-buffer)

  ;; Show candidate buffer.
  (split-window (selected-window) (line-pixel-height) 'below t)
  (other-window 1)
  (switch-to-buffer blink-search-candidate-buffer)

  ;; Show backend buffer.
  (split-window (selected-window) nil 'right t)
  (other-window 1)
  (switch-to-buffer blink-search-backend-buffer)

  ;; Select input window.
  (select-window (get-buffer-window blink-search-input-buffer))

  (blink-search-buffer-list-update)
  (blink-search-init-search-dir)
  (blink-search-init-current-buffer)
  (blink-search-init-imenu)
  (blink-search-start)

  ;; Start process.
  (unless blink-search-is-starting
    (blink-search-start-process)))

(defun blink-search-get-window-allocation (&optional window)
  "Get WINDOW allocation."
  (let* ((window-edges (window-pixel-edges window))
         (x (nth 0 window-edges))
         (y (+ (nth 1 window-edges)
               (if (version< emacs-version "27.0")
                   (window-header-line-height window)
                 (window-tab-line-height window))))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) (window-mode-line-height window) y)))
    (list x y w h)))

(defun blink-search-disable-options (&optional disable-cursor)
  "Disable many options for blink-search buffers."
  ;; Disable line numbers mode.
  (when display-line-numbers
    (setq-local display-line-numbers nil))
  ;; Disable tab-line.
  (when (version< "27.0" emacs-version)
    (setq-local tab-line-format nil))
  ;; Disable hl-line, header-line and mode-line in input buffer.
  (setq-local header-line-format nil)
  (setq-local mode-line-format nil)
  ;; Disable cursor type if option `disable-cursor' is non-nil.
  (when disable-cursor
    (setq-local cursor-type nil)))

(defun blink-search-monitor-input (_begin _end _length)
  "This is input monitor callback to hook `after-change-functions'."
  ;; Send new input to all backends when user change input.
  (when (string-equal (buffer-name) blink-search-input-buffer)
    (let* ((input (with-current-buffer blink-search-input-buffer
                    (buffer-substring-no-properties (point-min) (point-max)))))
      (blink-search-call-async "search" input (blink-search-get-row-number))
      )))

(defun blink-search-get-row-number ()
  (/ (nth 3 (blink-search-get-window-allocation (get-buffer-window blink-search-candidate-buffer))) (line-pixel-height)))

(defun blink-search-elisp-symbol-update ()
  "We need synchronize elisp symbols to Python side when idle."
  (let* ((symbols (all-completions "" obarray))
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

(defun blink-search-recent-file-update ()
  "We need synchronize recent files to Python side when idle."
  (let* ((files-size (length recentf-list)))
    ;; Only synchronize when new symbol created.
    (unless (equal blink-search-recent-file-size files-size)
      (blink-search-call-async "search_recent_file_update" recentf-list)
      (setq blink-search-recent-file-size files-size))))

(defun blink-search-start-recent-file-update ()
  (blink-search-recent-file-update)

  (unless blink-search-recent-file-timer
    (setq blink-search-recent-file-timer (run-with-idle-timer blink-search-recent-file-update-idle t #'blink-search-recent-file-update))))

(defun blink-search-stop-recent-file-update ()
  (when blink-search-recent-file-timer
    (cancel-timer blink-search-recent-file-timer)
    (setq blink-search-recent-file-timer nil)
    (setq blink-search-recent-file-size 0)))

(defun blink-search-buffer-list-update ()
  (when (blink-search-epc-live-p blink-search-epc-process)
    (if (featurep 'sort-tab)
        (blink-search-call-async "search_sort_buffer_list_update" (mapcar #'buffer-name (append sort-tab-visible-buffers (buffer-list))))
      (blink-search-call-async "search_buffer_list_update" (mapcar #'buffer-name (buffer-list))))))

(defun blink-search-encode-string (str)
  "Encode string STR with UTF-8 coding using Base64."
  (base64-encode-string (encode-coding-string str 'utf-8)))

(defun blink-search-init-current-buffer ()
  (when (blink-search-epc-live-p blink-search-epc-process)
    (blink-search-call-async "search_init_current_buffer"
                             (buffer-name blink-search-start-buffer)
                             (blink-search-encode-string
                              (with-current-buffer blink-search-start-buffer
                                (buffer-string))))))

(defun blink-search-init-search-dir ()
  (when (blink-search-epc-live-p blink-search-epc-process)
    (blink-search-call-async "search_init_search_dir"
                             (with-current-buffer blink-search-start-buffer
                               default-directory))))

(defun blink-search-start ()
  (when (blink-search-epc-live-p blink-search-epc-process)
    (blink-search-call-async "search" "" (blink-search-get-row-number))))

(defsubst blink-search-indent-pixel (xpos)
  "Return a display property that aligns to XPOS."
  `(space :align-to (,xpos)))

(defun blink-search-update-items (candidate-items candidate-select-index backend-items backend-select-index)
  (setq blink-search-candidate-items candidate-items)
  (setq blink-search-candidate-select-index candidate-select-index)
  (setq blink-search-backend-items backend-items)
  (setq blink-search-backend-select-index backend-select-index)

  (save-excursion
    (let* ((window-allocation (blink-search-get-window-allocation (get-buffer-window blink-search-candidate-buffer)))
           (window-width (nth 2 window-allocation)))
      (with-current-buffer blink-search-candidate-buffer
        (let* ((candidate-max-length (ceiling (/ window-width (window-font-width) 2)))
               (candidate-index 0))
          (erase-buffer)

          (when candidate-items
            (dolist (item candidate-items)
              (let* ((candidate (plist-get item :candidate))
                     (candidate-length (length candidate))
                     (display-candiate (if (<= candidate-length candidate-max-length)
                                           candidate
                                         (concat
                                          (substring candidate 0 (ceiling (* candidate-max-length 0.4)))
                                          "..."
                                          (substring candidate (- candidate-length (ceiling (* candidate-max-length 0.4))) candidate-length))))
                     (number (format "(%s)" (plist-get item :number)))
                     (backend (plist-get item :backend))
                     (padding-right 5)
                     candidate-line)
                (setq candidate-line (concat
                                      (format " %s %s" display-candiate number)
                                      (propertize " " 'display
                                                  (blink-search-indent-pixel
                                                   (- window-width
                                                      (* (window-font-width) (+ (string-width backend) padding-right)))))
                                      (propertize (format "%s " backend)
                                                  'face (if (equal candidate-index candidate-select-index) 'blink-search-select-face 'font-lock-doc-face))
                                      "\n"
                                      ))

                (when (equal candidate-index candidate-select-index)
                  (add-face-text-property 0 (length candidate-line) 'blink-search-select-face 'append candidate-line))

                (insert candidate-line)

                (setq candidate-index (1+ candidate-index))
                ))))
        (with-current-buffer blink-search-backend-buffer
          (let ((backend-index 0))
            (erase-buffer)

            (when backend-items
              (dolist (item backend-items)
                (let* (backend-line)
                  (setq backend-line
                        (concat
                         (propertize (format " %s " item) 'face (if (equal backend-index backend-select-index) 'blink-search-select-face 'font-lock-doc-face))
                         (propertize " " 'display (blink-search-indent-pixel window-width))
                         "\n"
                         ))

                  (when (equal backend-index backend-select-index)
                    (add-face-text-property 0 (length backend-line) 'blink-search-select-face 'append backend-line))

                  (insert backend-line)

                  (setq backend-index (1+ backend-index)))))))
        ))))

(defun blink-search-candidate-select-next ()
  (interactive)
  (blink-search-call-async "select_next_candidate_item"))

(defun blink-search-candidate-select-prev ()
  (interactive)
  (blink-search-call-async "select_prev_candidate_item"))

(defun blink-search-backend-select-next ()
  (interactive)
  (blink-search-call-async "select_next_backend_item"))

(defun blink-search-backend-select-prev ()
  (interactive)
  (blink-search-call-async "select_prev_backend_item"))

(defun blink-search-rg-do (file line column)
  (find-file file)
  (goto-line line)
  (blink-search-goto-column column)
  (recenter)
  (blink-search-flash-line))

(defun blink-search-current-buffer-do (buffer line column)
  (switch-to-buffer buffer)
  (goto-line line)
  (blink-search-goto-column column)
  (recenter)
  (blink-search-flash-line))

(defun blink-search-imenu-do (point)
  (goto-char point)
  (recenter)
  (blink-search-flash-line))

(defun blink-search-common-directory-do (dir)
  (if (featurep 'eaf-file-manager)
      (eaf-open-in-file-manager dir)
    (find-file dir)))

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

(defun blink-search-goto-column (column)
  "This function use for jump to correct column positions in multi-byte strings.
Such as, mixed string of Chinese and English.

Function `move-to-column' can't handle mixed string of Chinese and English correctly."
  (let ((scan-column 0)
        (first-char-point (point)))

    (while (> column scan-column)
      (forward-char 1)
      (setq scan-column (string-bytes (buffer-substring first-char-point (point)))))))

(defun blink-search-flash-line ()
  (let ((pulse-iterations 1)
        (pulse-delay blink-search-flash-line-delay))
    ;; Flash match line.
    (pulse-momentary-highlight-one-line (point) 'blink-search-font-lock-flash)
    ))

(defun blink-search-init-imenu ()
  (when (blink-search-epc-live-p blink-search-epc-process)
    (blink-search-call-async "search_init_imenu"
                             (with-current-buffer blink-search-start-buffer
                               (blink-search-imenu-get-candidates)))))

(defun blink-search-imenu-get-candidates ()
  (mapcar (lambda (info) (list (car info) (marker-position (cdr info))))
          (let* ((index (ignore-errors (imenu--make-index-alist t))))
            (when index
              (blink-search-imenu-build-candidates
               (delete (assoc "*Rescan*" index) index))))))

(defun blink-search-imenu-build-candidates (alist)
  (cl-remove-if
   (lambda (c)
     (or (string-equal (car c) "Types")
         (string-equal (car c) "Variables")
         ))
   (cl-loop for elm in alist
            nconc (cond
                   ((imenu--subalist-p elm)
                    (blink-search-imenu-build-candidates
                     (cl-loop for (e . v) in (cdr elm) collect
                              (cons
                               e
                               (if (integerp v) (copy-marker v) v)))))
                   ((listp (cdr elm))
                    (and elm (list elm)))
                   (t
                    (and (cdr elm)
                         (setcdr elm (pcase (cdr elm)
                                       ((and ov (pred overlayp))
                                        (copy-overlay ov))
                                       ((and mk (or (pred markerp)
                                                    (pred integerp)))
                                        (copy-marker mk))))
                         (list elm)))))))

(defun blink-search-do ()
  (interactive)
  (when (and (> (length blink-search-candidate-items) 0)
             (> (length blink-search-backend-items) 0))
    (let* ((backend-name (plist-get (nth blink-search-candidate-select-index blink-search-candidate-items) :backend))
           (candidate (nth blink-search-backend-select-index blink-search-backend-items)))
      (blink-search-quit)

      (pcase backend-name
        ("Elisp Symbol" (blink-search-elisp-symbol-do candidate))
        ("Recent File" (find-file candidate))
        ("Buffer List" (switch-to-buffer candidate))
        ("EAF Browser History" (eaf-open-browser (car (last (split-string candidate)))))
        (t (blink-search-call-async "search_do" backend-name candidate))
        ))))

(provide 'blink-search)

;;; blink-search.el ends here

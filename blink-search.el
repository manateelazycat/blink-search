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
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6"))
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
(require 'blink-search-icon)

(require 'recentf)
(require 'imenu)
(require 'pulse)

(recentf-mode 1)

(defvar blink-search-backend-path (expand-file-name "backend"
                                                    (if load-file-name
                                                        (file-name-directory load-file-name)
                                                      default-directory)))
(eval-and-compile
  (add-to-list 'load-path blink-search-backend-path t))

(defvar blink-search-idle-update-list nil)
(defvar blink-search-start-update-list nil)

(require 'blink-search-elisp-symbol)
(require 'blink-search-imenu)
(require 'blink-search-grep-file)
(require 'blink-search-current-buffer)
(require 'blink-search-recent-file)
(require 'blink-search-buffer-list)
(require 'blink-search-grep-pdf)
(require 'blink-search-pdf)

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
(defvar blink-search-start-path-name nil)
(defvar blink-search-start-buffer-name nil)
(defvar blink-search-start-buffer-directory nil)
(defvar blink-search-continue-directory nil)
(defvar blink-search-start-keyword nil)
(defvar blink-search-input-buffer " *blink search input*")
(defvar blink-search-tooltip-buffer " *blink search tooltip*")
(defvar blink-search-candidate-buffer " *blink search candidate*")
(defvar blink-search-backend-buffer " *blink search backend*")

(defvar blink-search-candidate-items nil)
(defvar blink-search-candidate-select-index nil)
(defvar blink-search-backend-items nil)
(defvar blink-search-backend-select-index nil)
(defvar blink-search-backend-name nil)
(defvar blink-search-item-index nil)
(defvar blink-search-items-number nil)
(defvar blink-search-backend-number nil)

(defvar blink-search-posframe-preview-window nil)
(defvar blink-search-posframe-emacs-frame nil)
(defvar blink-search-posframe-frame nil)

(defcustom blink-search-enable-posframe nil
  "Enable posframe."
  :type 'boolean
  :group 'blink-search)

(defcustom blink-search-posframe-width-ratio 0.6
  "The width of posframe."
  :type 'number
  :group 'blink-search)

(defcustom blink-search-posframe-height-ratio 0.6
  "The height of posframe."
  :type 'number
  :group 'blink-search)

(defcustom blink-search-posframe-standalone nil
  "If non-nil, standalone display posframe."
  :type 'number
  :group 'blink-search)

(defvar blink-search-search-backends nil
  "Default backends for blink search, which is a list of backend names, nil for all backends defined in python side.")

(defcustom blink-search-enable-icon t
  "Show icon in menu."
  :type 'boolean
  :group 'blink-search)

(defcustom blink-search-common-directory '(("HOME" "~/"))
  "Common directory to search and open."
  :type 'cons)

(defcustom blink-search-history-path (expand-file-name (concat user-emacs-directory (file-name-as-directory "blink-search") "history.txt"))
  "The path to store search history."
  :type 'string)

(defcustom blink-search-db-path (expand-file-name "blink-search.db" user-emacs-directory)
  "Key Value store database path."
  :type 'string)

(defcustom blink-search-db-table "kvstore"
  "Key Value store table."
  :type 'string)

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

(defcustom blink-search-flash-line-delay .3
  "How many seconds to flash `blink-search-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'blink-search)


(defcustom blink-search-browser-function #'eaf-open-browser
  "The function used to open web browser for google suggestion"
  :type 'function
  :set (lambda (sym val)
         (defalias #'blink-search-browser-function val)))

(defcustom blink-search-file-manager 'eaf-file-manager
  "File-manager to open file or directory."
  :type 'symbol
  :options '(eaf-file-manager dired)
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
  ;; Avoid background around input string.
  (with-current-buffer blink-search-input-buffer
    (face-remap-add-relative 'hl-line :background (face-background 'default)))

  (let* ((is-dark-mode (string-equal (blink-search-get-theme-mode) "dark"))
         (blend-background (if is-dark-mode "#000000" "#AAAAAA"))
         (default-background (face-attribute 'default :background)))
    ;; Make sure menu follow the theme of Emacs.
    (when (or force (equal (face-attribute 'blink-search-select-face :background) 'unspecified))
      (set-face-background 'blink-search-select-face (blink-search-color-blend default-background blend-background 0.6)))
    (when (or force (equal (face-attribute 'blink-search-select-face :foreground) 'unspecified))
      (set-face-foreground 'blink-search-select-face (face-attribute 'font-lock-function-name-face :foreground)))))

(defun blink-search-reset-colors (&rest args)
  ;; Reset colors.
  (when (get-buffer blink-search-input-buffer)
    (blink-search-init-colors t)
    (blink-search-render)))

(if (daemonp)
    ;; The :background of 'default is unavailable until frame is created in
    ;; daemon mode.
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (advice-add #'load-theme :after #'blink-search-reset-colors)
                ;; Compensation for missing the first `load-thme' in
                ;; `after-init-hook'.
                (blink-search-reset-colors)))
  (advice-add #'load-theme :after #'blink-search-reset-colors))

;;;###autoload
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

;;;###autoload
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
                                  :connection (blink-search-epc-connect "127.0.0.1" blink-search-epc-port)
                                  ))
  (blink-search-epc-init-epc-layer blink-search-epc-process)
  (setq blink-search-is-starting nil)

  (dolist (update-func blink-search-idle-update-list)
    (funcall update-func))

  (dolist (update-func blink-search-start-update-list)
    (funcall update-func)))

(defvar blink-search-quick-keys
  '("h" "l" "u" "i" "y"
    "," "." ";" "/" "'"
    "r" "v" "g" "t" "c"
    "7" "8" "9" "0"
    "H" "L" "U" "I" "Y"
    "s" "a" "e" "q"
    "1" "2" "3" "4"
    "[" "]")
  "This list keys to access candidate quickly, all those keys is prefix with Alt key,

you need customize option if some 'M + key' conflict with your command.")

(defvar blink-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'blink-search-quit)
    (define-key map (kbd "ESC ESC ESC") 'blink-search-quit)
    (define-key map (kbd "C-n") 'blink-search-candidate-select-next)
    (define-key map (kbd "C-p") 'blink-search-candidate-select-prev)
    (define-key map (kbd "M-n") 'blink-search-backend-select-next)
    (define-key map (kbd "M-p") 'blink-search-backend-select-prev)
    (define-key map (kbd "M-j") 'blink-search-candidate-group-select-next)
    (define-key map (kbd "M-k") 'blink-search-candidate-group-select-prev)
    (define-key map (kbd "C-m") 'blink-search-do)
    (define-key map (kbd "C-M-m") 'blink-search-preview)
    (define-key map (kbd "C-M-n") 'blink-search-preview-next)
    (define-key map (kbd "C-M-p") 'blink-search-preview-prev)
    (define-key map (kbd "C-j") 'blink-search-parent)
    (define-key map (kbd "C-l") 'blink-search-continue)
    (define-key map (kbd "M-w") 'blink-search-copy)

    (dolist (key blink-search-quick-keys)
      (define-key map (kbd (format "M-%s" key)) 'blink-search-quick-do))
    map)
  "Keymap used by `blink-search-mode'.")

(define-derived-mode blink-search-mode text-mode "blink-search"
  ;; Kill all local variables.
  (kill-all-local-variables)
  ;; Switch new mode.
  (setq major-mode 'blink-search-mode)
  (setq mode-name "blink-search")
  ;; Injection keymap.
  (use-local-map blink-search-mode-map)
  (when (featurep 'evil)
    (evil-set-initial-state 'blink-search-mode 'emacs))
  )

;;;###autoload
(defun blink-search-quit ()
  (interactive)
  (blink-search-call-async "clean")

  (when blink-search-window-configuration
    (set-window-configuration blink-search-window-configuration)
    (setq blink-search-window-configuration nil)

    (when blink-search-enable-posframe
      (make-frame-invisible blink-search-posframe-frame)
      (select-frame-set-input-focus blink-search-posframe-emacs-frame)

      ;; popup will call 'eaf--mac-focus-change'
      ;; which causes switched buffer
      ;; switch back with (set-window-configuration (frame-parameter (selected-frame) 'eaf--mac-frame))
      (when (and (eq system-type 'darwin) (featurep 'eaf)
                 (eaf-emacs-not-use-reparent-technology)
                 eaf--mac-safe-focus-change)
        (set-frame-parameter  blink-search-posframe-emacs-frame
                              'eaf--mac-frame nil)))

    (setq blink-search-start-buffer nil)
    (setq blink-search-start-buffer-name nil)
    (setq blink-search-start-path-name nil)
    (setq blink-search-start-buffer-directory nil)

    (setq blink-search-continue-directory nil)))

;;;###autoload
(defun blink-search-quick-do ()
  (interactive)
  (let* ((event-type (event-basic-type last-command-event))
         (event-string (if (characterp event-type)
                           (string event-type)
                         (error "Unexpected input")))
         (candidate-index (cl-position event-string blink-search-quick-keys :test 'equal)))
    (when (< candidate-index (length blink-search-candidate-items))
      (let* ((backend-name (plist-get (nth candidate-index blink-search-candidate-items) :backend))
             (candidate-info (plist-get (nth candidate-index blink-search-candidate-items) :candidate))
             (candidate (blink-search-get-candidate-text candidate-info)))
        (blink-search-quit)
        (blink-search-call-async "search_do" backend-name candidate)
        ))))

;;;###autoload
(defun blink-search (&optional arg)
  "Start blink-search.

blink-search will search current symbol if you call this function with `C-u' prefix."
  (interactive "P")

  (setq blink-search-start-buffer (current-buffer))
  (setq blink-search-start-buffer-name (buffer-name (current-buffer)))
  (setq blink-search-start-path-name
        (if (derived-mode-p 'eaf-mode)
            eaf--buffer-url
          (buffer-file-name (current-buffer))))
  (setq blink-search-start-buffer-directory (with-current-buffer (current-buffer)
                                              (expand-file-name default-directory)))

  (setq blink-search-start-keyword (cond
                                    ((region-active-p)
                                     (buffer-substring-no-properties (region-beginning) (region-end)))
                                    (t
                                     (if arg (or (thing-at-point 'symbol t) "") ""))))

  ;; Save window configuration.
  (unless blink-search-window-configuration
    (setq blink-search-window-configuration (current-window-configuration)))

  ;; Create buffers.
  (with-current-buffer (get-buffer-create blink-search-input-buffer)
    (erase-buffer)

    (blink-search-mode)

    (run-hooks 'blink-search-mode-hook)
    (add-hook 'after-change-functions 'blink-search-monitor-input nil t)

    (blink-search-disable-options nil)

    ;; Set window margin.
    (setq-local left-margin-width 1)
    (setq-local right-margin-width 1))

  (with-current-buffer (get-buffer-create blink-search-tooltip-buffer)
    (erase-buffer)

    (blink-search-disable-options t)

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

  ;; Init color.
  (blink-search-init-colors)

  (if blink-search-enable-posframe
      (blink-search-init-popup-layout)
    (blink-search-init-bottom-layout))

  (dolist (update-func blink-search-start-update-list)
    (funcall update-func))

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

(defun blink-search-get-row-number ()
  (/ (nth 3 (blink-search-get-window-allocation (get-buffer-window blink-search-candidate-buffer))) (line-pixel-height)))

(defun blink-search-monitor-input (_begin _end _length)
  "This is input monitor callback to hook `after-change-functions'."
  ;; Send new input to all backends when user change input.
  (when (string-equal (buffer-name) blink-search-input-buffer)
    (let* ((input (string-trim
                   (with-current-buffer blink-search-input-buffer
                     (buffer-substring-no-properties (point-min) (point-max))))))
      (cond ((or (string-prefix-p "#" input)
                 (string-prefix-p "#" input))
             (blink-search-call-async "search" (substring input 1) (blink-search-get-row-number) (list "Current Buffer")))
            ((or (string-prefix-p "!" input)
                 (string-prefix-p "！" input))
             (blink-search-call-async "search" (substring input 1) (blink-search-get-row-number) (list "Grep File")))
            ((or (string-prefix-p ";" input)
                 (string-prefix-p "；" input))
             (blink-search-call-async "search" (substring input 1) (blink-search-get-row-number) (list "Grep PDF")))
            ((or (string-prefix-p ":" input)
                 (string-prefix-p "：" input))
             (blink-search-call-async "search" (substring input 1) (blink-search-get-row-number) (list "PDF")))
            (t
             (blink-search-call-async "search" input (blink-search-get-row-number) blink-search-search-backends))))))

(defun blink-search-start ()
  (when (blink-search-epc-live-p blink-search-epc-process)
    (unless (string-empty-p blink-search-start-keyword)
      (message "[blink-search] Search symbol '%s'" blink-search-start-keyword))
    (blink-search-call-async "search" blink-search-start-keyword (blink-search-get-row-number) blink-search-search-backends)))

(defsubst blink-search-indent-pixel (xpos)
  "Return a display property that aligns to XPOS."
  `(space :align-to (,xpos)))

(defun blink-search-render-candidate (backend-name candidate candidate-max-length)
  (let ((candidate-length (length candidate)))
    (cond ((string-equal backend-name "Recent File")
           (file-name-nondirectory candidate))
          ((member backend-name '("Current Buffer" "Grep File" "Grep PDF" "EAF Browser" "History" "Common Directory" "PDF"))
           (blink-search-truncate-candidate-with-ellipsis candidate candidate-max-length candidate-max-length))
          (t
           (if (<= candidate-length candidate-max-length)
               candidate
             (concat (substring candidate 0 (/ candidate-max-length 2))
                     "..."
                     (substring candidate (- (length candidate) (/ candidate-max-length 2)) (length candidate))))))))

(defun blink-search-truncate-candidate-with-ellipsis (candidate candidate-max-length truncate-length)
  (let ((candidate-length (length candidate)))
    (if (<= candidate-length truncate-length)
        candidate
      (concat (substring candidate 0 (- truncate-length (length "..."))) "..."))))

(defun blink-search-select-window (window)
  (when (window-live-p window)
    (select-window window)))

(defun blink-search-show-backend-window ()
  (unless (get-buffer-window blink-search-backend-buffer)
    (save-excursion
      (if (and blink-search-enable-posframe blink-search-posframe-preview-window)
          (set-window-buffer blink-search-posframe-preview-window blink-search-backend-buffer)
        (blink-search-select-window (get-buffer-window blink-search-candidate-buffer))
        (split-window (selected-window) nil 'right t)
        (other-window 1))
      (switch-to-buffer blink-search-backend-buffer)

      (blink-search-select-window (get-buffer-window blink-search-input-buffer)))))

(defun blink-search-hide-backend-window ()
  (when (get-buffer-window blink-search-backend-buffer)
    (save-excursion
      (unless blink-search-enable-posframe
        (delete-window (get-buffer-window blink-search-backend-buffer)))

      (blink-search-select-window (get-buffer-window blink-search-input-buffer)))))

(defun blink-search-get-candidate-text (candidate-info)
  ;; Remove ^M char at end of line.
  (replace-regexp-in-string
   "\r+$" ""
   (format
    "%s"
    (if (stringp candidate-info)
        candidate-info
      (plist-get candidate-info :text)))))

(defun blink-search-get-candidate-matches (candidate-info)
  (unless (stringp candidate-info)
    (plist-get candidate-info :matches)))

(defun blink-search-render ()
  (let ((candidate-items blink-search-candidate-items)
        (candidate-select-index blink-search-candidate-select-index)
        (backend-items blink-search-backend-items)
        (backend-select-index blink-search-backend-select-index)
        (backend-name blink-search-backend-name)
        (search-items-index blink-search-item-index)
        (search-items-number blink-search-items-number)
        (backend-number blink-search-backend-number))
    (save-excursion
      (let* ((window-allocation (blink-search-get-window-allocation (get-buffer-window blink-search-candidate-buffer)))
             (window-width (nth 2 window-allocation)))
        (with-current-buffer blink-search-tooltip-buffer
          (let* ((tooltip-window-allocation (blink-search-get-window-allocation (get-buffer-window blink-search-input-buffer)))
                 (tooltip-window-width (nth 2 tooltip-window-allocation))
                 tooltip-line)
            (erase-buffer)

            (setq tooltip-line
                  (concat
                   (propertize (format "%s [%s/%s]" backend-name search-items-index search-items-number)
                               'face font-lock-constant-face)
                   (propertize " search prefix: " 'face font-lock-type-face)
                   (propertize "#" 'face font-lock-type-face)
                   (propertize " buffer " 'face font-lock-keyword-face)
                   (propertize "!" 'face font-lock-type-face)
                   (propertize " directory " 'face font-lock-keyword-face)
                   (propertize ";" 'face font-lock-type-face)
                   (propertize " pdfs " 'face font-lock-keyword-face)
                   (propertize ":" 'face font-lock-type-face)
                   (propertize " pdf " 'face font-lock-keyword-face)
                   ))

            (insert tooltip-line)
            ))

        (with-current-buffer blink-search-candidate-buffer
          (let* ((candidate-max-length (ceiling (* (/ window-width (frame-char-width)) 0.6)))
                 (candidate-index 0))
            (erase-buffer)

            (when candidate-items
              (dolist (item candidate-items)
                (let* ((candidate-info (plist-get item :candidate))
                       (candidate (blink-search-get-candidate-text candidate-info))
                       (candidate-length (length candidate))
                       (matches (blink-search-get-candidate-matches candidate-info))
                       (backend (plist-get item :backend))
                       (display-candiate (blink-search-render-candidate backend candidate candidate-max-length))
                       (padding-right 5)
                       (icon (cdr (assoc backend blink-search-icon-alist)))
                       (icon-default (cdr (assoc t blink-search-icon-alist)))
                       (display-icon (if icon icon icon-default))
                       (icon-text (blink-search-icon-build (nth 0 display-icon) (nth 1 display-icon) (nth 2 display-icon)))
                       candidate-prefix
                       candidate-prefix-length
                       candidate-line)

                  (setq candidate-prefix
                        (concat
                         icon-text
                         (propertize (format "%s " (nth candidate-index blink-search-quick-keys)) 'face 'font-lock-type-face)))

                  (setq candidate-prefix-length (length candidate-prefix))

                  (setq candidate-line
                        (concat
                         candidate-prefix
                         (if (> backend-number 1)
                             (format "%s " display-candiate)
                           (format "%s " candidate))
                         (if (> backend-number 1)
                             (propertize " " 'display
                                         (blink-search-indent-pixel
                                          (- window-width
                                             (if (fboundp 'string-pixel-width)
                                                 (string-pixel-width (format "%s%s" (make-string 2 ?\s) backend))
                                               (* (frame-char-width) (+ (string-width backend) padding-right)))
                                             )))
                           (propertize " " 'display (blink-search-indent-pixel window-width)))
                         (when (> backend-number 1)
                           (propertize (format "%s " backend)
                                       'face (if (equal candidate-index candidate-select-index) 'blink-search-select-face 'font-lock-doc-face)))
                         "\n"
                         ))

                  ;; Highlight match strings.
                  (when (and matches
                             (equal backend-number 1))
                    (dolist (match matches)
                      (let ((match-column
                             (let (match-start-point
                                   match-end-point)
                               ;; We need use `blink-search-goto-column' to handle mixed string of Chinese and English correctly.
                               (with-temp-buffer
                                 (insert (substring candidate-line candidate-prefix-length))
                                 (goto-char (point-min))
                                 (blink-search-goto-column (nth 0 match))
                                 (backward-char 1)
                                 (setq match-start-point (+ (point) candidate-prefix-length))
                                 (goto-char (point-min))
                                 (blink-search-goto-column (nth 1 match))
                                 (backward-char 1)
                                 (setq match-end-point (+ (point) candidate-prefix-length))
                                 (list match-start-point match-end-point)))))
                        (add-face-text-property (nth 0 match-column) (nth 1 match-column) 'font-lock-type-face 'append candidate-line)
                        )))

                  (when (equal candidate-index candidate-select-index)
                    (add-face-text-property 0 (length candidate-line) 'blink-search-select-face 'append candidate-line))

                  (insert candidate-line)

                  (setq candidate-index (1+ candidate-index))
                  ))))


          (with-current-buffer blink-search-backend-buffer
            (erase-buffer)

            (when (> backend-number 1)
              (let ((backend-index 0))

                (when backend-items
                  (dolist (candidate-info backend-items)
                    (let* ((candidate (if (stringp candidate-info) candidate-info (plist-get candidate-info :text)))
                           (matches (unless (stringp candidate-info) (plist-get candidate-info :matches)))
                           backend-line)

                      (setq backend-line
                            (concat
                             (propertize (format " %s " candidate) 'face (if (equal backend-index backend-select-index) 'blink-search-select-face 'font-lock-doc-face))
                             (propertize " " 'display (blink-search-indent-pixel window-width))
                             "\n"
                             ))

                      (when (equal backend-index backend-select-index)
                        (add-face-text-property 0 (length backend-line) 'blink-search-select-face 'append backend-line))

                      (insert backend-line)

                      (setq backend-index (1+ backend-index))))))))
          )))))

(defun blink-search-update-items (candidate-items
                                  candidate-select-index
                                  backend-items backend-select-index backend-name
                                  search-items-index search-items-number backend-number)
  ;; Don't pop completion candidates when
  (when (get-buffer-window blink-search-input-buffer)
    (setq blink-search-candidate-items candidate-items)
    (setq blink-search-candidate-select-index candidate-select-index)
    (setq blink-search-backend-items backend-items)
    (setq blink-search-backend-select-index backend-select-index)
    (setq blink-search-backend-name backend-name)
    (setq blink-search-item-index search-items-index)
    (setq blink-search-items-number search-items-number)
    (setq blink-search-backend-number backend-number)

    (if (> backend-number 1)
        (blink-search-show-backend-window)
      (blink-search-hide-backend-window))

    (blink-search-render)))

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

(defun blink-search-candidate-group-select-next ()
  (interactive)
  (blink-search-call-async "select_next_candidate_group"))

(defun blink-search-candidate-group-select-prev ()
  (interactive)
  (blink-search-call-async "select_prev_candidate_group"))

(defun blink-search-goto-column (column)
  "This function use for jump to correct column positions in multi-byte strings.
Such as, mixed string of Chinese and English.

Function `move-to-column' can't handle mixed string of Chinese and English correctly."
  (let ((scan-column 0)
        (first-char-point (point)))

    (while (> column scan-column)
      (forward-char 1)
      (setq scan-column (string-bytes (buffer-substring first-char-point (point)))))))

(defun blink-search-flash-locate ()
  (ignore-errors
    (recenter)
    (let ((pulse-iterations 1)
          (pulse-delay blink-search-flash-line-delay))
      ;; Flash match line.
      (pulse-momentary-highlight-one-line (point) 'blink-search-font-lock-flash)
      )))

(defun blink-search-preview-select-window ()
  (if blink-search-enable-posframe
      (unless (blink-search-select-window blink-search-posframe-preview-window)
        (blink-search-select-window (get-buffer-window blink-search-candidate-buffer))
        (split-window (selected-window) nil 'right t)
        (other-window 1)
        (setq blink-search-posframe-preview-window (selected-window)))
    (other-window -1)))


(cl-defmacro blink-search-select-input-window (&rest body)
  `(let* ((inhibit-message t)
          (input-window (get-buffer-window blink-search-input-buffer)))
     (when input-window
       (select-window input-window)
       (blink-search-preview-select-window)

       ,@body

       (select-window input-window)
       )))

(defun blink-search-select-start-buffer (buffer)
  (unless blink-search-enable-posframe
    (blink-search-select-input-window
     (switch-to-buffer buffer))))

(defun blink-search-get-select-backend-name ()
  (plist-get (nth blink-search-candidate-select-index blink-search-candidate-items) :backend))

(defun blink-search-get-select-candidate ()
  (let* ((candidate-info
          (if (get-buffer-window blink-search-backend-buffer)
              (nth blink-search-backend-select-index blink-search-backend-items)
            (plist-get (nth blink-search-candidate-select-index blink-search-candidate-items) :candidate)))
         (candidate (blink-search-get-candidate-text candidate-info)))
    candidate))

(defun blink-search-open-file (candidate)
  (cond ((and (eq blink-search-file-manager 'eaf-file-manager)
              (file-directory-p candidate)
              (featurep 'eaf-file-manager))
         (eaf-open-in-file-manager candidate))
        ((and (eq blink-search-file-manager 'dired)
              (file-directory-p candidate))
         (dired candidate))
        (t (find-file candidate))))

(defun blink-search-action (action)
  (interactive)
  (when (> (length blink-search-candidate-items) 0)
    (let* ((backend-name (blink-search-get-select-backend-name))
           (candidate (blink-search-get-select-candidate)))
      (blink-search-quit)

      (blink-search-call-async action backend-name candidate))))

(defun blink-search-do ()
  (interactive)
  (blink-search-action "search_do"))

(defun blink-search-parent ()
  (interactive)
  (blink-search-action "search_parent"))

(defun blink-search-preview ()
  (interactive)
  (blink-search-call-async "select_candidate_item"))

(defun blink-search-preview-next ()
  (interactive)
  (blink-search-candidate-select-next)
  (blink-search-call-async "select_candidate_item"))

(defun blink-search-preview-prev ()
  (interactive)
  (blink-search-candidate-select-prev)
  (blink-search-call-async "select_candidate_item"))

(defun blink-search-continue ()
  (interactive)
  (when (> (length blink-search-candidate-items) 0)
    (let* ((backend-name (blink-search-get-select-backend-name))
           (candidate (blink-search-get-select-candidate)))

      (blink-search-call-async "search_continue" backend-name candidate))))

(defun blink-search-copy ()
  (interactive)
  (blink-search-action "search_copy"))

(defun blink-search-continue-search (path)
  (if (file-directory-p path)
      (progn
        (setq blink-search-continue-directory path)
        (with-current-buffer blink-search-input-buffer
          (erase-buffer))
        (message "[blink-search] continue search at: %s" path))
    (message "[blink-search] '%s' is not directory, can't continue search." path)))

(defun blink-search-posframe-show (buffer)
  (let* ((posframe-height (round (* (frame-height) blink-search-posframe-height-ratio)))
         (posframe-width (round (* (frame-width) blink-search-posframe-width-ratio))))
    (apply #'posframe-show
           (get-buffer buffer)
           :poshandler #'posframe-poshandler-frame-center
           (list
            :max-height posframe-height
            :min-height posframe-height
            :min-width  posframe-width
            :max-width  posframe-width
            :border-width 2
            :border-color "gray"
            :accept-focus (equal buffer blink-search-input-buffer)
            ))))


(defun blink-search-init-bottom-layout ()
  ;; Clean layout.
  ;; NOTE: `ignore-errors' make sure start always successfully,
  ;; even no buffer does exist when Emacs first start.
  (ignore-errors
    (delete-other-windows)
    (split-window)
    (other-window 1))

  ;; Show input buffer.
  (switch-to-buffer blink-search-input-buffer)

  ;; Show tooltip buffer.
  (split-window (selected-window) (line-pixel-height) 'below t)
  (split-window (selected-window) nil 'right t)
  (other-window 1)
  (switch-to-buffer blink-search-tooltip-buffer)

  ;; Show candidate buffer.
  (other-window 1)
  (switch-to-buffer blink-search-candidate-buffer)

  ;; Show backend buffer.
  (split-window (selected-window) nil 'right t)
  (other-window 1)
  (switch-to-buffer blink-search-backend-buffer)

  ;; Select input window.
  (blink-search-select-window (get-buffer-window blink-search-input-buffer)))

(defun blink-search-init-popup-layout ()
  (setq blink-search-posframe-emacs-frame (selected-frame))
  (setq blink-search-posframe-frame (blink-search-posframe-show blink-search-input-buffer))

  (when blink-search-posframe-standalone
    (set-frame-parameter blink-search-posframe-frame 'parent-frame nil))

  ;; Make popup frame's font same as Emacs frame one.
  (with-selected-frame blink-search-posframe-frame
    (set-frame-font (with-selected-frame blink-search-posframe-emacs-frame
                      (face-attribute 'default :font))))

  (select-frame-set-input-focus blink-search-posframe-frame)

  (when (equal (length (window-list)) 1)
    (split-window (selected-window) (line-pixel-height) 'below t)
    (split-window (selected-window) nil 'right t)

    (dolist (buffer (list blink-search-tooltip-buffer
                          blink-search-candidate-buffer
                          blink-search-backend-buffer))
      (when (equal buffer blink-search-backend-buffer)
        (split-window (selected-window) nil 'right t))
      (other-window 1)
      (switch-to-buffer buffer)
      (when (equal buffer blink-search-backend-buffer)
        (setq blink-search-posframe-preview-window (selected-window)))))

  (blink-search-select-window (get-buffer-window blink-search-input-buffer))
  (setq-local cursor-type 'box)
  (set-window-margins (get-buffer-window blink-search-input-buffer) 1 1))


(add-to-list 'blink-search-start-update-list #'blink-search-start t)

(provide 'blink-search)

;;; blink-search.el ends here

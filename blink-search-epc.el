;;; epcs.el --- EPC Server              -*- lexical-binding: t; -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro blink-search-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar blink-search-deferred-debug nil
  "Debug output switch.")

(defvar blink-search-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun blink-search-deferred-log (&rest args)
  "[internal] Debug log function."
  (when blink-search-deferred-debug
    (with-current-buffer (get-buffer-create "*blink-search-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" blink-search-deferred-debug-count (apply #'format args)))))
    (cl-incf blink-search-deferred-debug-count)))

(defvar blink-search-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro blink-search-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`blink-search-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal blink-search-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar blink-search-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar blink-search-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `blink-search-deferred-post-task' and `blink-search-deferred-worker'.")

(defun blink-search-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`blink-search-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack blink-search-deferred-queue)
    (blink-search-deferred-log "QUEUE-POST [%s]: %s" (length blink-search-deferred-queue) pack)
    (run-at-time blink-search-deferred-tick-time nil 'blink-search-deferred-worker)
    d))

(defun blink-search-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when blink-search-deferred-queue
    (let* ((pack (car (last blink-search-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq blink-search-deferred-queue (nbutlast blink-search-deferred-queue))
      (condition-case err
          (setq value (blink-search-deferred-exec-task d which arg))
        (error
         (blink-search-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: blink-search-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `blink-search-deferred-resignal')
;; cancel      : a canceling function (default `blink-search-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct blink-search-deferred-object
  (callback 'identity)
  (errorback 'blink-search-deferred-resignal)
  (cancel 'blink-search-deferred-default-cancel)
  next status value)

(defun blink-search-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun blink-search-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (blink-search-deferred-log "CANCEL : %s" d)
  (setf (blink-search-deferred-object-callback d) 'identity)
  (setf (blink-search-deferred-object-errorback d) 'blink-search-deferred-resignal)
  (setf (blink-search-deferred-object-next d) nil)
  d)

(defun blink-search-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (blink-search-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "blink-search-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (blink-search-deferred-object-callback d)
                    (blink-search-deferred-object-errorback d)))
        (next-deferred (blink-search-deferred-object-next d)))
    (cond
     (callback
      (blink-search-deferred-condition-case err
                                            (let ((value (funcall callback arg)))
                                              (cond
                                               ((blink-search-deferred-object-p value)
                                                (blink-search-deferred-log "WAIT NEST : %s" value)
                                                (if next-deferred
                                                    (blink-search-deferred-set-next value next-deferred)
                                                  value))
                                               (t
                                                (if next-deferred
                                                    (blink-search-deferred-post-task next-deferred 'ok value)
                                                  (setf (blink-search-deferred-object-status d) 'ok)
                                                  (setf (blink-search-deferred-object-value d) value)
                                                  value))))
                                            (error
                                             (cond
                                              (next-deferred
                                               (blink-search-deferred-post-task next-deferred 'ng err))
                                              (t
                                               (blink-search-deferred-log "ERROR : %S" err)
                                               (message "deferred error : %S" err)
                                               (setf (blink-search-deferred-object-status d) 'ng)
                                               (setf (blink-search-deferred-object-value d) err)
                                               err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (blink-search-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (blink-search-deferred-resignal arg)))))))

(defun blink-search-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (blink-search-deferred-object-next prev) next)
  (cond
   ((eq 'ok (blink-search-deferred-object-status prev))
    (setf (blink-search-deferred-object-status prev) nil)
    (let ((ret (blink-search-deferred-exec-task
                next 'ok (blink-search-deferred-object-value prev))))
      (if (blink-search-deferred-object-p ret) ret
        next)))
   ((eq 'ng (blink-search-deferred-object-status prev))
    (setf (blink-search-deferred-object-status prev) nil)
    (let ((ret (blink-search-deferred-exec-task next 'ng (blink-search-deferred-object-value prev))))
      (if (blink-search-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun blink-search-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-blink-search-deferred-object :callback callback)
    (make-blink-search-deferred-object)))

(defun blink-search-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (blink-search-deferred-exec-task d 'ok arg))

(defun blink-search-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (blink-search-deferred-exec-task d 'ng arg))

(defun blink-search-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (blink-search-deferred-post-task d 'ok arg))

(defun blink-search-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (blink-search-deferred-callback-post (blink-search-deferred-new callback))."
  (let ((d (if callback
               (make-blink-search-deferred-object :callback callback)
             (make-blink-search-deferred-object))))
    (blink-search-deferred-callback-post d arg)
    d))

(defun blink-search-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-blink-search-deferred-object :callback callback)))
    (blink-search-deferred-set-next d nd)))

(defun blink-search-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-blink-search-deferred-object :errorback callback)))
    (blink-search-deferred-set-next d nd)))

(defvar blink-search-epc-debug nil)

(defun blink-search-epc-log (&rest args)
  (when blink-search-epc-debug
    (with-current-buffer (get-buffer-create "*blink-search-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun blink-search-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar blink-search-epc-uid 1)

(defun blink-search-epc-uid ()
  (cl-incf blink-search-epc-uid))

(defvar blink-search-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct blink-search-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun blink-search-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return blink-search-epc-connection object."
  (blink-search-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (blink-search-epc-uid))
         (connection-name (format "blink-search-epc con %s" connection-id))
         (connection-buf (blink-search-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-blink-search-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (blink-search-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (blink-search-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (blink-search-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun blink-search-epc-process-sentinel (connection process msg)
  (blink-search-epc-log "!! Process Sentinel [%s] : %S : %S"
                        (blink-search-epc-connection-name connection) process msg)
  (blink-search-epc-disconnect connection))

(defun blink-search-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (blink-search-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (blink-search-epc-connection-process connection)))
    (blink-search-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun blink-search-epc-disconnect (connection)
  (let ((process (blink-search-epc-connection-process connection))
        (buf (blink-search-epc-connection-buffer connection))
        (name (blink-search-epc-connection-name connection)))
    (blink-search-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (blink-search-epc-log "!! Disconnected finished [%s]" name)))

(defun blink-search-epc-process-filter (connection process message)
  (blink-search-epc-log "INCOMING: [%s] [%S]" (blink-search-epc-connection-name connection) message)
  (with-current-buffer (blink-search-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (blink-search-epc-process-available-input connection process)))

(defun blink-search-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (blink-search-deferred-new callback)
             (blink-search-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun blink-search-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (blink-search-deferred-callback-post d event))))

(defun blink-search-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (blink-search-epc-net-have-input-p)
      (let ((event (blink-search-epc-net-read-or-lose process))
            (ok nil))
        (blink-search-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'blink-search-epc-signal-send
                         (cons (blink-search-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (blink-search-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (blink-search-epc-process-available-input connection process)))))))

(defun blink-search-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (blink-search-epc-net-decode-length))))

(defun blink-search-epc-net-read-or-lose (_process)
  (condition-case error
      (blink-search-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun blink-search-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (blink-search-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun blink-search-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun blink-search-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct blink-search-epc-manager
  "Root object that holds all information related to an EPC activity.

`blink-search-epc-start-epc' returns this object.

title          : instance name for displaying on the `blink-search-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : blink-search-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct blink-search-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar blink-search-epc-live-connections nil
  "[internal] A list of `blink-search-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun blink-search-epc-server-process-name (uid)
  (format "blink-search-epc-server:%s" uid))

(defun blink-search-epc-server-buffer-name (uid)
  (format " *%s*" (blink-search-epc-server-process-name uid)))

(defun blink-search-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (blink-search-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (blink-search-epc-disconnect (blink-search-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 blink-search-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq blink-search-epc-live-connections (delete mngr blink-search-epc-live-connections))
    ))

(defun blink-search-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun blink-search-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an blink-search-epc-connection instance."
  (let* ((mngr mngr)
         (conn (blink-search-epc-manager-connection mngr))
         (channel (blink-search-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (blink-search-epc-log "SIG CALL: %S" args)
                    (apply 'blink-search-epc-handler-called-method ,mngr (blink-search-epc-args args))))
               (return
                . (lambda (args)
                    (blink-search-epc-log "SIG RET: %S" args)
                    (apply 'blink-search-epc-handler-return ,mngr (blink-search-epc-args args))))
               (return-error
                . (lambda (args)
                    (blink-search-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'blink-search-epc-handler-return-error ,mngr (blink-search-epc-args args))))
               (epc-error
                . (lambda (args)
                    (blink-search-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'blink-search-epc-handler-epc-error ,mngr (blink-search-epc-args args))))
               (methods
                . (lambda (args)
                    (blink-search-epc-log "SIG METHODS: %S" args)
                    (blink-search-epc-handler-methods ,mngr (caadr args))))
               ) do
             (blink-search-epc-signal-connect channel method body))
    (push mngr blink-search-epc-live-connections)
    mngr))

(defun blink-search-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (blink-search-epc-manager-connection mngr)))
    (blink-search-epc-net-send conn (cons method messages))))

(defun blink-search-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (blink-search-epc-manager-methods mngr)
           if (eq method-name (blink-search-epc-method-name i))
           do (cl-return i)))

(defun blink-search-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (blink-search-epc-manager-methods mngr)
                  collect
                  (list
                   (blink-search-epc-method-name i)
                   (or (blink-search-epc-method-arg-specs i) "")
                   (or (blink-search-epc-method-docstring i) "")))))
    (blink-search-epc-manager-send mngr 'return uid info)))

(defun blink-search-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (blink-search-epc-manager-methods mngr))
           (method (blink-search-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (blink-search-epc-log "ERR: No such method : %s" name)
        (blink-search-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (blink-search-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((blink-search-deferred-object-p ret)
                (blink-search-deferred-nextc ret
                                             (lambda (xx) (blink-search-epc-manager-send mngr 'return uid xx))))
               (t (blink-search-epc-manager-send mngr 'return uid ret))))
          (error
           (blink-search-epc-log "ERROR : %S" err)
           (blink-search-epc-manager-send mngr 'return-error uid err))))))))

(defun blink-search-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (blink-search-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (blink-search-epc-manager-sessions mngr) ret)))

(defun blink-search-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (blink-search-epc-manager-sessions mngr))))
    (cond
     (pair
      (blink-search-epc-log "RET: id:%s [%S]" uid args)
      (blink-search-epc-manager-remove-session mngr uid)
      (blink-search-deferred-callback (cdr pair) args))
     (t                                 ; error
      (blink-search-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun blink-search-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (blink-search-epc-manager-sessions mngr))))
    (cond
     (pair
      (blink-search-epc-log "RET-ERR: id:%s [%S]" uid args)
      (blink-search-epc-manager-remove-session mngr uid)
      (blink-search-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (blink-search-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun blink-search-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (blink-search-epc-manager-sessions mngr))))
    (cond
     (pair
      (blink-search-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (blink-search-epc-manager-remove-session mngr uid)
      (blink-search-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (blink-search-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun blink-search-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (blink-search-epc-uid))
        (sessions (blink-search-epc-manager-sessions mngr))
        (d (blink-search-deferred-new)))
    (push (cons uid d) sessions)
    (setf (blink-search-epc-manager-sessions mngr) sessions)
    (blink-search-epc-manager-send mngr 'call uid method-name args)
    d))

(defun blink-search-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-blink-search-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (blink-search-epc-manager-methods mngr))))
    (setf (blink-search-epc-manager-methods mngr) methods)
    method))

(defun blink-search-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'blink-search-epc-nothing))
    (blink-search-deferred-chain
     d
     (blink-search-deferred-nextc it
                                  (lambda (x) (setq result x)))
     (blink-search-deferred-error it
                                  (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'blink-search-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (blink-search-epc-connection-process (blink-search-epc-manager-connection mngr))
         0 blink-search-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun blink-search-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (blink-search-epc-sync mngr (blink-search-epc-call-deferred mngr method-name args)))

(defun blink-search-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (blink-search-epc-connection-process (blink-search-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar blink-search-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`blink-search-epc-manager' instance]).
When the server process accepts the client connection, the
`blink-search-epc-manager' instance is created and stored in this variable
`blink-search-epc-server-client-processes'. This variable is used for the management
purpose.")

;; blink-search-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `blink-search-epc-manager' instances
(cl-defstruct blink-search-epc-server name process port connect-function)

(defvar blink-search-epc-server-processes nil
  "[internal] A list of ([process object] . [`blink-search-epc-server' instance]).
This variable is used for the management purpose.")

(defun blink-search-epc-server-get-manager-by-process (proc)
  "[internal] Return the blink-search-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in blink-search-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun blink-search-epc-server-accept (process)
  "[internal] Initialize the process and return blink-search-epc-manager object."
  (blink-search-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (blink-search-epc-uid))
         (connection-name (format "blink-search-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-blink-search-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (blink-search-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (blink-search-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (blink-search-epc-process-sentinel connection p e)))
    (make-blink-search-epc-manager :server-process process :port t
                                   :connection connection)))

(defun blink-search-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (blink-search-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (blink-search-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (blink-search-epc-server-accept process)))
            (push (cons process mngr) blink-search-epc-server-client-processes)
            (blink-search-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (blink-search-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (blink-search-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process blink-search-epc-server-client-processes)) _d)
        (when pair
          (blink-search-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (blink-search-epc-stop-epc (cdr pair))
          (setq blink-search-epc-server-client-processes
                (assq-delete-all process blink-search-epc-server-client-processes))
          ))
      nil))))

(defun blink-search-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "BLINK-SEARCH EPC Server %s" (blink-search-epc-uid)))
       (buf (blink-search-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (blink-search-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-blink-search-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          blink-search-epc-server-processes)
    main-process))

(provide 'blink-search-epc)
;;; blink-search-epc.el ends here

;;;---------------------------------------------------------------------------
;;; This file is intended for testing through CI scripts.  However, in order
;;; to run it locally, you can set the environment variable DEBUG_PR_TESTS
;;; to some value that is not NIL and run this from the command line.
;;; You will need to figure out how to load quicklisp before doing this.
;;; e.g.
;;; DEBUG_PR_TESTS=yes \
;;; sbcl --no-userinit --load "${HOME}/quicklisp/setup.lisp" --load "do-test.lisp"
;;;---------------------------------------------------------------------------


(defpackage testing-plan-repair-rewrite
    (:use common-lisp))

(in-package :testing-plan-repair-rewrite)

(require :asdf)
(require :quicklisp)
(defparameter *debug* (uiop:getenv "DEBUG_PR_TESTS"))
(asdf:initialize-source-registry '(:source-registry (:directory :here)
                                   (:tree (:here "ext/"))
                                   :inherit-configuration))
(ql:quickload "fiveam-asdf")
(ql:quickload "iterate")
(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")
(asdf:load-system "hddl-utils")
(ql:quickload "shop3")
(ql:quickload "fiveam")

(defun leave-lisp (message return)
  (fresh-line *error-output*)
  (when message
    (format *error-output* message)
    (terpri *error-output*))
  (finish-output *error-output*)
  (finish-output *standard-output*)
  (uiop:quit return))

(defmacro quit-on-error (&body body)
  `(call-quitting-on-error (lambda () ,@body)))

(defun call-quitting-on-error (thunk)
  "Unless the environment variable DEBUG_ASDF_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (flet ((quit (c desc)
           (format *error-output* "~&Encountered ~a during test.~%~a~%" desc c)
           (cond
            ;; decline to handle the error.
            ((ignore-errors (funcall (find-symbol "GETENV" :asdf) "DEBUG_ASDF_TEST"))
             (format t "~&Interactive mode (DEBUG_ASDF_TEST) -- Invoke debugger.~%")
             (invoke-debugger c))
            (t
             (finish-output *standard-output*)
             (finish-output *trace-output*)
             (format *error-output* "~&ABORTING:~% ~S~%" c)
             (uiop:print-condition-backtrace c)
             (format *error-output* "~&ABORTING:~% ~S~%" c)
             (finish-output *error-output*)
             (leave-lisp "~&Script failed~%" 1)))))
    (handler-bind
        ((error (lambda (c)
                  (unless *debug*
                   (quit c  "ERROR"))))
         (storage-condition
          (lambda (c) (quit c "STORAGE-CONDITION")))
         (serious-condition (lambda (c)
                              (unless *debug*
                                (quit c "Other SERIOUS-CONDIITON")))))
      (funcall thunk)
      (format t "~&Script succeeded~%")
      t)))

(quit-on-error
 (let ((fiveam:*on-error* :backtrace))
   (asdf:test-system "plan-repair-rewrite")))

(unless *debug*
 (uiop:quit 0))

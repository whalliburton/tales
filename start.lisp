(in-package :cl-user)

(setf sb-sys:*tty* (make-two-way-stream sb-sys:*stdin* sb-sys:*stdout*))

(defparameter *original-standard-output* *standard-output*)

(defparameter *required-sbcl-version* "1.0.55")

(defun split-sbcl-version (string)
  (loop with start = 0
        with end = (length string)
        while (< start end)
        for (n p) = (multiple-value-list
                        (parse-integer string :start start :junk-allowed t))
        when n
        collect n
        do (setf start (1+ p))))

(defun sbcl-version-as-number (str)
  (destructuring-bind (a b c &optional (d 0) &rest more)
      (split-sbcl-version str)
    (declare (ignore more))
    (+ (* a 1000000000000)
       (* b 100000000)
       (* c 10000)
       d)))

(let ((this-version (lisp-implementation-version)))
  (when (< (sbcl-version-as-number this-version)
           (sbcl-version-as-number *required-sbcl-version*))
    (format t "SBCL version of at least ~A is required. This SBCL is ~A.~%"
            *required-sbcl-version* this-version)
    (quit)))

(unless *project* (error "No project set."))

(defparameter *start-time* (get-universal-time))

(format t "starting the ~(~A~)~%" *project*)

(defparameter *backtrace-exit-on-error* t)
(defparameter *run-all-tests* nil)
(defparameter *report-code-coverage* nil)

(flet ((split-arg (arg)
         (let ((pos (position #\= arg)))
           (if pos
             (list (subseq arg 2 pos) (subseq arg (1+ pos)))
             (list arg "")))))
  (let ((args (cdr sb-ext:*posix-argv*)))
    (when args
      (format t "additional arguments:~%~:{  ~A~30T~A~%~}~%" (mapcar #'split-arg args))
      (when (member 'debug args :test #'string-equal)
        (setf *backtrace-exit-on-error* nil))
      (when (member 'run-tests args :test #'string-equal)
        (format t "~%** Running all tests and exiting. **~2%")
        (setf *run-all-tests* t))
      (when (member 'cover args :test #'string-equal)
        (format t "~%** Performing code coverage and exiting. **~2%")
        (setf *report-code-coverage* t)))))

(format t "~A ~A~%"
        (lisp-implementation-type)
        (lisp-implementation-version))

(require :asdf)

(setf asdf:*central-registry*
      '("/mnt/projects/site-lisp/systems/" "/mnt/projects/systems/"))

(defparameter *start-column* 1)

(defvar *asdf-register-system* #'asdf::register-system)

;; this is incorrect, we want to be able to print when we are actually
;; compling or loading the system not just registering
(let ((stream *standard-output*))
  (handler-bind ((style-warning #'muffle-warning))
    (defun asdf::register-system (&rest args)
      (if (> *start-column* 40)
        (progn
          (terpri stream)
          (setf *start-column* 0))
        (incf *start-column*))
      (write-char #\. stream)
      (finish-output stream)
      (apply *asdf-register-system* args))))

(defmacro with-backtrace-exit (&body body)
  (if *backtrace-exit-on-error*
    `(handler-bind
         ((error
           (lambda (e)
             (format t "~%ERROR  ~A~%" e)
             (sb-debug:backtrace)
             (sb-ext:quit :unix-status -1))))
       ,@body)
    `(progn ,@body)))

(unless (boundp '*loaded-from-core*)
  (let ((prefix "loading the libraries..."))
    (setf *start-column* (1- (length prefix)))
    (princ prefix))
  (finish-output)

  (defun support-libraries (systems)
    (loop for system in systems
          append (cdadar (slot-value (asdf:find-system system) 'asdf::do-first))))

  (defparameter *excluded-libraries*
    '(:helpers :deck-client :event-client :sail :test-client :manual :weblite))

  (let ((errors-filename
         (format nil "~(~A~)-library-errors.txt" *project*)))
    (with-open-file (stream errors-filename :direction :output :if-exists :supersede)
      (handler-bind ((sb-ext:compiler-note #'muffle-warning))
        (let ((*error-output* stream)
              (*standard-output* stream)
              (*trace-output* stream)
              (*debug-io* stream)
              (asdf:*asdf-verbose* nil)
              (*compile-verbose* nil)
              (*compile-print* nil)
              current-library)
          (handler-case
              (dolist (m (support-libraries (cons *project* *excluded-libraries*)))
                (unless (member m *excluded-libraries*)
                  (setf current-library m)
                  (require m)))
            (error (c)
              (format *original-standard-output*
                      "~%ERROR Compiling Library ~A~%~A~%See the ~S file for more information~%"
                      current-library c errors-filename)
              (format t "~%ERROR Compiling Libraries~%~A~%" c)
              (sb-debug:backtrace)
              (close stream) ;; must close before QUIT
              (sb-ext:quit :unix-status -1)))))))

  (defparameter *loaded-from-core* t)
  (format t "~2%saving a core...~2%")
  (ensure-directories-exist "/mnt/projects/cores/")
  (save-lisp-and-die (format nil "/mnt/projects/cores/~(~A.core~)" *project*)))

(format t "PID ~A~%" (sb-unix:unix-getpid))

;; stefil is capturing the now closed library.errors.txt file
(setf hu.dwim.stefil::*test-run-standard-output* *standard-output*)

(format t "loading the ~(~A~)...~%" *project*)

(with-backtrace-exit
  (let ((asdf:*asdf-verbose* nil)
        (*compile-verbose* nil)
        (*compile-print* nil))
    (require 'helpers)))

(load "/mnt/projects/acorn/config.lisp")

(let ((count 0))
  (defun progress-macroexpand-hook (expander form env)
    (multiple-value-bind (column remainder) (floor count 1000)
      (when (> column 60) (terpri) (setf count 0))
      (when (zerop count) (write-string "  "))
      (incf count)
      (when (zerop remainder) (write-char #\.))
      (finish-output)
      (funcall expander form env))))

(when *report-code-coverage*
  (require :sb-cover)
  (declaim (optimize sb-cover:store-coverage-data))
  (setf *run-all-tests* t))

(let ((*macroexpand-hook* #'progress-macroexpand-hook))
  (with-backtrace-exit
    (let ((asdf:*asdf-verbose* nil)
          (*compile-verbose* nil)
          (*compile-print* nil))
      (handler-bind ((sb-ext:compiler-note #'muffle-warning))
        (require *project*)))))

(helpers:start-swank)

(format t "~%initializing...~%")

(funcall (find-symbol "INITIALIZE" *project*))

(when *run-all-tests*
  (format t "Running all tests...~2%")
  (funcall (find-symbol "RUN-TESTS" :helpers)))

(when *report-code-coverage*
  (let ((dir "/mnt/projects/coverage-reports/" ))
    (ensure-directories-exist dir)
    (sb-cover:report dir)))

(when (or *report-code-coverage* *run-all-tests*)
  (sb-ext:quit))

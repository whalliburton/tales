(in-package :tales-js)

(defpsmacro with-id ((var id) &body body)
  `(let ((,var (get-by-id ,id)))
     (if ,var
       (progn
         ,@body))))

(defpsmacro plusp (el)
  `(> ,el 0))

(defpsmacro console (&rest rest)
  `(cond
     ((=== *browser* :firefox) ((@ console log) ,@rest))
     ((or (=== *browser* :chrome) (=== *browser* :trident))
      ((@ console log)
       (+ ,@(butlast (loop for arg in rest
                           nconc (list arg " "))))))))

(defun ensure-string (symbol-or-string)
  (etypecase symbol-or-string
    (symbol (symbol-name symbol-or-string))
    (string symbol-or-string)))

(defun string-starts-with (string prefix &key (test #'char=))
  "Returns true if STRING starts with PREFIX."
  (let ((prefix (ensure-string prefix))
        (string (ensure-string string)))
    (let ((mismatch (mismatch prefix string :test test)))
      (or (not mismatch) (= mismatch (length prefix))))))

(defun this-swap (from to)
  (cond
    ((eql from 'this) to)
    (t
     (let ((sfrom (symbol-name from))
           (sto (symbol-name to)))
       (and (string-starts-with sfrom "THIS.")
            (intern (concatenate 'string sto "." (subseq sfrom 5))))))))

(defun subthis (this tree)
  (labels ((s (subtree)
             (or (and (symbolp subtree) (this-swap subtree this))
                 (cond ((atom subtree) subtree)
                       (t (let ((car (s (car subtree)))
                                (cdr (s (cdr subtree))))
                            (if (and (eq car (car subtree))
                                     (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr))))))))
    (s tree)))

(defpsmacro defun-trace (name args &rest body)
  (let* ((sname (ps::symbol-to-js-string name))
         (tname (ps-gensym name))
         (this (ps-gensym "this"))
         (arg-names (loop for arg in args
                          unless (eq arg '&optional)
                            collect (if (consp arg) (car arg) arg)))
         (argpairs
          (loop for arg in arg-names
                nconc (list (ps::symbol-to-js-string arg) arg))))
    `(progn
       (defun ,tname (,this ,@args)
         ,@(subthis this body))
       (defun ,name ,arg-names
         (console *trace-level* ,sname ":" ,@argpairs)
         (incf *trace-level*)
         (let ((rtn (,tname this ,@arg-names)))
           (decf *trace-level*)
           (console *trace-level* ,sname "returned" rtn)
           (return rtn))))))

(defparameter *js-file*
  (ps*
   '(progn

     (defvar *trace-level* 0)

     (defvar *browser*
       (cond
         ((plusp ((@ (@ navigator user-agent) index-of) "Chrome")) :chrome)
         ((plusp ((@ (@ navigator user-agent) index-of) "Gecko")) :firefox)
         ((plusp ((@ (@ navigator user-agent) index-of) "MSIE")) :trident)
         (t :unknown)))

     (defun get-by-id (id)
       (return ((@ document get-element-by-id) id)))

     (defun show (id)
       (with-id (o id)
         (setf (@ o style visibility) "visible")))

     (defun hide (id)
       (with-id (o id)
         (setf (@ o style visibility) "hidden")))

     (defun visit (id)
      (setf (@ window location) (+ "/?id=" id)))

     (defun word (id index)
      (setf (@ window location) (+ "/?id=" id "&index=" index)))

     (defun word-text (id index)
       (setf (@ window location) (+ "/?id=" id "&word=" index)))

     )))

(defun js-file () *js-file*)



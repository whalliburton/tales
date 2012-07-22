(in-package :tales)

(defparameter *tales-html* (slurp-file (tales-file "documents/Beelzebub.htm")))
(defparameter *tales-raw*
  (nthcdr 173 (fourth (closure-html:parse *tales-html* (chtml:make-lhtml-builder)))))

(defun process-raw ()
  (iter
   (for el in *tales-raw*)
   (when (consp el)
     (cond
       ((eq (car el) :h1)
        (when chapter
          (collect chapter into chapters)
          (setf chapter nil))
        (collect (list :chapter (third el)) into chapter))
       ((eq (car el) :h2) (collect (list :title (third el)) into chapter))
       ((eq (car el) :p)
        (collect
            (if (string= (second (first (second el))) "PageNo")
              (list :page
                    (let ((raw (string-trim '(#\[ #\]) (last1 el))))
                     (parse-integer raw :start (position #\space raw :from-end t))))
              (list :p (third el)))
          into chapter))))
   (finally
    (return (nconc chapters (list chapter))))))

(defparameter *tales* (process-raw))



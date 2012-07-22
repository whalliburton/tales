(in-package :tales)

(defvar *word-frequency* nil)

(defun find-word (text)
  (deck:search `((:node "word" (:= "text" ,text))) :first-one t))

(defun list-word-paragraph-edges (word-id)
  (mapcar #'caadr
          (deck:get-children word-id :edge-type "parent" :with-edges t :return-ids t)))

(defun word-frequency (word-id)
  (iter (for edge in (list-word-paragraph-edges word-id))
        (summing (deck:get-field edge "count"))))

(defun create-words-index ()
  (setf *word-frequency* (make-hash-table :test 'equal))
  (iter (for word in (deck:search "word"))
        (let ((frequency (word-frequency (id word)))
              (text (field-value word "text")))
          (setf (gethash text *word-frequency*) frequency)
          (format t "~A ~40T~A~%" text frequency))))

(defun list-word-frequency (&optional sort-by-frequency)
  (let ((alphabetical (sort
                       (iter (for (k v) in-hashtable *word-frequency*)
                             (collect (list k v)))
                       #'string< :key #'first)))
    (if sort-by-frequency
      (stable-sort alphabetical #'< :key #'second)
      alphabetical)))

(defun render-words-index (sort)
  (render-page
   "Words Index"
   (:h1 "Words")
   (:table :class "word-frequency"
    (iter (for (word frequency) in (list-word-frequency))
          (htm (:tr :class "word-link"
                    :onclick (format nil "visit(\"~A\");" (get-word-id word))
                    (:td (esc word))
                    (:td (str frequency))))))))


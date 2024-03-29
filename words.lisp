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
                       #'dictionary< :key #'first)))
    (if sort-by-frequency
      (stable-sort alphabetical #'< :key #'second)
      alphabetical)))

(defun dictionary< (a b)
  (flet ((try (el) (handler-case (parse-integer el) (error () nil))))
    (let ((ai (try a))
          (bi (try b)))
      (if ai
        (if bi
          (< ai bi)
          t)
        (if bi
          nil
          (string< a b))))))

(defvar *words-alphabetical* nil)
(defvar *words-by-frequency* nil)
(defvar *words-by-length* nil)

(defun initialize-word-lists ()
  (create-words-index)
  (setf *words-alphabetical* (list-word-frequency)
        *words-by-frequency* (list-word-frequency t)
        *words-by-length* (stable-sort (copy-list *words-alphabetical*)
                                       #'< :key (lambda (el) (length (car el))))))

(defun pos-string (word)
  (let ((pos (pos word)))
    (or (second (assoc word pos :test 'string-equal))
        (format nil "~{~A~^,~}" (mapcar 'second pos)))))

(defun render-words-index (sort show-pos)
  (render-page
   "Words Index"
   (:h1 "Words")
   (create-button-strip stream
                        `(("contents" "go(\"/\");")
                          ("alphabetical" "go(\"/words?sort=alpha\");"
                                          ,(or (and (null sort) (null show-pos))
                                               (equal sort "alpha")))
                          ("by frequency" "go(\"/words?sort=freq\");" ,(equal sort "freq"))
                          ("show pos" "go(\"/words?pos=true\");" ,show-pos)
                          ("by length" "go(\"/words?sort=length\");" ,(equal sort "length"))))
   (:br)
   (:table :class "word-frequency"
           (iter (for (word frequency) in
                      (cond
                        ((and sort (string= sort "freq")) *words-by-frequency*)
                        ((and sort (string= sort "length")) *words-by-length*)
                        (t *words-alphabetical*)))
                 (htm (:tr :class "word-link"
                           :onclick (format nil "visit(\"~A\");" (get-word-id word))
                           (:td (esc word))
                           (:td (str frequency))
                           (:td (esc (if show-pos (pos-string word) "")))))))
   (when show-pos
     (htm (:br) (:h2 "Parts of Speech Key"))
     (print-pos-tags :html t :stream stream))))

(in-package :tales)

(defvar *tales-book* nil)

(defun start-session ()
  (setf *deck-id* (start-deck-session "demo" "demo"))
  (start-printer-session))

(defun create-templates ()
  (iter (for name in '("book" "chapter" "paragraph" "sentence" "phrase" "word" "child"))
        (ignore-errors (deck:delete-template name)))
  (deck:add-node-template "book" '(("title" :string) ("author" :string)))
  (deck:add-node-template "chapter" '(("title" :string) ("number" :integer)))
  (deck:add-node-template "paragraph" '(("text" :string) ("chapter" :integer) ("number" :integer)))
  (deck:add-node-template "sentence" '(("text" :string)))
  (deck:add-node-template "phrase" '(("text" :string)))
  (deck:add-node-template "word" '(("text" :string)))
  (deck:add-edge-template "child" nil nil
                          :fields '(("count" :integer :default 1))
                          :insert-into-list t :reverse-name "parent"))

(defun fetch-parent (id)
  (deck:search `((:node :any (:= :id ,(id id))) "parent") :first-one t))

(defun fetch-previous (parent id)
  (deck:node-before parent id))

(defun fetch-next (parent id)
  (deck:node-after parent id))

(defun split-paragraph (text)
  (iter (with rtn)
        (for char in-string text)
        (if (alphanumericp char)
          (collect char into acc)
          (progn
            (when acc
              (push (coerce acc 'string) rtn)
              (setf acc nil))
            (push (coerce (list char) 'string) rtn)))
        (finally
         (when acc (push (coerce acc 'string) rtn))
         (return (nreverse rtn)))))

(defparameter *ignore-words* '("in" "a" "or" "the" "of" "is" "and" "but" "not" "at" "if"))

(defun paragraph-words (text)
  (iter (for word in (split-paragraph text))
        (when (and (> (length word) 1) (not (member word *ignore-words* :test #'string=)))
          (collect word))))

(defparameter *word-ids* (make-hash-table :test 'equal))

(defun get-word-id (word)
  (let ((word (string-downcase word)))
    (or (gethash word *word-ids*)
        (setf (gethash word *word-ids*) (deck:add-node "word" `(("text" ,word)))))))

(define-condition edge-already-exists (deck-client::rpc-error) ())

(defun build (&optional (chapters :all))
  (create-templates)
  (let ((book (deck:add-node "book" '(("title" "Beelzebub's Tales to His Grandson")
                                      ("author" "G.I. Gurdjieff")))))
    (setf *tales-book* book)
    (build-chapters chapters)))

(defun build-chapters (chapters &optional (book *tales-book*))
  (iter (for raw in *tales*)
        (for count from 1)
        (when (or (eq chapters :all) (member count (ensure-list chapters) :test #'eql))
          (let* ((title (second (second raw)))
                 (chapter (deck:add-node "chapter" `(("title" ,title) ("number" ,count)))))
            (format t "~A~%" title)
            (deck:add-edge "child" book chapter)
            (let ((paragraph-count 0))
              (iter (for el in raw)
                    (when (eq (first el) :p)
                      (let* ((text (second el))
                             (paragraph (deck:add-node
                                         "paragraph" `(("text" ,text)
                                                       ("chapter" ,(id chapter))
                                                       ("number" ,(incf paragraph-count))))))
                        (format t "  ~A~%" (helpers:princ-with-ellipses-to-string text 40))
                        (deck:add-edge "child" chapter paragraph)
                        (iter (for word in (paragraph-words text))
                              (handler-case
                                  (deck:add-edge "child" paragraph (get-word-id word))
                                (edge-already-exists ()
                                  (let ((edge (car (deck:get-edges-between
                                                    paragraph (get-word-id word)))))
                                    (deck:set-field edge "count" (1+ (field-value edge "count")))
                                    ))))))))))))

(defun list-chapters () (deck:search "chapter"))

(defun delete-chapter (chapter-id)
  (iter (for paragraph in (deck:get-children chapter-id :node-types '("paragraph") :return-ids t))
        (deck:delete-node paragraph))
  (deck:delete-node chapter-id))

(defmethod sail:serialize-replacement ((fields-base fields-base))
  (id fields-base))

(defun clear-build ()
  (iter (for chapter in (mapcan #'deck:search '("book" "chapter" "paragraph" "sentence" "word")))
        (deck:delete-node (id chapter)))
  (setf *word-ids* (make-hash-table :test 'equal)))


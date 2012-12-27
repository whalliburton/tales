(in-package :tales)

(defmacro render-page (title &rest body)
  `(with-html-output-to-string (stream)
     (:html
      (:head
       (:title (str ,title))
       (:link :rel "stylesheet" :type "text/css" :href "/css/tales.css")
       (:script :src "/js/tales.js" :type "text/javascript"))
     (:body
      ,@body))))

(defun render-book-page (book)
  (render-page
   (field-value book "title")
   (:div :class "book-title" (str (field-value book "title")))
   (:div :class "author" (fmt "By ~A" (field-value book "author")))
   (:br) (:br)
   (:h1 "Chapters")
   (iter (for chapter in (deck:search `(("book" (:= :id ,*tales-book*))
                                        "child" "chapter")))
         (render-chapter chapter stream))
   (:br)
   (:h1 "Indexes")
   (:div :class "index-link" :onclick "go(\"/words\");" "Words")))

(defun render-chapter (chapter stream)
  (with-html-output (stream)
    (:div :class "chapter-link" :onclick (format nil "visit(\"~A\");" (id chapter))
          (str (field-value chapter "title")))))

(defun create-button-strip (stream description)
  (with-html-output (stream)
    (:table
     (:tr
      (iter (for (name onclick selected) in description)
            (htm (:td (:div :class (if selected "page-button-selected" "page-button")
                            :onclick onclick (str name)))))))))

(defun page-controls (node stream)
  (let* ((parent (fetch-parent node))
         (previous (fetch-previous parent node))
         (next (fetch-next parent node)))
    (create-button-strip stream
                         `(,@(when parent `(("parent" ,(format nil "visit(\"~A\");" (id parent)))))
                           ,@(when previous `(("previous" ,(format nil "visit(\"~A\");" (id previous)))))
                           ,@(when next `(("next" ,(format nil "visit(\"~A\");" (id next)))))))))

(defun render-chapter-page (chapter)
  (render-page
   (field-value chapter "title")
   (page-controls chapter stream)
   (:h1 (str (field-value chapter "title")))
   (iter (for paragraph in (deck:search `(("chapter" (:= :id ,(id chapter))) "child" "paragraph")))
         (render-paragraph paragraph stream))))

(defun render-paragraph (paragraph stream)
  (with-html-output (stream)
    (:table :class "paragraph"
            :onmouseover (format nil "show(\"pc-~A\");" (id paragraph))
            :onmouseout (format nil "hide(\"pc-~A\");" (id paragraph))
            :onclick (format nil "visit(\"~A\");" (id paragraph))
            (:tr
             (:td (:div :class "p-text" :style "width:800px;"
                        (str (escape-non-tags (field-value paragraph "text")))))
             ;;               (:td (paragraph-controls paragraph stream))
             ))))

(defun escape-non-tags (string)
  (cl-who:escape-string string :test
                        (lambda (char)
                          (or (find char "&'\"")
                              (> (char-code char) 127)))))

(defun paragraph-controls (paragraph stream)
  (with-html-output (stream)
    (:table :style "visibility:hidden"
     :id (format nil "pc-~A" (id paragraph))
     (:tr
      (:td (:a :href (format nil "paragraph?id=~A" (id paragraph))
                   "sentences"))))))

(defun render-paragraph-page (paragraph index word)
  (let* ((text (field-value paragraph "text")))
    (render-page
     (cl-who:escape-string (princ-with-ellipses-to-string text 40))
     (page-controls paragraph stream)
     (:br)
     (let ((word (render-paragraph-detail paragraph stream index word)))
       (when word
         (render-word-detail word stream))))))

(defun render-paragraph-detail (paragraph stream index word)
  (let (index-word)
    (with-html-output (stream)
      (:div :class "p-text-detail" :style "width:800px;"
            (iter (for el in (split-paragraph (field-value paragraph "text")))
                  (for count from 1)
                  (cond
                    ((or (and word (string-equal el word)) (and index (= index count)))
                     (htm (:span :class "sw" (esc el)))
                     (setf index-word el))
                    ((and (> (length el) 1) (not (member el *ignore-words* :test #'string=))
                          (not (char= (char el 0) #\<)))
                     (htm (:span :class "w"
                                 :onclick (format nil "word(~A,~A);" (id paragraph) count)
                                 (esc el))))
                    ((char= (char el 0) #\<) (str el))
                    (t (esc el))))))
    (or word index-word)))

(defun group-paragraphs-by-chapter (paragraphs)
  (iter
   (with acc)
   (with current)
   (for paragraph in paragraphs)
   (unless current (setf current (field-value paragraph "chapter")))
   (if (eql (field-value paragraph "chapter") current)
     (push paragraph acc)
     (progn
       (collect (list current (nreverse acc)) into rtn)
       (setf current nil
             acc (list paragraph))))
   (finally (return
              (nconc rtn
                     (when current (list (list current acc))))))))

(defun paragraphs-with-word (word)
  (deck:search `((:node "word" (:= :id ,(get-word-id word))) "parent")))

(defun chapter-name (chapter-id)
  (first (or (deck:search `((:node "chapter" (:= :id ,chapter-id))) :fields '("title"))
             (error "No chapter with id ~S." chapter-id))))

(defun paragraph-excerpt-with-word (paragraph word &optional (size 80))
  (let* ((parts (split-paragraph (field-value paragraph "text")))
         (count (length parts))
         (pos (or (position word parts :test #'string-equal)
                  (error "Word ~S not found in paragraph ~S." word paragraph))))
    (subseq parts (max 0 (- pos (/ size 2))) (min count (+ pos (/ size 2))))))

(defun render-paragraph-excerpt (paragraph word stream)
  (with-html-output (stream)
    (:div :class "p-text-detail" :style "width:800px;"
          (iter (for el in (paragraph-excerpt-with-word paragraph word))
                (for count from 1)
                (cond
                  ((string-equal el word) (htm (:span :class "sw" (esc el))))
                  ((char= (char el 0) #\<) (str el))
                  (t (esc el)))))))

(defun render-word-detail (word stream)
  (with-html-output (stream)
    (:h1 (esc word))
    (iter (for (chapter paragraphs) in (group-paragraphs-by-chapter (paragraphs-with-word word)))
          (htm (:div :class "chapter-with-word" (esc (chapter-name chapter)))
               (:div :class "paragraphs-with-word"
                     (iter (for paragraph in paragraphs)
                           (htm (:div :class "word-p"
                                      :onclick (format nil "wordText(~A,~S);" (id paragraph)
                                                       (who:escape-string word))
                                      (render-paragraph-excerpt paragraph word stream)))))))))

(defun render-word-page (word-node)
  (let ((word (field-value word-node "text")))
    (render-page
     word
     (:link :rel "stylesheet" :type "text/css" :href "/css/tales.css")
     (:script :src "/js/tales.js" :type "text/javascript")
     ;;   (page-controls paragraph stream)
     (render-word-detail word stream))))
(in-package :tales)

(defvar *tales-acceptor* nil)
(defvar *tales-port* 1439) ; gold
(defvar *tales-username* "men")
(defvar *tales-password* "slugs")

(defun start-tales ()
  (when *tales-acceptor* (hunchentoot:stop *tales-acceptor*))
  (setf hunchentoot:*message-log-pathname* (tales-file (format nil "logs/message-~A.log" (now)))
        hunchentoot:*access-log-pathname* (tales-file (format nil "logs/access-~A.log" (now)))
        *tales-acceptor*
        (hunchentoot:start
         (make-instance 'hunchentoot:acceptor :port *tales-port*))
        hunchentoot:*dispatch-table*
        `(,(hunchentoot:create-prefix-dispatcher "/css/tales.css" 'tales-css)
          ,(hunchentoot:create-static-file-dispatcher-and-handler "/futura.ttf"
                                                                  (tales-file "futura.ttf"))
          ,(hunchentoot:create-prefix-dispatcher "/js/tales.js" 'tales-js:js-file)
          ,(hunchentoot:create-prefix-dispatcher "/favicon.ico" 'favicon-dispatch)
          hunchentoot:dispatch-easy-handlers
          hunchentoot:default-dispatcher)))

(defun tales-css () (slurp-file (tales-file "tales.css")))

(defun favicon-dispatch () (slurp-as-octets (tales-file "icon.ico")))

(hunchentoot:define-easy-handler (front-page :uri "/") (id index word)
  (multiple-value-bind (username password) (hunchentoot:authorization)
    (if (not (and (string= username *tales-username*)
                  (string= password *tales-password*)))
      (hunchentoot:require-authorization "tales")
      (if id
        (let* ((node (deck:get-node (parse-integer id)))
               (template-id (template-id node)))
          (cond
            ((template-is-type-of template-id "demo:book") (render-book-page node))
            ((template-is-type-of template-id "demo:chapter") (render-chapter-page node))
            ((template-is-type-of template-id "demo:paragraph")
             (render-paragraph-page node (and index (parse-integer index)) word))
            ((template-is-type-of template-id "demo:word") (render-word-page node))
            (t (error "Invalid ID ~A." id))))
        (render-book-page (deck:get-node *tales-book*))))))

(hunchentoot:define-easy-handler (words-index :uri "/words") (sort pos)
  (render-words-index sort pos))

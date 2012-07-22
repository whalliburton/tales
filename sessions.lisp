(in-package :tales)

(defmacro with-sessions ((session-id-var session-var) &body body)
  `(iter (for (,session-id-var . ,session-var) in (with-mutex ((session-db-lock *tales-acceptor*))
                                                    (session-db *tales-acceptor*)))
         ,@body))

(defun list-sessions ()
  (with-sessions (id session) (collect session)))


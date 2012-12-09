(in-package :tales)

(helpers:add-config-parameters
 deck-local-socket-name             "/mnt/projects/sockets/deck"
 deck-cache-invalidation-port       2003
 deck-uses-sharder                  nil)

(defun initialize ()
  (setf hunchentoot:*catch-errors-p* nil)
  (load-pos-dict)
  (start-session)
  (build)
  (initialize-word-lists)
  (start-tales))


(defpackage tales
  (:use common-lisp deck-client iterate)
  (:import-from sb-thread with-mutex)
  (:import-from cl-who with-html-output-to-string with-html-output htm str esc fmt)
  (:import-from alexandria with-input-from-file)
  (:import-from hunchentoot session-value session-db-lock session-db *session*
                *catch-errors-p*)
  (:import-from local-time now)
  (:import-from helpers breakout symb princ-with-ellipses-to-string))

(defpackage tales-js
  (:use common-lisp parenscript)
  (:export js-file))

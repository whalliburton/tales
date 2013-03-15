(defsystem :tales
  :serial t
  :components ((:static-file "tales.asd")
               (:file "package")
               (:file "rpc-sail")
               (:file "utility")
               (:file "parse")
               (:file "build")
               (:file "parts-of-speech")
               (:file "render")
               (:file "words")
               (:file "server")
               (:file "js")
               (:file "initialize"))
  :depends-on (:hunchentoot :deck-client :closure-html :alexandria :iterate :cl-who
                            :parenscript :local-time))
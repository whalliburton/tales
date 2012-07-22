(defsystem :tales
  :serial t
  :components ((:static-file "tales.asd")
               (:file "package")
               (:file "utility")
               (:file "parse")
               (:file "build")
               (:file "render")
               (:file "words")
               (:file "server")
               (:file "js")
               (:file "initialize"))
  :depends-on (:hunchentoot :deck-client :closure-html :alexandria :iterate :cl-who
                            :parenscript :local-time))
(defsystem yamson.yaml
  :depends-on (#:yamson.base #:yamson.json)
  :serial t
  :pathname "yaml/"
  :components ((:file "common")
               (:file "simple")
               (:file "string")
               (:file "property")
               (:file "object")
               (:file "directive")
               (:file "document")
               (:file "parse")))

(defsystem yamson.json
  :depends-on (#:yamson.base)
  :serial t
  :pathname "json/"
  :components ((:file "json")
               (:file "parse")))

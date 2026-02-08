(defsystem yamson.base
  :depends-on (#:alexandria #:parsonic)
  :serial t
  :components ((:file "package")
               (:file "construct")
               (:file "parse")))

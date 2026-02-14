(defsystem yamson.base
  :depends-on (#:alexandria #:parsonic #:parsonic.stream)
  :serial t
  :components ((:file "package")
               (:file "construct")
               (:file "parse")))

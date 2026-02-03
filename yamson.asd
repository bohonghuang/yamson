(defsystem yamson
  :version "0.1.0"
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "Apache-2.0"
  :description "Fast YAML and JSON parsers for Common Lisp."
  :homepage "https://github.com/bohonghuang/yamson"
  :bug-tracker "https://github.com/bohonghuang/yamson/issues"
  :source-control (:git "https://github.com/bohonghuang/yamson.git")
  :depends-on (#:alexandria #:parsonic)
  :serial t
  :components ((:file "package")
               (:file "construct")
               (:file "json")
               (:file "yaml")
               (:file "parse"))
  :in-order-to ((test-op (test-op #:yamson/test))))

(defsystem yamson/test
  :depends-on (#:yamson #:parachute)
  :pathname "test/"
  :components ((:file "package"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:yamson.test))))

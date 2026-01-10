(defpackage yamson.test
  (:use #:cl #:parachute #:yamson))

(in-package #:yamson.test)

(define-test suite)

(define-test json-number :parent suite)

(define-test json-integer :parent json-number
  (is = 123 (parse "123"))
  (is = -123 (parse "-123")))

(define-test json-float :parent json-number
  (is = 3.1415926 (parse "3.1415926"))
  (is = -3.1415926 (parse "-3.1415926"))
  (is = 0.5 (parse "0.5"))
  (is = -0.5 (parse "-0.5"))
  (is = 123.0 (parse "123.0")))

(define-test json-string :parent suite
  (is string= "hello" (parse "\"hello\""))
  (is string= "hello
world" (parse "\"hello\\nworld\""))
  (is string= "hello\"world" (parse "\"hello\\\"world\"")))

(define-test json-boolean :parent suite
  (is eq t (parse "true"))
  (is eq nil (parse "false")))

(define-test json-null :parent suite
  (is eq :null (parse "null")))

(define-test json-array :parent suite
  (is equal '(1 2 3) (parse "[1,2,3]"))
  (is equal '(1 2 3) (parse "[1, 2, 3]"))
  (is equal '(1 2 3) (parse "[ 1 , 2 , 3 ]"))
  (is equal '(1 "hello" t :null) (parse "[1,\"hello\",true,null]")))

(define-test json-object :parent suite
  (is equal '(("key" . "value")) (parse "{\"key\":\"value\"}"))
  (is equal '(("key1" . "value1") ("key2" . "value2")) (parse "{\"key1\":\"value1\",\"key2\":\"value2\"}"))
  (is equal '(("key1" . "value1") ("key2" . "value2")) (parse "{\"key1\": \"value1\", \"key2\": \"value2\"}"))
  (is equal '(("key" . 123)) (parse "{\"key\":123}"))
  (is equal '(("key" . 123.45)) (parse "{\"key\":123.45}"))
  (is equal '(("key" . t)) (parse "{\"key\":true}"))
  (is equal '(("key" . nil)) (parse "{\"key\":false}"))
  (is equal '(("key" . :null)) (parse "{\"key\":null}"))
  (is equal '(("key" . (1 2 3))) (parse "{\"key\":[1,2,3]}"))
  (is equal '(("key1" . "value") ("key2" . 123)) (parse "{\"key1\":\"value\",\"key2\":123}")))

(define-test yaml-simple :parent suite
  (is equal '(("key" . "value")) (parse "key: \"value\""))
  (is equal '(("key" . 123)) (parse "key: 123"))
  (is equal '(("key" . 123.45)) (parse "key: 123.45"))
  (is equal '(("key" . t)) (parse "key: true"))
  (is equal '(("key" . :null)) (parse "key: null")))

(define-test yaml-json-mix :parent suite
  (is equal '(("key" . (1 2 3))) (parse "key: [1, 2, 3]"))
  (is equal '(("key" . (("subkey" . "subvalue")))) (parse "key: {\"subkey\": \"subvalue\"}")))

(define-test yaml-nested :parent suite
  (is equal '(("key1" . "value1") ("key2" . (("subkey" . "subvalue")))) 
      (parse "key1: \"value1\"
key2:
  subkey: \"subvalue\""))
  (is equal '(("array" . (1 2 3))) 
      (parse "array:
  - 1
  - 2
  - 3"))
  (is equal '(("users" . ((("name" . "Alice") ("age" . 30)) (("name" . "Bob") ("age" . 25)))))
      (parse "users:
  - name: \"Alice\"
    age: 30
  - name: \"Bob\"
    age: 25"))
  (is equal '(("users" . ((("name" . "Alice") ("age" . 30)) (("name" . "Bob") ("age" . 25)))))
      (parse "users: [{\"name\": \"Alice\", \"age\": 30}, {\"name\": \"Bob\", \"age\": 25}]")))

(define-test yaml-comment :parent suite
  (is equal '(("key" . "value")) (parse "# This is a comment
key: \"value\" # This is another comment"))
  (is equal '(("key1" . "value1") ("key2" . "value2")) 
      (parse "key1: \"value1\" # Comment after key1
# This is a full line comment
key2: \"value2\"")))

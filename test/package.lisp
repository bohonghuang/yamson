(defpackage yamson.test
  (:use #:cl #:parachute #:yamson))

(in-package #:yamson.test)

(define-test suite)

(define-test json :parent suite)

(define-test json-number :parent json)

(define-test json-integer :parent json-number
  (is = 123 (parse "123" :subset :json))
  (is = -123 (parse "-123" :subset :json)))

(define-test json-float :parent json-number
  (is = 3.1415926 (parse "3.1415926" :subset :json))
  (is = -3.1415926 (parse "-3.1415926" :subset :json))
  (is = 0.5 (parse "0.5" :subset :json))
  (is = -0.5 (parse "-0.5" :subset :json))
  (is = 123.0 (parse "123.0" :subset :json)))

(define-test json-scientific :parent json-number
  (is = 1.23e-4 (parse "1.23e-4" :subset :json))
  (is = -1.23e-4 (parse "-1.23e-4" :subset :json))
  (is = 1.23e4 (parse "1.23e4" :subset :json))
  (is = -1.23e4 (parse "-1.23e4" :subset :json))
  (is = 1.23E-4 (parse "1.23E-4" :subset :json))
  (is = -1.23E-4 (parse "-1.23E-4" :subset :json))
  (is = 1.23E4 (parse "1.23E4" :subset :json))
  (is = -1.23E4 (parse "-1.23E4" :subset :json))
  (is = 12300.0 (parse "1.23e4" :subset :json))
  (is = 0.000123 (parse "1.23e-4" :subset :json)))

(define-test json-string :parent json
  (is string= "hello" (parse "\"hello\"" :subset :json))
  (is string= "hello
world" (parse "\"hello\\nworld\"" :subset :json))
  (is string= "hello\"world" (parse "\"hello\\\"world\"" :subset :json))
  (is string= "hello\\world" (parse "\"hello\\\\world\"" :subset :json))
  (is string= "hello
world" (parse "\"hello\\nworld\"" :subset :json))
  (is string= "hello	world" (parse "\"hello\\tworld\"" :subset :json))
  (is string= "helloworld" (parse "\"hello\\rworld\"" :subset :json))
  (is string= "helloworld" (parse "\"hello\\bworld\"" :subset :json))
  (is string= "hello
world" (parse "\"hello\\fworld\"" :subset :json))
  (is string= "hello/world" (parse "\"hello\\/world\"" :subset :json))
  (is string= "hello world" (parse "\"hello\\u2005world\"" :subset :json)))

(define-test json-boolean :parent json
  (is eq t (parse "true" :subset :json))
  (is eq nil (parse "false" :subset :json)))

(define-test json-null :parent json
  (is eq :null (parse "null" :subset :json)))

(define-test json-array :parent json
  (is equal '(1 2 3) (parse "[1,2,3]" :subset :json))
  (is equal '(1 2 3) (parse "[1, 2, 3]" :subset :json))
  (is equal '(1 2 3) (parse "[ 1 , 2 , 3 ]" :subset :json))
  (is equal '(1 "hello" t :null) (parse "[1,\"hello\",true,null]" :subset :json)))

(define-test json-object :parent json
  (is equal '(("key" . "value")) (parse "{\"key\":\"value\"}" :subset :json))
  (is equal '(("key1" . "value1") ("key2" . "value2")) (parse "{\"key1\":\"value1\",\"key2\":\"value2\"}" :subset :json))
  (is equal '(("key1" . "value1") ("key2" . "value2")) (parse "{\"key1\": \"value1\", \"key2\": \"value2\"}" :subset :json))
  (is equal '(("key" . 123)) (parse "{\"key\":123}" :subset :json))
  (is equal '(("key" . 123.45)) (parse "{\"key\":123.45}" :subset :json))
  (is equal '(("key" . t)) (parse "{\"key\":true}" :subset :json))
  (is equal '(("key" . nil)) (parse "{\"key\":false}" :subset :json))
  (is equal '(("key" . :null)) (parse "{\"key\":null}" :subset :json))
  (is equal '(("key" . (1 2 3))) (parse "{\"key\":[1,2,3]}" :subset :json))
  (is equal '(("key1" . "value") ("key2" . 123)) (parse "{\"key1\":\"value\",\"key2\":123}" :subset :json)))

(define-test yaml :parent suite)

(define-test yaml-simple :parent yaml
  (is equal '(("key" . "value")) (parse "key: \"value\""))
  (is equal '(("key" . 123)) (parse "key: 123"))
  (is equal '(("key" . 123.45)) (parse "key: 123.45"))
  (is equal '(("key" . t)) (parse "key: true"))
  (is equal '(("key" . :null)) (parse "key: null")))

(define-test yaml-number :parent yaml)

(define-test yaml-integer :parent yaml-number)

(define-test yaml-float :parent yaml-number
  (is = 3.1415926 (parse "3.1415926"))
  (is = -3.1415926 (parse "-3.1415926"))
  (is = 0.5 (parse "0.5"))
  (is = -0.5 (parse "-0.5"))
  (is = 123.0 (parse "123.0"))
  (is = 123456.789012 (parse "123_456_._78___9_012"))
  (is = -1.23e-4 (parse "-1.23E-4"))
  (is = 12300.0 (parse "1.23e4")))

(define-test yaml-integer-2 :parent yaml-integer
  (is = 10 (parse "0b1010"))
  (is = 10 (parse "0b__1_0_1__0")))

(define-test yaml-integer-8 :parent yaml-integer
  (is = 64 (parse "0o100"))
  (is = 64 (parse "0o_1_0__0")))

(define-test yaml-integer-10 :parent yaml-integer
  (is = 123456 (parse "123456"))
  (is = 123456 (parse "123_456")))

(define-test yaml-integer-16 :parent yaml-integer
  (is = 255 (parse "0xFF"))
  (is = 255 (parse "0x_F__F")))

(define-test yaml-string :parent yaml)

(define-test yaml-unquoted-string :parent yaml-string
  (is string= "hello" (parse "hello"))
  (is string= "hello world" (parse "hello world"))
  (is string= "hello:world" (parse "hello:world"))
  (is string= "-hello world" (parse "-hello world"))
  (is string= "?hello world" (parse "?hello world"))
  (is string= "hello:world" (parse "hello:world"))
  (is string= "hello#world" (parse "hello#world"))
  (is string= "hello world" (parse "hello
  world"))
  (is string= "hello
world" (parse "hello

    world")))

(define-test yaml-single-quoted-string :parent yaml-string
  (is string= "hello" (parse "'hello'"))
  (is string= "hello world" (parse "'hello world'"))
  (is string= "hello'world" (parse "'hello''world'"))
  (is string= "hello\\nworld" (parse "'hello\\nworld'"))
  (is string= "hello world" (parse "'hello
 world'"))
  (is string= "hello
world" (parse "'hello

  world'")))

(define-test yaml-double-quoted-string :parent yaml-string
  (is string= "hello" (parse "\"hello\""))
  (is string= "hello world" (parse "\"hello world\""))
  (is string= "hello
world" (parse "\"hello\\nworld\""))
  (is string= "hello world" (parse "\"hello
 world\""))
  (is string= "hello
world" (parse "\"hello

  world\"")))

(define-test yaml-string-multiline :parent yaml-string)

(define-test yaml-string-multiline-literal :parent yaml-string-multiline
  (is string= "hello
 world
" (parse "| # Comment
hello
 world"))
  (is string= "
hello
world" (parse "|-

 hello
 world"))
  (is string= "
hello
 
world

 !

" (parse "|+

    hello
     
    world

     !

"))
  (is string= "" (parse "|-"))
  (is equal '("" "") (parse "- |-
- |-")))

(define-test yaml-string-multiline-folded :parent yaml-string-multiline
  (is string= "hello world
" (parse "> # Comment
  hello
  world"))
  (is string= "hello world" (parse ">-
  hello
  world"))
  (is string= "hello world
 !

" (parse ">+
   hello
   world

    !

")))

(define-test yaml-boolean :parent yaml
  (is eq t (parse "true"))
  (is eq t (parse "True"))
  (is eq t (parse "TRUE"))
  (is eq nil (parse "false"))
  (is eq nil (parse "False"))
  (is eq nil (parse "FALSE")))

(define-test yaml-null :parent yaml
  (is eq :null (parse "null"))
  (is eq :null (parse "~")))

(define-test yaml-flow :parent yaml)

(define-test yaml-flow-sequence :parent yaml-flow
  (is equal '(1 2 3) (parse "[1, 2, 3]"))
  (is equal '("a" "b" "c") (parse "[\"a\", \"b\", \"c\"]"))
  (is equal '(1 "two" 3.0) (parse "[1, \"two\", 3.0]"))
  (is equal '((("key" . "value"))) (parse "[{key: \"value\"}]"))
  (is equal '((1 2) (3 4)) (parse "[[1, 2], [3, 4]]"))
  (is equal '((1 2) (3 4)) (parse "[[1, 2], [3, 4]]"))
  (is equal '() (parse "[]"))
  (is equal '() (parse "[ ]"))
  (is equal '(:null) (parse "[,]"))
  (is equal '(:null) (parse "[ , ]"))
  (is equal '(:null :null) (parse "[,,]"))
  (is equal '(("a" . "d") ("b" . "d")) (parse "[a: d,b: d]"))
  (is equal '(("seq" . (("a" . 1) ("b" . 2) ("c" . 3) ("d" . :null)))) (parse "seq: [a: 1,b: 2, c: 3,d:]")))

(define-test yaml-flow-mapping :parent yaml-flow
  (is equal '(("key" . "value")) (parse "{key: \"value\"}"))
  (is equal '(("key1" . "value1") ("key2" . "value2")) (parse "{key1: \"value1\", key2: \"value2\"}"))
  (is equal '(("key1" . 1) ("key2" . 2.0)) (parse "{key1: 1, key2: 2.0}"))
  (is equal '(("key" . (1 2 3))) (parse "{key: [1, 2, 3]}"))
  (is equal '(("outer" . (("inner" . "value")))) (parse "{outer: {inner: \"value\"}}"))
  (is equal '() (parse "{}"))
  (is equal '() (parse "{ }"))
  (is equal '((:null . :null)) (parse "{,}"))
  (is equal '((:null . :null)) (parse "{ , }"))
  (is equal '(("a" . :null) ("b" . :null) ("c" . "d")) (parse "{a,b, c: d}"))
  (is equal '(("key" . (("a" . :null) ("b" . :null) ("c" . :null)))) (parse "key: {a,b,c}")))

(define-test yaml-nested :parent yaml
  (is equal '(("key1" . "value1") ("key2" . (("subkey" . "subvalue")))) 
      (parse "key1: value1
key2:
  subkey: subvalue"))
  (is equal '(("array" . (1 2 3))) 
      (parse "array:
  - 1
  - 2
  - 3"))
  (is equal '(("array" . (1 2 3)))
        (parse "array:
- 1
- 2
- 3"))
  (is equal '(((1 2) 3) 4) 
      (parse "
    - - - 1
        - 2
      - 3
    - 4"))
  (is equal '(("users" . ((("name" . "Alice") ("age" . 30)) (("name" . "Bob") ("age" . 25)))))
      (parse "users:
  - name: Alice
    age: 30
  - name: Bob
    age: 25"))
  (is equal '(("users" . ((("name" . "Alice") ("age" . 30)) (("name" . "Bob") ("age" . 25)))))
      (parse "users: [{name: Alice, age: 30}, {name: Bob, age: 25}]")))

(define-test yaml-comment :parent yaml
  (is equal '(("key" . "value")) (parse "# This is a comment
key: value # This is another comment"))
  (is equal '(("key1" . "value1") ("key2" . "value2")) 
      (parse "key1: value1 # Comment after key1
# This is a full line comment
key2: value2"))
  (is equal '(("key" . (("subkey" . "subvalue")))) 
      (parse "key: {\"subkey\": \"subvalue\" # This is a comment
}")))

(define-test yaml-anchor :parent yaml
  (is equal '(("key" . "value")) (parse "key: &anchor value"))
  (is equal '(("key1" . "value") ("key2" . "value")) (parse "key1: &anchor value
key2: *anchor"))
  (is equal '(("key1" . (("subkey" . "value"))) ("key2" . (("subkey" . "value")))) (parse "key1: &anchor
  subkey: value
key2: *anchor"))
  (is equal '(("users" . ((("name" . "Alice")) (("name" . "Alice"))))) (parse "users:
- &alice
  name: Alice
- *alice"))
  (is equal '(("key1" . "value1") ("key2" . "value2") ("key3" . "value1")) (parse "key1: &anchor1 value1
key2: &anchor2 value2
key3: *anchor1"))
  (is equal '(("parent" . (("child" . (("grandchild" . "value")))))
              ("other" . (("child" . (("grandchild" . "value"))))))
      (parse "parent: &base
  child:
    grandchild: value
other: *base"))
  (is equal '((("a" . 1)) (("b" . 2) ("c" . (("a" . 1))))) (parse "[&x {a: 1}, {b: 2, c: *x}]"))
  (is equal '(("key" . ((1 2) (1 2)))) (parse "key: [ &ref [1, 2], *ref ]"))
  (is equal '(1 2 2 1) (parse "[ &a 1, &b 2, *b, *a ]"))
  (is equal '(("mapping" . (("k1" . "v1") ("k2" . "v1")))) (parse "mapping: {\"k1\":&anchor v1,\"k2\":*anchor}"))
  (is equal '(("key1" . (1 2)) ("key2" . (1 2))) (parse "
key1: &list
 - 1
 - 2
key2: *list"))
  (is equal '(("key1" . (1 2)) ("key2" . (1 2))) (parse "
key1: &list
        - 1
        - 2
key2: *list")))

(define-test yaml-document :parent yaml
  (is string= "value" (parse "--- value"))
  (is equal :null (parse "---"))
  (is equal :null (parse "---
..."))
  (is string= "abc
" (parse "--- |
 abc"))
  (is equal '(("a" . 1)) (parse "%YAML 1.2
---
a: 1"))
  (is equal :null (parse "...")))

(define-test yaml-multiple-documents :parent yaml
  (is equal '(1 2) (parse "--- 1
--- 2" :multiple-documents-p t))
  (is equal '((("a" . 1)) (("b" . 2))) (parse "--- 
a: 1
...
--- 
b: 2
..." :multiple-documents-p t))
  (is equal '(1) (parse "--- 1" :multiple-documents-p t))
  (is equal '(:null :null) (parse "%YAML 1.2
---
...
%YAML 1.2
---" :multiple-documents-p t)))

(define-test yaml-block-complex-key :parent yaml
  (is equal '(((("key1" . "value1") ("key2" . "value2")) . "result")) (parse "? key1: value1
  key2: value2
: result"))
  (is equal '(((("nested" . (("a" . 1) ("b" . 2)))) . "value")) (parse "? nested:
    a: 1
    b: 2
: value"))
  (is equal '(((1 2 3) . "sequence key")) (parse "?
  - 1
  - 2
  - 3
: sequence key"))
  (is equal '((((1 . "sequence key")) . :null)) (parse "? 1: sequence key"))
  (is equal '((123 . :null)) (parse "? 123")))

(define-test yaml-flow-complex-key :parent yaml
  (is equal '(((("a" . 1) ("b" . 2)) . "value")) (parse "{[a: 1, b: 2]: value}"))
  (is equal '(((1 2) . "value")) (parse "{? [1, 2]: value }"))
  (is equal '((:null . "value")) (parse "{? : value }"))
  (is equal '((:null . "value")) (parse "[? : value]"))
  (is equal '(("foo" . :null) ("bar" . :null)) (parse "{? foo, ? bar}")))

(define-test yaml-tag :parent yaml)

(define-test yaml-tag-global :parent yaml-tag
  (is string= "hello" (parse "!!str hello"))
  (is string= "123" (parse "!!str 123"))
  (is = 123 (parse "!!int 123"))
  (is = 123 (parse "!!int \"123\""))
  (is = 123.45 (parse "!!float 123.45"))
  (is = 123.45 (parse "!!float \"123.45\""))
  (is eq :null (parse "!!null null"))
  (is equal '(:null) (parse "[!!null]"))
  (is eq t (parse "!!bool true"))
  (is eq nil (parse "!!bool false"))
  (is equal '(1 2 3) (parse "!!seq
- 1
- 2
- 3"))
  (is equal nil (parse "!!seq []"))
  (is equal '(("key" . "value")) (parse "!!map
key: value"))
  (is equal nil (parse "!!map {}")))

(define-test yaml-tag-local :parent yaml-tag
  (is eq 'foo (parse "!intern FOO" :tags (list (lambda (tag value)
                                                 (when (string= tag "intern")
                                                   (values (intern value #.*package*) t))))))
  (is equal '(1 2 3) (parse "!list 1 2 3" :tags (list (lambda (tag value)
                                                        (when (string= tag "list")
                                                          (values (read-from-string (concatenate 'string "(" value ")")) t)))))))

(define-test yaml-tag-verbatim :parent yaml-tag
  (is string= "hello" (parse "!<tag:yaml.org,2002:str> hello"))
  (is = 456 (parse "!<tag:yaml.org,2002:int> \"456\"")))

(define-test yaml-tag-named :parent yaml-tag
  (is string= "123" (parse "%TAG !yaml!tag:yaml.org,2002:
---
!yaml!str 123"))
  (is equal '(:null) (parse "%TAG !yaml!tag:yaml.org,2002:
---
!yaml!seq [!yaml!null]")))

(in-package #:yamson)

(defparser yaml-directive-yaml ()
  (for ((nil '"YAML")
        (nil (yaml-whitespaces 1))
        (nil '"1.2"))
    (cons :yaml 1.2)))

(defparser yaml-directive-tag ()
  (for ((nil '"TAG")
        (nil (yaml-whitespaces 1))
        (short (prog2 '#\! (yaml-tag-name) '#\!))
        (nil (yaml-whitespaces 0))
        (long (yaml-tag-name)))
    (cons :tag (cons short long))))

(defparser yaml-directive ()
  '#\%
  (cut (or (yaml-directive-yaml) (yaml-directive-tag))))

(defun yaml-tag-shorthands-update (directives &optional (shorthands (yaml-tag-shorthands)))
  (loop :initially (clrhash shorthands)
        :for (key . value) :in directives
        :do (ecase key
              (:tag (setf (gethash (car value) shorthands) (cdr value)))
              (:yaml))))

(defparser yaml-directives ()
  (let ((directives (rep (progn (yaml-newlines) (yaml-directive)))))
    (notinline (constantly (yaml-tag-shorthands-update directives (yaml-tag-shorthands))))))

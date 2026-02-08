(in-package #:yamson)

(defvar *yaml-anchors*)

(defun yaml-anchors ()
  *yaml-anchors*)

(defparser yaml-identifier ()
  (for ((characters (rep (and (not (yaml-flow-object-terminator)) (satisfies (lambda (x) (not (or (yaml-whitespace-char-p x) (yaml-newline-char-p x)))))) 1)))
    (declare (type list characters))
    (coerce characters 'string)))

(defparser yaml-anchor ()
  '#\& (yaml-identifier))

(defparser yaml-alias ()
  (for ((anchor (progn '#\* (yaml-identifier))))
    (ensure-gethash anchor (yaml-anchors) (error "Undefined anchor: ~A" anchor))))

(declaim (ftype (function ((simple-array character (*)) t) (values t boolean)) yaml-tag-process-default))
(defun yaml-tag-process-default (tag value)
  (values
   (switch (tag :test #'string=)
     ("tag:yaml.org,2002:str"
      (typecase value
        (string value)
        (t (princ-to-string value))))
     ("tag:yaml.org,2002:int"
      (etypecase value
        (string
         (let ((value (parser-run (load-time-value (parser (yaml-number))) value)))
           (check-type value fixnum)
           value))
        (fixnum value)))
     ("tag:yaml.org,2002:float"
      (etypecase value
        (string
         (let ((value (parser-run (load-time-value (parser (yaml-number))) value)))
           (check-type value single-float)
           value))
        (single-float value)))
     ("tag:yaml.org,2002:null"
      (assert (eql value (construct-null)))
      value)
     ("tag:yaml.org,2002:bool"
      (assert (or (eql value (construct-boolean nil)) (eql value (construct-boolean t))))
      value)
     ("tag:yaml.org,2002:seq"
      (assert (or (type= (type-of value) (type-of (construct-mapping nil)))
                  (type= (type-of value) (type-of (construct-mapping (list nil))))))
      value)
     ("tag:yaml.org,2002:map"
      (assert (or (type= (type-of value) (type-of (construct-mapping nil)))
                  (type= (type-of value) (type-of (construct-mapping (list (cons nil nil)))))))
      value)
     (t (return-from yaml-tag-process-default (values nil nil))))
   t))

(defparameter *yaml-tags* (list #'yaml-tag-process-default))

(defvar *yaml-tag-shorthands*)

(defun yaml-tag-shorthands ()
  *yaml-tag-shorthands*)

(defun yaml-tags ()
  *yaml-tags*)

(defun yaml-tag-process (tag value &optional (tag-processors (yaml-tags)))
  (loop :for processor :in tag-processors
        :do (multiple-value-bind (value processedp) (funcall processor tag value)
              (when processedp (return value)))
        :finally (error "Unknown tag: ~A" tag)))

(defparser yaml-tag-local ()
  '#\! (yaml-identifier))

(defparser yaml-tag-global ()
  '"!!" (for ((name (yaml-identifier)))
          (declare (type (simple-array character (*)) name))
          (concatenate 'string "tag:yaml.org,2002:" name)))

(defparser yaml-tag-verbatim ()
  (prog2 '"!<"
      (for ((uri (rep (satisfies (lambda (c) (not (char= c #\>)))) 1)))
        (declare (type list uri))
        (coerce uri 'string))
    '">"))

(defparser yaml-tag-name ()
  (for ((name (rep (satisfies (lambda (c)
                                (and (not (yaml-whitespace-char-p c))
                                     (not (yaml-newline-char-p c))
                                     (not (char= c #\!)))))
                   1)))
    (declare (type list name))
    (coerce name 'string)))

(defparser yaml-tag-named ()
  (for ((prefix (progn '#\! (yaml-tag-name)))
        (suffix (progn '#\! (yaml-identifier))))
    (declare (type (simple-array character (*)) prefix suffix))
    (let ((prefix (ensure-gethash prefix (yaml-tag-shorthands) (error "Undefined tag prefix: ~A" prefix))))
      (declare (type (simple-array character (*)) prefix))
      (concatenate 'string prefix suffix))))

(defparser yaml-tag ()
  (or (yaml-tag-verbatim) (yaml-tag-global) (yaml-tag-named) (yaml-tag-local)))

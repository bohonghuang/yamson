(in-package #:yamson)

(defun parse-json-stream (stream &key junk-allowed)
  (assert (input-stream-p stream))
  (assert (type= (stream-element-type stream) 'character))
  (with-constructors
    (parser-run
     (parser (json-file junk-allowed))
     (the parsonic::character-input-stream stream))))

(defmethod parse-json ((stream stream) &rest args)
  (apply #'parse-json-stream stream args))

(defmethod parse-json ((pathname pathname) &rest args)
  (with-input-from-file (stream pathname)
    (apply #'parse-json-stream stream args)))

(defmethod parse-json ((string string) &rest args)
  (check-type string (simple-array character (*)))
  (with-input-from-string (stream string)
    (apply #'parse-json-stream stream args)))

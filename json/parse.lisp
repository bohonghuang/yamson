(in-package #:yamson)

(defun parse-json-stream (stream &key junk-allowed)
  (assert (input-stream-p stream))
  (assert (type= (stream-element-type stream) 'character))
  (with-constructors
    (funcall
     (parser-lambda (input)
       (declare (type parsonic::character-input-stream input)
                (optimize (speed 3) (debug 0) (safety 0)))
       (json-file junk-allowed))
     stream)))

(defmethod parse-json ((stream stream) &rest args)
  (apply #'parse-json-stream stream args))

(defmethod parse-json ((string string) &rest args)
  (check-type string (simple-array character (*)))
  (with-input-from-string (stream string)
    (apply #'parse-json-stream stream args)))

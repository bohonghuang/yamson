(in-package #:yamson)

(defun parse-json-string (string &key junk-allowed)
  (check-type string (simple-array character (*)))
  (with-constructors
    (funcall
     (parser-lambda (input)
       (declare (type (simple-array character (*)) input)
                (optimize (speed 3) (debug 0) (safety 0)))
       (json-file junk-allowed))
     string)))

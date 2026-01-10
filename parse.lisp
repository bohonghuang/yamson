(in-package #:yamson)

(defun parse-string (string &key junk-allowed)
  (funcall
   (parser-lambda (input)
     (declare (type (simple-array character (*)) input))
     (yaml-file junk-allowed))
   string))

(define-condition yamson-parse-error ()
  ((position :initarg :position)))

(defun parse (object &rest args)
  (multiple-value-bind (result errorp)
      (etypecase object
        ((simple-array character (*)) (apply #'parse-string object args)))
    (if errorp
        (error 'yamson-parse-error :position result)
        result)))

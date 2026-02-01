(in-package #:yamson)

(defun parse-string (string &key junk-allowed)
  (check-type string (simple-array character (*)))
  (let* ((anchors (make-hash-table :test #'equal))
         (*yaml-anchors* anchors))
    (macrolet ((yaml-anchors () 'anchors))
      (funcall
       (parser-lambda (input)
         (declare (type (simple-array character (*)) input)
                  (optimize (speed 3) (debug 0) (safety 0)))
         (yaml-file junk-allowed))
       string))))

(define-condition yamson-parse-error ()
  ((position :initarg :position :reader yamson-parse-error-position))
  (:report (lambda (condition stream)
             (format stream "Parse error at position ~A" (yamson-parse-error-position condition)))))

(defun parse (object &rest args &key multiple-documents-p &allow-other-keys)
  (delete-from-plistf args :multiple-documents-p)
  (multiple-value-call
      (lambda (result &optional (position nil errorp))
        (if errorp
            (error 'yamson-parse-error :position position)
            (if multiple-documents-p result (destructuring-bind (document) result document))))
    (etypecase object
      ((simple-array character (*)) (apply #'parse-string object args)))))

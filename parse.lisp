(in-package #:yamson)

(define-condition yamson-parse-error ()
  ((position :initarg :position :reader yamson-parse-error-position))
  (:report (lambda (condition stream)
             (format stream "Parse error at position ~A" (yamson-parse-error-position condition)))))

(declaim (ftype (function ((simple-array character (*)) &key (:tags list) (:junk-allowed boolean)) t) parse-yaml-string)
         (ftype (function ((simple-array character (*)) &key (:junk-allowed boolean)) t) parse-json-string))

(defun parse (object &rest args &key multiple-documents-p subset
                                  (mapping *constructor-mapping*)
                                  (sequence *constructor-sequence*)
                                  (boolean *constructor-boolean*)
                                  (null *constructor-null*)
              &allow-other-keys)
  (delete-from-plistf args :multiple-documents-p :subset :mapping :sequence :boolean :null)
  (let ((*constructor-mapping* mapping)
        (*constructor-sequence* sequence)
        (*constructor-boolean* boolean)
        (*constructor-null* null))
    (multiple-value-call
        (lambda (result &optional (position nil errorp))
          (if errorp
              (error 'yamson-parse-error :position position)
              (if multiple-documents-p result (destructuring-bind (document) result document))))
      (etypecase object
        ((simple-array character (*))
         (apply
          (ecase subset
            ((:json) (setf multiple-documents-p t) #'parse-json-string)
            ((:yaml nil) #'parse-yaml-string))
          object args))))))

(in-package #:yamson)

(define-condition yamson-parse-error ()
  ((position :initarg :position :reader yamson-parse-error-position))
  (:report (lambda (condition stream)
             (format stream "Parse error at position ~A" (yamson-parse-error-position condition)))))

(defgeneric parse-yaml (object &rest args))

(defgeneric parse-json (object &rest args))

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
      (apply
       (ecase subset
         ((:json) (setf multiple-documents-p t) #'parse-json)
         ((:yaml nil) #'parse-yaml))
       object args))))

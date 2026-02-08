(in-package #:yamson)

(defun parse-yaml-string (string &key junk-allowed)
  (check-type string (simple-array character (*)))
  (let* ((tags *yaml-tags*)
         (tag-shorthands (make-hash-table :test #'equal))
         (*yaml-tag-shorthands* tag-shorthands)
         (anchors (make-hash-table :test #'equal))
         (*yaml-anchors* anchors))
    (macrolet ((yaml-anchors () 'anchors)
               (yaml-tags () 'tags)
               (yaml-tag-shorthands () 'tag-shorthands))
      (funcall
       (parser-lambda (input)
         (declare (type (simple-array character (*)) input)
                  (optimize (speed 3) (debug 0) (safety 0)))
         (yaml-file junk-allowed))
       string))))

(defun parse-json-string (string &key junk-allowed)
  (check-type string (simple-array character (*)))
  (with-constructors
    (funcall
     (parser-lambda (input)
       (declare (type (simple-array character (*)) input)
                (optimize (speed 3) (debug 0) (safety 0)))
       (json-file junk-allowed))
     string)))

(define-condition yamson-parse-error ()
  ((position :initarg :position :reader yamson-parse-error-position))
  (:report (lambda (condition stream)
             (format stream "Parse error at position ~A" (yamson-parse-error-position condition)))))

(defun parse (object &rest args &key multiple-documents-p subset
                                  (mapping *constructor-mapping*)
                                  (sequence *constructor-sequence*)
                                  (boolean *constructor-boolean*)
                                  (null *constructor-null*)
                                  (tags nil)
              &allow-other-keys)
  (delete-from-plistf args :multiple-documents-p :subset :mapping :sequence :boolean :null :tags)
  (let ((*constructor-mapping* mapping)
        (*constructor-sequence* sequence)
        (*constructor-boolean* boolean)
        (*constructor-null* null)
        (*yaml-tags* (append tags *yaml-tags*)))
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

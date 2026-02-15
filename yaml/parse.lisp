(in-package #:yamson)

(defun parse-yaml-stream (stream &key tags junk-allowed)
  (assert (input-stream-p stream))
  (assert (type= (stream-element-type stream) 'character))
  (let* ((tags (append tags *yaml-tags*))
         (*yaml-tags* tags)
         (tag-shorthands (make-hash-table :test #'equal))
         (*yaml-tag-shorthands* tag-shorthands)
         (anchors (make-hash-table :test #'equal))
         (*yaml-anchors* anchors))
    (macrolet ((yaml-anchors () 'anchors)
               (yaml-tags () 'tags)
               (yaml-tag-shorthands () 'tag-shorthands))
      (with-constructors
        (funcall
         (parser-lambda (input)
           (declare (type parsonic::character-input-stream input)
                    (optimize (speed 3) (debug 0) (safety 0)))
           (yaml-file junk-allowed))
         stream)))))

(defmethod parse-yaml ((stream stream) &rest args)
  (apply #'parse-yaml-stream stream args))

(defmethod parse-yaml ((pathname pathname) &rest args)
  (with-input-from-file (stream pathname)
    (apply #'parse-yaml-stream stream args)))

(defmethod parse-yaml ((string string) &rest args)
  (check-type string (simple-array character (*)))
  (with-input-from-string (stream string)
    (apply #'parse-yaml-stream stream args)))

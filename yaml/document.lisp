(in-package #:yamson)

(defparser yaml-newlines (&optional (count 0))
  (rep (yaml-newline) count)
  (constantly nil))

(defparser yaml-document-begin-indicator ()
  '"---")

(defparser yaml-document-begin ()
  (yaml-newlines)
  (yaml-indicator (yaml-document-begin-indicator)))

(defparser yaml-document-end-indicator ()
  '"...")

(defparser yaml-document-end ()
  (yaml-newlines)
  (yaml-indicator (yaml-document-end-indicator)))

(defparser yaml-document-indicator ()
  (or (yaml-document-begin-indicator) (yaml-document-end-indicator)))

(defparser yaml-document (&optional junk-allowed)
  (opt (prog1 (yaml-directives) (yaml-document-begin)))
  (let ((result (or (peek (progn
                            (or (yaml-document-begin) (yaml-document-end))
                            (constantly (construct-null))))
                    (yaml-value -1))))
    (prog1 (constantly result)
      (yaml-newlines)
      (rep (or (yaml-eof)
               (yaml-document-end-indicator)
               (peek (yaml-document-begin-indicator)))
           (if junk-allowed 0 1) 1)
      (yaml-newlines))))

(defparser yaml-documents ()
  (for ((documents (repsep (cut (yaml-document)) (not (yaml-eof)))))
    (copy-list documents)))

(defparser yaml-file (&optional junk-allowed)
  (let ((result (yaml-documents)))
    (prog1 (constantly result)
      (rep (yaml-eof) (if junk-allowed 0 1) 1))))

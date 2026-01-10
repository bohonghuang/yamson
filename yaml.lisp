(in-package #:yamson)

(defparser yaml-indent (min &optional (max most-positive-fixnum))
  (for ((spaces (rep '#\Space min max)))
    (length (the list spaces))))

(defparser yaml-whitespaces (&optional (min 0) (max most-positive-fixnum))
  (for ((spaces (rep '#\Space min max)))
    (length (the list spaces))))

(declaim (ftype (function (character) (values boolean)) yaml-newline-char-p)
         (inline yaml-newline-char-p))
(defun yaml-newline-char-p (char)
  (or (char= char #\Newline) (char= char #\Return)))

(defparser yaml-comment ()
  (for ((comment (progn '"#" (rep (satisfies (complement #'yaml-newline-char-p))))))
    (coerce comment 'string)))

(defparser yaml-eol ()
  (progn
    (yaml-whitespaces)
    (opt (yaml-comment))
    (constantly nil)))

(defparser yaml-newline ()
  (progn
    (yaml-eol)
    (satisfies #'yaml-newline-char-p)))

(defparser yaml-newline-indent (min &optional (max most-positive-fixnum))
  (progn
    (rep (yaml-newline) 1)
    (yaml-indent min max)))

(declaim (ftype (function (character) (values boolean)) yaml-identifier-char-p)
         (inline yaml-identifier-char-p))
(defun yaml-identifier-char-p (char &aux (code (char-code char)))
  (or (<= (char-code #\A) code (char-code #\Z))
      (<= (char-code #\a) code (char-code #\z))
      (<= (char-code #\0) code (char-code #\9))
      (eq char #\_)))

(defparser yaml-identifier ()
  (for ((identifier (rep (satisfies #'yaml-identifier-char-p) 1)))
    (coerce (the list identifier) 'string)))

(defparser yaml-array (level)
  (for ((elems (repsep
                (progn '"-" (yaml-value level t))
                (yaml-newline-indent level level)
                1)))
    (copy-list elems)))

(defparser yaml-mixed-indent (level)
  (or (yaml-newline-indent level)
      (for ((spaces (yaml-whitespaces)))
        (- (+ spaces level)))))

(defparser yaml-value (level &optional array-element-p)
  (funcall
   (lambda (level)
     (parser (or (json-value)
                 (funcall
                  (lambda ()
                    (if (minusp level)
                        (if array-element-p
                            (parser (yaml-object (- level)))
                            (parser (or)))
                        (parser (or (yaml-array level) (yaml-object level))))))
                 (constantly nil))))
   (yaml-mixed-indent (1+ level))))

(defparser yaml-field (level)
  (for ((key (yaml-identifier))
        (nil '":")
        (value (yaml-value level)))
    (cons key value)))

(defparser yaml-object (level)
  (for ((fields (repsep (yaml-field level) (yaml-newline-indent level level) 1)))
    (copy-list fields)))

(defparser yaml-file (&optional junk-allowed)
  (funcall
   (lambda (result)
     (if junk-allowed
         (parser (constantly result))
         (parser (prog1 (constantly result) (rep (yaml-newline)) (yaml-eol) (eof)))))
   (yaml-value -1)))

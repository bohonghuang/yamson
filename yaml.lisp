(in-package #:yamson)

(defparser yaml-indent (min &optional (max most-positive-fixnum))
  (for ((spaces (rep '#\Space min max)))
    (declare (type list spaces))
    (length spaces)))

(defparser yaml-whitespaces (&optional (min 0) (max most-positive-fixnum))
  (for ((spaces (rep '#\Space min max)))
    (declare (type list spaces))
    (length spaces)))

(declaim (ftype (function (character) (values boolean)) yaml-newline-char-p)
         (inline yaml-newline-char-p))
(defun yaml-newline-char-p (char)
  (or (char= char #\Newline) (char= char #\Return)))

(defparser yaml-newline-char ()
  (satisfies #'yaml-newline-char-p))

(defparser yaml-comment ()
  (for ((comment (progn '#\# (rep (satisfies (complement #'yaml-newline-char-p))))))
    (coerce comment 'string)))

(defparser yaml-eol ()
  (progn
    (yaml-whitespaces)
    (opt (yaml-comment))
    (constantly nil)))

(defparser yaml-newline ()
  (progn
    (yaml-eol)
    (yaml-newline-char)))

(defparser yaml-newline-indent (min &optional (max most-positive-fixnum))
  (progn
    (rep (yaml-newline) 1)
    (yaml-indent min max)))

(defparser yaml-block-sequence (level)
  (for ((elems (repsep
                (progn '#\- (yaml-value level t))
                (yaml-newline-indent level level)
                1)))
    (copy-list elems)))

(defparser yaml-mixed-indent (level)
  (or (yaml-newline-indent level)
      (for ((spaces (yaml-whitespaces)))
        (- (+ spaces level)))))

(defparser yaml-single-quoted-string ()
  (for ((characters (prog2 '#\' (rep (or (progn '#\' '#\') (satisfies (lambda (x) (not (eql x #\')))))) (cut '#\'))))
    (declare (type list characters))
    (coerce characters 'string)))

(defparser yaml-double-quoted-string ()
  (json-string))

(defparser yaml-unquoted-string (&optional (terminator (or)))
  (for ((characters (rep (and (not (progn (yaml-eol) (or (yaml-newline-char) (eof) terminator))) (satisfies #'characterp)) 1)))
    (declare (type list characters))
    (coerce characters 'string)))

(defparser yaml-boolean ()
  (or (json-boolean)
      (progn (or'"True" '"TRUE") (constantly t))
      (progn (or'"False" '"FALSE") (constantly nil))))

(defparser yaml-number ()
  (json-number))

(defparser yaml-flow-whitespaces ()
  (rep (yaml-newline) 0)
  (yaml-whitespaces))

(defparser yaml-flow-trim (parser)
  (prog2 (yaml-flow-whitespaces) parser (yaml-flow-whitespaces)))

(defparser yaml-flow-brackets ()
  (or '#\{ '#\[ '#\] '#\}))

(defparser yaml-flow-object-terminator ()
  (or #1=(or (yaml-flow-brackets) '#\,) (prog1 '#\: (peek (or (yaml-newline) (yaml-whitespaces 1) #1#)))))

(defparser yaml-flow-unquoted-string ()
  (yaml-unquoted-string (yaml-flow-object-terminator)))

(defparser yaml-flow-value (&optional value-required-p)
  (yaml-flow-whitespaces)
  (or (prog1 (yaml-value-unquoted)
        (yaml-flow-whitespaces)
        (peek (yaml-flow-object-terminator)))
      (prog1 (or (yaml-value-quoted) (yaml-flow-unquoted-string))
        (yaml-flow-whitespaces))
      (prog1 (constantly :null)
        (yaml-flow-whitespaces)
        (rep (not (yaml-flow-brackets)) (if value-required-p 0 1) 1))))

(defparser yaml-flow-mapping-element (&optional sequence-element-p)
  (for ((key (yaml-flow-value))
        (value (or (progn '#\: (yaml-flow-value t)) (constantly #1='#:null))))
    (if (eq value #1#) (if sequence-element-p key (cons key :null)) (cons key value))))

(defparser yaml-flow-sequence ()
  (for ((list (prog2 '#\[
                  (repsep (yaml-flow-mapping-element t) '#\,)
                (opt (progn '#\, (yaml-flow-whitespaces)))
                (cut '#\]))))
    (copy-list list)))

(defparser yaml-flow-mapping ()
  (for ((alist (prog2 '#\{
                   (repsep (yaml-flow-mapping-element nil) '#\,)
                 (opt (progn '#\, (yaml-flow-whitespaces)))
                 (cut '#\}))))
    (copy-list alist)))

(defparser yaml-null ()
  (json-null))

(defparser yaml-value-unquoted ()
  (or (yaml-boolean) (yaml-number) (yaml-null)))

(defparser yaml-value-quoted ()
  (or (yaml-single-quoted-string) (yaml-double-quoted-string)
      (yaml-flow-sequence) (yaml-flow-mapping)))

(defparser yaml-value-simple ()
  (or (yaml-value-unquoted) (yaml-value-quoted)))

(defparser yaml-value (level &optional block-sequence-element-p)
  (let ((level (yaml-mixed-indent (1+ level))))
    (declare (type fixnum level))
    (or (prog1 (yaml-value-simple) (peek (progn (yaml-eol) (or (yaml-newline-char) (eof)))))
        ((lambda ()
           (if (minusp level)
               (if block-sequence-element-p
                   (parser (yaml-block-mapping (- level)))
                   (parser (or)))
               (parser (or (yaml-block-sequence level) (yaml-block-mapping level))))))
        (yaml-unquoted-string) (constantly nil))))

(defparser yaml-block-mapping-element (level)
  (for ((key (yaml-unquoted-string '#\:))
        (nil '#\:)
        (value (yaml-value level)))
    (cons key value)))

(defparser yaml-block-mapping (level)
  (for ((fields (repsep (yaml-block-mapping-element level) (yaml-newline-indent level level) 1)))
    (copy-list fields)))

(defparser yaml-file (&optional junk-allowed)
  ((lambda (result)
     (if junk-allowed
         (parser (constantly result))
         (parser (prog1 (constantly result) (rep (yaml-newline)) (yaml-eol) (eof)))))
   (yaml-value -1)))

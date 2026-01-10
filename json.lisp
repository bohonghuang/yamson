(in-package #:yamson)

(defparser json-boolean ()
  (or (progn '"true" (constantly t)) (progn '"false" (constantly nil))))

(defparser json-digit ()
  (for ((digit (or '#\0 '#\1 '#\2 '#\3 '#\4 '#\5 '#\6 '#\7 '#\8 '#\9)))
    (- (char-int digit) (char-int #\0))))

(declaim (ftype (function (list) non-negative-fixnum) digits-integer))
(defun digits-integer (digits)
  (loop :for digit :of-type (integer 0 10) :in (nreverse digits)
        :for base :of-type non-negative-fixnum := 1 :then (* base 10)
        :sum (the non-negative-fixnum (* digit base)) :of-type non-negative-fixnum))

(defparser json-integer ()
  (for ((digits (rep (json-digit) 1)))
    (digits-integer digits)))

(declaim (ftype (function (fixnum non-negative-fixnum) single-float) integers-float))
(defun integers-float (integer decimal)
  (let ((sign (if (minusp integer) -1 1))
        (abs-int (abs integer))
        (abs-dec (abs decimal)))
    (* sign (+ abs-int (if (zerop abs-dec) 0.0 (/ (coerce abs-dec 'single-float) (expt 10.0 (1+ (floor (log abs-dec 10))))))))))

(defparser json-number ()
  (for ((number (list (opt '"-") (json-integer) (opt (progn '"." (json-integer))))))
    (destructuring-bind (sign integer decimal) number
      (declare (type non-negative-fixnum integer)
               (type (or non-negative-fixnum null) decimal))
      (if decimal
          (* (integers-float integer decimal) (if sign -1.0 1.0))
          (* integer (if sign -1 1))))))

(defparser json-string-escape-char ()
  (or (progn '#\n (constantly #\Newline))
      (progn '#\r (constantly #\Return))
      (progn '#\t (constantly #\Tab))
      (progn '#\b (constantly #\Backspace))
      (progn '#\f (constantly #\Linefeed))
      (progn '#\\ (constantly #\\))
      (progn '#\" (constantly #\"))))

(defparser json-string ()
  (for ((characters (prog2 '"\""
                        (rep (or (progn '"\\" (json-string-escape-char)) (satisfies (lambda (x) (not (eql x #\"))))))
                      '"\"")))
    (coerce (the list characters) 'string)))

(defparser json-whitespace ()
  (rep (or '#\Space '#\Tab '#\Return '#\Newline) 0))

(defparser json-trim (parser)
  (prog2 (json-whitespace) parser (json-whitespace)))

(defparser json-null ()
  (progn '"null" (constantly :null)))

(defparser json-value ()
  (or (json-boolean) (json-string) (json-number) (json-array) (json-object) (json-null)))

(defparser json-array ()
  (for ((list (prog2 '"[" (repsep (json-trim (json-value)) '",") '"]")))
    (copy-list list)))

(defparser json-field ()
  (for ((key (json-trim (json-string)))
        (nil '":")
        (value (json-trim (json-value))))
    (cons key value)))

(defparser json-object ()
  (for ((alist (prog2 '"{" (repsep (json-field) '",") '"}")))
    (copy-list alist)))

(in-package #:yamson)

(defparser json-boolean ()
  (or (progn '"true" (constantly t)) (progn '"false" (constantly nil))))

(defparser json-digit ()
  (for ((digit (satisfies (lambda (char) (<= #.(char-code #\0) (char-code char) #.(char-code #\9))))))
    (- (char-code digit) (char-code #\0))))

(declaim (ftype (function (list) (values non-negative-fixnum)) digits-integer)
         (inline digits-integer))
(defun digits-integer (digits)
  (loop :for digit :of-type (mod 10) :in digits
        :for number :of-type non-negative-fixnum := digit :then (+ (the non-negative-fixnum (* number 10)) digit)
        :finally (return number)))

(defparser json-integer ()
  (for ((digits (rep (json-digit) 1)))
    (digits-integer digits)))

(declaim (ftype (function (list list) (values single-float)) integers-float)
         (inline digits-float))
(defun digits-float (integer-digits decimal-digits)
  (loop :for digit :of-type (mod 10) :in integer-digits
        :for integer-part :of-type single-float := (coerce digit 'single-float) :then (+ (* integer-part 10.0) digit)
        :finally
           (loop :for digit :of-type (mod 10) :in decimal-digits
                 :for base :of-type single-float := 0.1 :then (* base 0.1)
                 :sum (* digit base) :into decimal-part :of-type single-float
                 :finally (return-from digits-float (+ integer-part decimal-part)))))

(defparser json-number ()
  (for ((number (list (opt '#\-) (rep (json-digit) 1) (opt (progn '"." (rep (json-digit) 1))))))
    (destructuring-bind (negativep integer-digits decimal-digits) number
      (if decimal-digits
          (* (digits-float integer-digits decimal-digits) (if negativep -1.0 1.0))
          (* (digits-integer integer-digits) (if negativep -1 1))))))

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

(in-package #:yamson)

(defparser yaml-null ()
  (or (json-null) (progn '#\~ (constantly (construct-null)))))

(defparser yaml-boolean ()
  (or (json-boolean)
      (progn (or'"True" '"TRUE") (constantly (construct-boolean t)))
      (progn (or'"False" '"FALSE") (constantly (construct-boolean nil)))))

(defparser yaml-digit-2 ()
  (for ((digit (or '#\0 '#\1)))
    (- (char-code digit) (char-code #\0))))

(defparser yaml-integer-2 ()
  (for ((digits (progn '"0b" (rep (progn (rep '#\_) (yaml-digit-2)) 1))))
    (digits-integer digits 2)))

(defparser yaml-digit-8 ()
  (for ((digit (satisfies (lambda (char) (<= #.(char-code #\0) (char-code char) #.(char-code #\7))))))
    (- (char-code digit) (char-code #\0))))

(defparser yaml-integer-8 ()
  (for ((digits (progn '"0o" (rep (progn (rep '#\_) (yaml-digit-8)) 1))))
    (digits-integer digits 8)))

(defparser yaml-digit-16 ()
  (json-digit-16))

(defparser yaml-integer-16 ()
  (for ((digits (progn '"0x" (rep (progn (rep '#\_) (yaml-digit-16)) 1))))
    (digits-integer digits 16)))

(defparser yaml-digit-10 ()
  (json-digit))

(defparser yaml-number-10 ()
  (for ((integer-digits (repsep (json-digit) (rep '#\_) 1))
        (decimal-digits (opt (progn (rep '#\_) '#\. (rep (progn (rep '#\_) (yaml-digit-10)) 1))))
        (exponent (opt (progn (or '#\E '#\e) (json-integer)))))
    (declare (type (or fixnum null) exponent))
    (cond
      (exponent (* (digits-float integer-digits decimal-digits) (expt 10.0 exponent)))
      (decimal-digits (digits-float integer-digits decimal-digits))
      (t (digits-integer integer-digits)))))

(defparser yaml-number ()
  (for ((sign (or (progn '#\- (constantly -1)) (constantly +1)))
        (number (or (yaml-integer-2) (yaml-integer-8) (yaml-integer-16) (yaml-number-10))))
    (declare (type (integer -1 +1) sign))
    (etypecase number
      (fixnum (* sign number))
      (single-float (* sign number)))))

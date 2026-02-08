(in-package #:yamson)

(defparser json-boolean ()
  (or (progn '"true" (constantly (construct-boolean t))) (progn '"false" (constantly (construct-boolean nil)))))

(defparser json-digit ()
  (for ((digit (satisfies (lambda (char) (<= #.(char-code #\0) (char-code char) #.(char-code #\9))))))
    (- (char-code digit) (char-code #\0))))

(declaim (ftype (function (list &optional positive-fixnum) (values non-negative-fixnum)) digits-integer)
         (inline digits-integer))
(defun digits-integer (digits &optional (radix 10))
  (loop :for digit :of-type (mod 16) :in digits
        :for number :of-type non-negative-fixnum := digit :then (+ (the non-negative-fixnum (* number radix)) digit)
        :finally (return number)))

(defparser json-integer ()
  (for ((sign (or (progn '#\- (constantly -1)) (progn (opt '#\+) (constantly +1))))
        (digits (rep (json-digit) 1)))
    (declare (type (integer -1 +1) sign))
    (* (digits-integer digits) sign)))

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
  (for ((sign (or (progn '#\- (constantly -1)) (constantly +1)))
        (integer-digits (rep (json-digit) 1))
        (decimal-digits (opt (progn '#\. (rep (json-digit) 1))))
        (exponent (opt (progn (or '#\E '#\e) (json-integer)))))
    (declare (type (integer -1 +1) sign) (type (or fixnum null) exponent))
    (cond
      (exponent (* (digits-float integer-digits decimal-digits) (expt 10.0 exponent) sign))
      (decimal-digits (* (digits-float integer-digits decimal-digits) sign))
      (t (* (digits-integer integer-digits) sign)))))

(defparser json-digit-16 ()
  (for ((digit (or (satisfies (lambda (char)
                                (or (<= #.(char-code #\0) (char-code char) #.(char-code #\9))
                                    (<= #.(char-code #\a) (char-code char) #.(char-code #\f))
                                    (<= #.(char-code #\A) (char-code char) #.(char-code #\F))))))))
    (cond
      ((<= #.(char-code #\0) (char-code digit) #.(char-code #\9))
       (- (char-code digit) #.(char-code #\0)))
      ((<= #.(char-code #\a) (char-code digit) #.(char-code #\f))
       (+ 10 (- (char-code digit) #.(char-code #\a))))
      ((<= #.(char-code #\A) (char-code digit) #.(char-code #\F))
       (+ 10 (- (char-code digit) #.(char-code #\A)))))))

(defparser json-string-escape-char ()
  (or (progn '#\n (constantly #\Newline))
      (progn '#\r (constantly #\Return))
      (progn '#\t (constantly #\Tab))
      (progn '#\b (constantly #\Backspace))
      (progn '#\f (constantly #\Linefeed))
      (progn '#\\ (constantly #\\))
      (progn '#\" (constantly #\"))
      (progn '#\/ (constantly #\/))
      (progn '#\u (for ((digits (rep (json-digit-16) 4)))
                    (code-char (digits-integer digits 16))))))

(defparser json-string ()
  (for ((characters (prog2 '#\"
                        (rep (or (progn '#\\ (cut (json-string-escape-char))) (satisfies (lambda (x) (not (char= x #\"))))))
                      (cut '#\"))))
    (declare (type list characters))
    (coerce characters 'string)))

(defparser json-whitespace ()
  (rep (or '#\Space '#\Tab '#\Return '#\Newline) 0))

(defparser json-trim (parser)
  (prog2 (json-whitespace) parser (json-whitespace)))

(defparser json-null ()
  (progn '"null" (constantly (construct-null))))

(defparser json-value ()
  (or (json-boolean) (json-string) (json-number) (json-array) (json-object) (json-null)))

(defparser json-array ()
  (for ((list (prog2 '#\[ (opt (cons #1=(json-trim (json-value)) (rep (progn '#\, (cut #1#))))) (cut '#\]))))
    (construct-sequence list)))

(defparser json-field ()
  (for ((key (json-trim (json-string)))
        (nil (cut '#\:))
        (value (cut (json-trim (cut (json-value))))))
    (cons key value)))

(defparser json-object ()
  (for ((alist (prog2 '#\{ (opt (cons #1=(json-field) (rep (progn '#\, (cut #1#))))) (cut '#\}))))
    (construct-mapping alist)))

(defparser json-file (&optional junk-allowed)
  (let ((result (json-trim (json-value))))
    (prog1 (constantly result)
      (rep (eof) (if junk-allowed 0 1) 1))))

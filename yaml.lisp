(in-package #:yamson)

(defparser yaml-indent (min &optional (max most-positive-fixnum))
  (for ((spaces (rep '#\Space min max)))
    (declare (type list spaces))
    (length spaces)))

(defparser yaml-whitespace-char ()
  '#\Space)

(defparser yaml-whitespaces (&optional (min 0) (max most-positive-fixnum))
  (for ((spaces (rep (yaml-whitespace-char) min max)))
    (declare (type list spaces))
    (length spaces)))

(declaim (ftype (function (character) (values boolean)) yaml-newline-char-p)
         (inline yaml-newline-char-p))
(defun yaml-newline-char-p (char)
  (or (char= char #\Newline) (char= char #\Return)))

(defparser yaml-newline-char ()
  (satisfies #'yaml-newline-char-p))

(defparser yaml-comment ()
  (for ((comment (progn '#\# (rep (satisfies (lambda (x) (not (yaml-newline-char-p x))))))))
    (coerce comment 'string)))

(defparser yaml-eol ()
  (yaml-whitespaces)
  (opt (yaml-comment))
  (constantly nil))

(defparser yaml-eof ()
  (yaml-eol)
  (eof))

(defparser yaml-newline ()
  (yaml-eol)
  (yaml-newline-char))

(defparser yaml-newline-indent (min &optional (max most-positive-fixnum))
  (rep (yaml-newline) 1)
  (yaml-indent min max))

(defparser yaml-end-of-simple-value (&optional (level most-positive-fixnum))
  (or (yaml-newline-char) (yaml-whitespace-char) (eof))
  (rep (yaml-newline)) (not (and (yaml-indent level) (not (yaml-eof)))))

(defparser yaml-end-of-indicator ()
  (or (yaml-end-of-simple-value) (yaml-flow-brackets) '#\,))

(defparser yaml-block-sequence (level)
  (for ((elems (repsep
                (progn '#\- (peek (yaml-end-of-indicator)) (yaml-value level))
                (yaml-newline-indent level level)
                1)))
    (copy-list elems)))

(defparser yaml-block-mapping-element (level)
  (for ((key (yaml-string-unquoted '#\:))
        (nil (progn '#\: (peek (yaml-end-of-indicator))))
        (value (or (progn (yaml-newline-indent level level) (yaml-block-sequence level)) (yaml-value level))))
    (cons key value)))

(defparser yaml-block-mapping (level)
  (for ((fields (repsep (yaml-block-mapping-element level) (yaml-newline-indent level level) 1)))
    (copy-list fields)))

(defparser yaml-mixed-indent (level)
  (or (yaml-newline-indent level)
      (for ((spaces (yaml-whitespaces)))
        (+ spaces level))))

(declaim (ftype (function (list t character &optional list list) (values string)) yaml-string-multiline-impl))
(defun yaml-string-multiline-impl (characters-list mode separator &optional prefixes (suffixes '(#\Newline)))
  (if characters-list
      (let ((string (make-array (1+ (loop :for characters :of-type list :in characters-list
                                          :sum (1+ (length characters))
                                            :of-type non-negative-fixnum))
                                :element-type 'character))
            (index 0))
        (declare (type (simple-array character (*)) string)
                 (type fixnum index))
        (macrolet ((buffer-push (character)
                     `(prog1 (setf (aref string index) ,character)
                        (incf index)))
                   (buffer-pop ()
                     `(aref string (decf index)))
                   (buffer-length ()
                     'index))
          (loop :for character :in prefixes
                :do (buffer-push character))
          (loop :for (characters . (characters-next)) :on characters-list
                :do (loop :for character :in (or characters (return))
                          :do (buffer-push character))
                    (when (or (char= separator #\Newline) (or characters (null characters-next)))
                      (buffer-push (if (and characters characters-next) separator #\Newline))))
          (when (plusp (buffer-length))
            (buffer-pop))
          (unless (eql mode #\+)
            (loop :for character :of-type character := (buffer-pop)
                  :unless (char= character #\Newline)
                    :return (buffer-push character)
                  :until (zerop (buffer-length))))
          (unless (eql mode #\-)
            (loop :for character :in suffixes
                  :do (buffer-push character)))
          (adjust-array string (buffer-length))))
      ""))

(defparser yaml-string-single-quoted-line ()
  (rep (or (progn '#\' '#\') (satisfies (lambda (x) (not (or (yaml-newline-char-p x) (char= x #\'))))))))

(defparser yaml-string-single-quoted ()
  (for ((characters-list (prog2 '#\'
                             (cons (yaml-string-single-quoted-line)
                                   (rep (progn (yaml-newline-char) (yaml-whitespaces) (yaml-string-single-quoted-line))))
                           (cut '#\'))))
    (yaml-string-multiline-impl characters-list #\+ #\Space nil nil)))

(defparser yaml-string-double-quoted-line ()
  (rep (or (progn '#\\ (cut (json-string-escape-char))) (satisfies (lambda (x) (not (or (yaml-newline-char-p x) (char= x #\"))))))))

(defparser yaml-string-double-quoted ()
  (for ((characters-list (prog2 '#\"
                             (cons (yaml-string-double-quoted-line)
                                   (rep (progn (yaml-newline-char) (yaml-whitespaces) (yaml-string-double-quoted-line))))
                           (cut '#\"))))
    (yaml-string-multiline-impl characters-list #\+ #\Space nil nil)))

(defparser yaml-string-unquoted-character (terminator)
  (and (not (progn (or (progn (yaml-whitespaces 1) (yaml-comment)) (yaml-whitespaces))
                   (or (yaml-newline-char) (eof) terminator)))
       (satisfies #'characterp)))

(defparser yaml-string-unquoted-line-rest (terminator)
  (rep (yaml-string-unquoted-character terminator)))

(defparser yaml-string-unquoted-line (terminator)
  (cons (and (not (yaml-end-of-indicator)) (yaml-string-unquoted-character terminator))
        (yaml-string-unquoted-line-rest terminator)))

(defparser yaml-string-unquoted (terminator)
  (for ((characters (yaml-string-unquoted-line terminator)))
    (declare (type list characters))
    (coerce characters 'string)))

(defparser yaml-string-unquoted-multiline (level &optional (terminator (or)))
  (for ((characters-list (cons (yaml-string-unquoted-line terminator)
                               (rep (progn
                                      (yaml-newline)
                                      (or (progn (yaml-eol) (peek (yaml-newline-char)) (constantly nil))
                                          (progn (yaml-indent level) (yaml-string-unquoted-line-rest terminator))))))))
    (yaml-string-multiline-impl characters-list #\- #\Space)))

(defparser yaml-string-multiline-line ()
  (rep (satisfies (lambda (x) (not (yaml-newline-char-p x))))))

(defparser yaml-string-multiline-content (level mode separator &optional (leading nil))
  (for ((characters-list (opt (cons (yaml-string-multiline-line)
                                    (rep (progn
                                           (yaml-newline-char)
                                           (or (progn (yaml-indent level level) (yaml-string-multiline-line))
                                               (progn
                                                 (yaml-whitespaces)
                                                 (peek (yaml-newline-char))
                                                 (constantly nil)))))))))
    (yaml-string-multiline-impl characters-list mode separator leading)))

(defparser yaml-string-multiline-body (level separator)
  (let ((mode (opt (or '#\+ '#\-)))
        (nil (yaml-eol))
        (leading (rep (prog2 (yaml-newline-char)
                          (yaml-whitespaces)
                        (peek (yaml-newline-char))))))
    (or (let ((nil (yaml-newline-char))
              (level (yaml-indent (max level (loop :for cons :on (or leading (return most-negative-fixnum))
                                                   :for indent :of-type non-negative-fixnum := (car cons)
                                                   :do (setf (car cons) #\Newline)
                                                   :maximize indent :of-type fixnum)))))
          (yaml-string-multiline-content level mode separator leading))
        (constantly ""))))

(defparser yaml-string-multiline-literal (level)
  (progn '#\| (yaml-string-multiline-body level #\Newline)))

(defparser yaml-string-multiline-folded (level)
  (progn '#\> (yaml-string-multiline-body level #\Space)))

(defparser yaml-string-multiline (level)
  (or (yaml-string-multiline-literal level) (yaml-string-multiline-folded level)))

(defparser yaml-boolean ()
  (or (json-boolean)
      (progn (or'"True" '"TRUE") (constantly t))
      (progn (or'"False" '"FALSE") (constantly nil))))

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

(defparser yaml-flow-whitespaces ()
  (rep (yaml-newline) 0)
  (yaml-whitespaces))

(defparser yaml-flow-trim (parser)
  (prog2 (yaml-flow-whitespaces) parser (yaml-flow-whitespaces)))

(defparser yaml-flow-brackets ()
  (or '#\{ '#\[ '#\] '#\}))

(defparser yaml-flow-object-terminator ()
  (or (yaml-flow-brackets) '#\, (prog1 '#\: (peek (yaml-end-of-indicator)))))

(defparser yaml-flow-string-unquoted ()
  (yaml-string-unquoted (yaml-flow-object-terminator)))

(defparser yaml-flow-value (&optional value-required-p)
  (yaml-flow-whitespaces)
  (or (prog1 (yaml-value-unquoted)
        (yaml-flow-whitespaces)
        (peek (yaml-flow-object-terminator)))
      (prog1 (or (yaml-value-quoted) (yaml-flow-string-unquoted))
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
  (or (json-null) (progn '#\~ (constantly :null))))

(defparser yaml-value-unquoted ()
  (or (yaml-boolean) (yaml-number) (yaml-null)))

(defparser yaml-value-quoted ()
  (or (yaml-string-single-quoted) (yaml-string-double-quoted)
      (yaml-flow-sequence) (yaml-flow-mapping)))

(defparser yaml-value-simple ()
  (or (yaml-value-unquoted) (yaml-value-quoted)))

(defparser yaml-value (parent-level)
  (let ((level (constantly (1+ parent-level))))
    (declare (type non-negative-fixnum level))
    (let ((child-level (yaml-mixed-indent level)))
      (declare (type non-negative-fixnum child-level))
      (or (prog1 (yaml-value-simple) (peek (yaml-end-of-simple-value level)))
          (yaml-string-multiline level)
          (yaml-block-sequence child-level) (yaml-block-mapping child-level)
          (yaml-string-unquoted-multiline level) (constantly :null)))))

(defparser yaml-file (&optional junk-allowed)
  ((lambda (result)
     (if junk-allowed
         (parser (constantly result))
         (parser (prog1 (constantly result) (rep (yaml-newline)) (yaml-eof)))))
   (yaml-value -1)))

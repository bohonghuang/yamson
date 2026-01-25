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
  (for ((comment (progn '#\# (rep (satisfies (lambda (x) (not (yaml-newline-char-p x))))))))
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

(defparser yaml-end-of-simple-value ()
  (yaml-eol) (or (yaml-newline-char) (eof)))

(defparser yaml-end-of-indicator ()
  (or (yaml-end-of-simple-value) (yaml-whitespaces 1) (yaml-flow-brackets) '#\,))

(defparser yaml-block-sequence (level)
  (for ((elems (repsep
                (progn '#\- (peek (yaml-end-of-indicator)) (yaml-value level))
                (yaml-newline-indent level level)
                1)))
    (copy-list elems)))

(defparser yaml-block-mapping-element (level)
  (for ((key (yaml-string-unquoted '#\:))
        (nil (progn '#\: (peek (yaml-end-of-indicator))))
        (value (yaml-value level)))
    (cons key value)))

(defparser yaml-block-mapping (level)
  (for ((fields (repsep (yaml-block-mapping-element level) (yaml-newline-indent level level) 1)))
    (copy-list fields)))

(defparser yaml-mixed-indent (level)
  (or (yaml-newline-indent level)
      (for ((spaces (yaml-whitespaces)))
        (+ spaces level))))

(defparser yaml-string-single-quoted ()
  (for ((characters (prog2 '#\' (rep (or (progn '#\' '#\') (satisfies (lambda (x) (not (char= x #\')))))) (cut '#\'))))
    (declare (type list characters))
    (coerce characters 'string)))

(defparser yaml-string-double-quoted ()
  (json-string))

(defparser yaml-string-unquoted-character (terminator)
  (and (not (progn (or (progn (yaml-whitespaces 1) (yaml-comment)) (yaml-whitespaces))
                   (or (yaml-newline-char) (eof) terminator)))
       (satisfies #'characterp)))

(defparser yaml-string-unquoted (&optional (terminator (or)))
  (for ((characters (cons (and (not (yaml-end-of-indicator)) (yaml-string-unquoted-character terminator))
                          (rep (yaml-string-unquoted-character terminator)))))
    (declare (type list characters))
    (coerce characters 'string)))

(declaim (ftype (function (list t character &optional list) (values string)) yaml-string-multiline-impl))
(defun yaml-string-multiline-impl (characters-list option separator &optional leading)
  (if characters-list
      (loop :with string := (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character)
            :and characters-all := nil
            :initially (loop :for character :in leading :do (vector-push-extend character string))
            :for characters-list-cons-previous := nil :then characters-list-cons
            :for characters-list-cons :on characters-list
            :for (characters . (characters-next)) := characters-list-cons
            :do (loop :for character-cons-previous := nil :then character-cons
                      :for character-cons :on (or characters (return))
                      :for (character) := character-cons
                      :do (vector-push-extend character string)
                      :finally (setf (cdr character-cons-previous) characters-all
                                     characters-all characters))
                (when (or (char= separator #\Newline) (or characters (null characters-next)))
                  (vector-push-extend (if (and characters characters-next) separator #\Newline) string))
            :finally
               (vector-pop string)
               (unless (eql option #\+)
                 (loop :for character :of-type character := (vector-pop string)
                       :unless (char= character #\Newline)
                         :return (vector-push character string)
                       :until (zerop (length string))))
               (unless (eql option #\-)
                 (vector-push #\Newline string))
               (setf (cdr characters-list-cons-previous) characters-all)
               (return string))
      ""))

(defparser yaml-string-multiline-line ()
  (for ((characters (rep (satisfies (lambda (x) (not (yaml-newline-char-p x)))))))
    (copy-list characters)))

(defparser yaml-string-multiline-content (level separator)
  (let ((option (opt (or '#\+ '#\-)))
        (nil (yaml-eol))
        (leading (rep (prog2 (yaml-newline-char)
                          (yaml-whitespaces)
                        (peek (yaml-newline-char))))))
    (or (let ((nil (yaml-newline-char))
              (level (yaml-indent (max level (loop :for cons :on (or leading (return most-negative-fixnum))
                                                   :for indent :of-type non-negative-fixnum := (car cons)
                                                   :do (setf (car cons) #\Newline)
                                                   :maximize indent :of-type fixnum)))))
          (for ((characters-list (opt (cons (yaml-string-multiline-line)
                                            (rep (progn
                                                   (yaml-newline-char)
                                                   (or (progn
                                                         (yaml-indent level level)
                                                         (yaml-string-multiline-line))
                                                       (progn
                                                         (yaml-whitespaces)
                                                         (peek (yaml-newline-char))
                                                         (constantly nil)))))))))
            (yaml-string-multiline-impl characters-list option separator leading)))
        (constantly ""))))

(defparser yaml-string-multiline-literal (level)
  (progn '#\| (yaml-string-multiline-content level #\Newline)))

(defparser yaml-string-multiline-folded (level)
  (progn '#\> (yaml-string-multiline-content level #\Space)))

(defparser yaml-string-multiline (level)
  (or (yaml-string-multiline-literal level) (yaml-string-multiline-folded level)))

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

(defparser yaml-value (level)
  (let ((level (yaml-mixed-indent (1+ level))))
    (declare (type non-negative-fixnum level))
    (or (prog1 (yaml-value-simple) (peek (yaml-end-of-simple-value)))
        (yaml-string-multiline level)
        (yaml-block-sequence level) (yaml-block-mapping level)
        (yaml-string-unquoted) (constantly :null))))

(defparser yaml-file (&optional junk-allowed)
  ((lambda (result)
     (if junk-allowed
         (parser (constantly result))
         (parser (prog1 (constantly result) (rep (yaml-newline)) (yaml-eol) (eof)))))
   (yaml-value -1)))

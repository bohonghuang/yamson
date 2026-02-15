(in-package #:yamson)

(declaim (ftype (function (list t character &optional list list) (values string)) yaml-string-multiline-impl))
(defun yaml-string-multiline-impl (characters-list mode separator &optional prefixes (suffixes (case mode (#\-) (t '(#\Newline)))))
  (if characters-list
      (let ((string (make-array (+ (loop :for characters :of-type list :in characters-list
                                         :sum (1+ (length characters))
                                           :of-type non-negative-fixnum)
                                   (length prefixes)
                                   (1- (length suffixes)))
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
          (loop :for (characters . has-next-p) :on characters-list
                :for (characters-next) := has-next-p
                :do (loop :for character :in (or characters (return))
                          :do (buffer-push character))
                :when has-next-p
                  :when (or (char= separator #\Newline) (or characters (null characters-next)))
                    :do (buffer-push (if (and characters characters-next) separator #\Newline)))
          (unless (eql mode #\+)
            (loop :for character :of-type character := (buffer-pop)
                  :unless (char= character #\Newline)
                    :return (buffer-push character)
                  :until (zerop (buffer-length))))
          (unless (eql mode #\-)
            (loop :for character :in suffixes
                  :do (buffer-push character)))
          (if (= (length string) (buffer-length)) string (adjust-array string (buffer-length)))))
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
  (and (not (progn
              (or (progn (yaml-whitespaces 1) (yaml-comment)) (yaml-whitespaces))
              (or (yaml-indicator '#\:) (yaml-newline-char) (eof) terminator)))
       (satisfies #'characterp)))

(defparser yaml-string-unquoted-line-rest (terminator)
  (rep (yaml-string-unquoted-character terminator)))

(defparser yaml-string-unquoted-line (terminator)
  (cons (and (not (yaml-end-of-indicator)) (yaml-string-unquoted-character terminator))
        (yaml-string-unquoted-line-rest terminator)))

(defparser yaml-string-unquoted (&optional (terminator (or)))
  (for ((characters (yaml-string-unquoted-line terminator)))
    (declare (type list characters))
    (coerce characters 'string)))

(defparser yaml-string-unquoted-multiline (level &optional (terminator (or)))
  (for ((characters-list (cons (yaml-string-unquoted-line terminator)
                               (rep (progn
                                      (yaml-newline)
                                      (and
                                       (not (yaml-document-indicator))
                                       (or (progn (yaml-eol) (peek (yaml-newline-char)) (constantly nil))
                                           (progn (yaml-indent level) (yaml-string-unquoted-line-rest terminator)))))))))
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

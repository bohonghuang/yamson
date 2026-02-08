(in-package #:yamson)

(declaim (ftype (function (character) (values boolean)) yaml-whitespace-char-p)
         (inline yaml-whitespace-char-p))
(defun yaml-whitespace-char-p (char)
  (char= char '#\Space))

(defparser yaml-whitespace-char ()
  (satisfies #'yaml-whitespace-char-p))

(defparser yaml-whitespaces (&optional (min 0) (max most-positive-fixnum))
  (for ((spaces (rep (yaml-whitespace-char) min max)))
    (declare (type list spaces))
    (length spaces)))

(defparser yaml-indent (min &optional (max most-positive-fixnum))
  (yaml-whitespaces min max))

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

(defparser yaml-end-of-level (level)
  (let ((level-next (yaml-indent 0)))
    (rep (yaml-eof) (if (> level-next level) 1 0) 1)
    (constantly nil)))

(defparser yaml-end-of-value (&optional (level most-positive-fixnum))
  (or (eof)
      (progn
        (yaml-newline-char)
        (yaml-newlines)
        (yaml-end-of-level level))
      (progn
        (yaml-whitespace-char)
        (yaml-eol)
        (or (progn
              (yaml-newline-char)
              (yaml-end-of-level level))
            (eof)))))

(defparser yaml-end-of-indicator ()
  (peek (or (yaml-whitespace-char) (yaml-end-of-value) (yaml-flow-brackets) '#\,)))

(defparser yaml-indicator (parser)
  (prog1 parser (yaml-end-of-indicator)))

(defparser yaml-mixed-indent (level)
  (or (yaml-newline-indent level)
      (for ((spaces (yaml-whitespaces)))
        (declare (type non-negative-fixnum spaces level))
        (the non-negative-fixnum (+ spaces level)))))

(defparser yaml-flow-brackets ()
  (or '#\{ '#\[ '#\] '#\}))

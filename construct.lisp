(in-package #:yamson)

(defparameter *constructor-sequence* #'copy-list)

(defun construct-sequence (list)
  (funcall *constructor-sequence* list))

(defparameter *constructor-mapping* #'copy-alist)

(defun construct-mapping (alist)
  (funcall *constructor-mapping* alist))

(defparameter *constructor-boolean* #'identity)

(defun construct-boolean (boolean)
  (funcall *constructor-boolean* boolean))

(defparameter *constructor-null* (constantly :null))

(defun construct-null ()
  (funcall *constructor-null*))

(defparameter *constructors* `((construct-sequence . *constructor-sequence*)
                               (construct-mapping . *constructor-mapping*)
                               (construct-boolean . *constructor-boolean*)
                               (construct-null . *constructor-null*)))

(defmacro with-constructors (&body body)
  (let* ((bindings (loop :for (nil . variable) :in *constructors*
                         :collect (list (gensym (symbol-name variable)) variable))))
    (with-gensyms (args)
      `(let ,bindings
         (declare (type function . ,(mapcar #'first bindings))
                  (ignorable . ,(mapcar #'first bindings)))
         (macrolet ,(loop :for (function . nil) :in *constructors*
                          :for (variable nil) :in bindings
                          :collect `(,function (&rest ,args) `(funcall ,',variable . ,,args)))
           ,@body)))))

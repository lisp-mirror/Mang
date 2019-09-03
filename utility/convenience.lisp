(in-package #:mang)

(defmacro chop (curr-var list-var &body body)
  `(bind (((,@(if (listp curr-var)
                  curr-var
                  `(,curr-var))
              &rest ,list-var)
           ,list-var))
     (declare (type list ,list-var)
              (ignorable ,list-var))
     ,@body))

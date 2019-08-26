(in-package #:mang)

(defmacro chop (curr-var list-var &body body)
  `(bind (((,@(ensure-list curr-var)
              &rest ,list-var)
           ,list-var))
     (declare (type list ,list-var))
     ,@body))

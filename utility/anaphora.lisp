(in-package #:mang)

(defmacro [a]if (test then &body else)
  `(bind ((it ,test))
     (if it
         ,then
         (progn
           ,@else))))

(defmacro [d]if ((predicate it &rest args)
                                 then
                 &body else)
  `(bind ((it ,it))
     (if (,predicate it ,@args)
         ,then
         (progn
           ,@else))))

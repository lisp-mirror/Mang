(in-package #:mang)

(defmacro [a]if (test then &body else)
  `(let ((it ,test))
     (if it
         ,then
         (progn
           ,@else))))

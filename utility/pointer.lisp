(in-package #:mang)

(defmacro & (place)
  (bind (((:values vars vals news setter getter)
          (get-setf-expansion place))
         (new (first news))
         (g!new-value (gensym "new-value"))
         (g!set? (gensym "set?")))
    `(let* (,@(mapcar #'list
                      vars vals))
       (lambda (&optional (,g!new-value nil ,g!set?))
         (if ,g!set?
             (let ((,new ,g!new-value))
               ,setter)
             ,getter)))))

(defun &<- (pointer)
  (funcall pointer))

(defsetf &<- (place)
    (value)
  `(funcall ,place ,value))

(defsetf & (place)
    (value)
  `(setf (&<- ,place)
         ,value))

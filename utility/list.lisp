(in-package #:mang)

(defun ensure-list (obj)
  (if (consp obj)
      obj
      (list obj)))

(defun repeat (n obj &optional (intersperse nil intersperse?))
  (when (> n 0)
    (if (and intersperse? (> n 1))
        (list* obj intersperse (repeat (1- n)
                                       obj intersperse))
        (cons obj (repeat (1- n)
                          obj)))))

(defun var-repeat (min max obj &optional (intersperse nil intersperse?))
  (let ((collected (empty-set)))
    (if intersperse?
        (loop :for n :from min :to max
           :do (setf collected
                     (with collected (repeat n obj intersperse))))
        (loop :for n :from min :to max
           :do (setf collected
                     (with collected (repeat n obj)))))
    collected))

(defun intersperse (item list)
  (if (rest list)
      (list* (first list)
             item (intersperse item (rest list)))
      list))

(defmethod tree-map (f (tree null))
  (declare (ignore f tree))
  nil)

(defmethod tree-map (f tree)
  (@ f tree))

(defmethod tree-map (f (tree cons))
  (cons (tree-map f (car tree))
        (tree-map f (cdr tree))))

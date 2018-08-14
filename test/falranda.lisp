(in-package #:mang)

(defparameter *falranda*
  (list (add-population (world)
                        "urdragons"
                        (<population> *dragon-dictionary* 100))))

(defmacro apply-world-changes (place &body changes)
  (when changes
    `(progn
       (setf ,place
             (cons (first ,place)
                   ,place))
       ,@(loop :for change :in changes
            :collect
            `(setf (first ,place)
                   (,(first change)
                     (first ,place)
                     ,@(rest change)))))))

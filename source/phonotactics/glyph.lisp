(in-package #:mang)

(defmodel glyph-system ()
  ((%phoneme-classes :type map
                     :accessor classes<-
                     :initarg :phoneme-classes
                     :initform (c_in (empty-map (empty-set))))
   (%glyphs :type set
            :reader glyphs<-
            :initform (c? (expand (lambda (k v)
                                    (declare (ignore k))
                                    v)
                                  (classes<- self))))))

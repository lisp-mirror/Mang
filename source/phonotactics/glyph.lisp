(in-package #:mang)

(defmodel glyph-system ()
  ((%classes :type map
             :accessor classes<-
             :initarg :phoneme-classes)
   (%glyphs :type set
            :reader glyphs<-
            :initform (c? (expand (lambda (k v)
                                    (declare (ignore k))
                                    v)
                                  (classes<- self))))))

(defun glyph-system (phoneme-classes)
  (make-instance 'glyph-system
                 :phoneme-classes (c_in (with-default phoneme-classes
                                          (empty-set)))))

(in-package #:mang)

(defmodel glyph-system ()
  ((%classes :type map
             :accessor classes<-
             :initarg :classes)
   (%glyphs :type set
            :initform (c? (expand (lambda (k v)
                                    (declare (ignore k))
                                    v)
                                  (classes<- self))))))

(defun glyph-system (classes)
  (make-instance 'glyph-system
                 :classes (c_in (with-default classes
                                  (empty-set)))))

(defmethod glyphs<- ((obj glyph-system))
  (with (slot-value obj '%glyphs)
        ""))

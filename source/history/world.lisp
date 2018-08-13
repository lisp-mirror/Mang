(in-package #:mang)

(defclass population ()
  ((%dialect :type dictionary
             :reader dialect<-
             :initarg :dialect)
   (%size :type (integer (0))
          :reader :size
          :initarg :size)))

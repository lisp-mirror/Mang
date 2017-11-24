(in-package #:mang)

(defclass sound-change ()
  ((%source :type function
            :reader source<-
            :initarg :source
            :initform (create-scanner `(:sequence)))
   (%target :type (or cons null)
            :reader target<-
            :initarg :target
            :initform nil)
   (%pre :type function
         :reader pre<-
         :initarg :pre
         :initform (create-scanner `(:sequence)))
   (%post :type function
          :reader post<-
          :initarg :post
          :initform (create-scanner `(:sequence)))))

;;;; ANNOTATED-STRING<-WORD is necessary(?) to be able to use CL-PPCRE – it only
;;;; works on strings. This means that "#" has to be an illegal character in any
;;;; given glyph.
(defmethod annotated-string<-word ((word cons))
  (format nil "~{~A~}"
          (intersperse "#" word)))

(defmethod annotated-string<-word ((word word))
  (annotated-string<-word (form<- word)))

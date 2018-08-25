(in-package #:mang)

(defmethod export-dictionary (stream (dictionary set)
                              (delimiter string)
                              (fields cons))
  (unless (empty? dictionary)
    (let ((entry (arb dictionary)))
      (loop :for (field delim?)
         :on fields
         :do
         (if delim?
             (format stream "~A~A"
                     (funcall field entry)
                     delimiter)
             (format stream "~A~%"
                     (funcall field entry))))
      (export-dictionary stream (less dictionary entry)
                         delimiter fields))))

(defmethod export-dictionary (stream (dictionary dictionary)
                              (delimiter string)
                              (fields cons))
  (export-dictionary stream (content<- dictionary)
                     delimiter fields))

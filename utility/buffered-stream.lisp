(in-package #:mang)

(defclass bus ()
  ((%buffer :type string
            :initform ""
            :initarg :buffer
            :accessor buffer<-)
   (%stream :type stream
            :initarg :stream
            :reader stream<-)))

(bind ((registry (make-weak-hash-table :weakness :key
                                       :test 'eq)))
  (defun bus (stream &optional (buffer ""))
    (bind ((bus (make-instance 'bus
                               :buffer buffer
                               :stream stream)))
      (setf (gethash stream registry)
            (cons (make-weak-pointer bus)
                  (gethash stream registry)))
      bus))

  (defun stream-buses (stream)
    (gethash stream registry))

  (defun bus-read (bus &optional (length 1))
    (declare (type bus bus)
             (type (integer 0)
                   length))
    (with-accessors ((buffer buffer<-)
                     (stream stream<-))
        bus
      (if (or (length= buffer length)
              (length> buffer length))
          (values (subseq buffer 0 length)
                  t)
          (bind ((length (- length (length buffer)))
                 (buses (image #'weak-pointer-value
                               (gethash stream registry)))
                 (read (make-string length))
                 (count (read-sequence read stream))
                 (read (subseq read 0 count)))
            (dolist (bus buses)
              (setf (buffer<- bus)
                    (concatenate 'string
                                 (buffer<- bus)
                                 read)))
            (values (buffer<- bus)
                    (= count length))))))

  (defun bus-consume (bus &optional (length 1))
    (declare (type bus bus)
             (type (integer 0)
                   length))
    (bus (stream<- bus)
         (subseq (buffer<- bus)
                 length))))

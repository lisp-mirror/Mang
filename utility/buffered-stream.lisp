(in-package #:mang)

(defclass bus ()
  ((%buffer :type string
            :initform ""
            :initarg :buffer
            :accessor buffer<-)
   (%stream :type stream
            :initarg :stream
            :reader stream<-)
   (%child :type (or bus null)
           :initarg :child
           :initform nil
           :accessor child<-)))

(defun bus-flatten (bus)
  (declare (type bus bus))
  (with-accessors ((child child<-)
                   (buffer buffer<-))
      bus
    (when child
      (setf buffer
            (concatenate 'string
                         buffer (buffer<- (bus-flatten child)))
            child nil))
    bus))

(defun buffer<-* (bus)
  (declare (type bus bus))
  (bus-flatten bus)
  (buffer<- bus))

(defmethod print-object ((object bus)
                         stream)
  (format stream "#<BUS ~S ~S>"
          (buffer<- object)
          (child<- object)))

(defun bus (stream &key (buffer "")
                     parent)
  (declare (type stream stream)
           (type string buffer)
           (type (or bus null)
                 parent))
  (bind ((bus (make-instance 'bus
                             :buffer buffer
                             :stream stream)))
    (when parent
      (bus-flatten parent)
      (setf (child<- parent)
            bus))
    bus))

(defun bus-read (bus &optional (length 1))
  (declare (type bus bus)
           (type (integer 0)
                 length))
  (bus-flatten bus)
  (with-accessors ((buffer buffer<-)
                   (stream stream<-))
      bus
    (if (length>= buffer length)
        (values (subseq buffer 0 length)
                t)
        (bind ((length (- length (length buffer)))
               (read (make-string length))
               (count (read-sequence read stream))
               (read (subseq read 0 count)))
          (setf buffer
                (concatenate 'string
                             buffer read))
          (values buffer (= count length))))))

(defun bus-consume (bus &optional (length 1))
  (declare (type bus bus)
           (type (integer 0)
                 length))
  (bus-flatten bus)
  (with-accessors ((buffer buffer<-)
                   (stream stream<-))
      bus
    (when (length> length buffer)
      (error "Tried to consume length ~A of bus ~A with buffer ~S of length ~A"
             length bus buffer (length buffer)))
    (bind ((begin (subseq buffer 0 length))
           (end (subseq buffer length)))
      (setf buffer begin)
      (bus stream
           :buffer end
           :parent bus))))

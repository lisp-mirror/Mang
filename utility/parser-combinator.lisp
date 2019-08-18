(in-package #:mang)

;;;; buffered stream
(defclass buffered-stream ()
  ((%buffer :type string
            :initform ""
            :initarg :buffer
            :accessor buffer<-)
   (%stream :type stream
            :initarg :stream
            :accessor stream<-)))

(defun buffered-stream (stream &optional (buffer ""))
  (declare (type stream stream)
           (type string buffer))
  (make-instance 'buffered-stream
                 :buffer buffer
                 :stream stream))

(defun buffered-stream-read-char (buffered-stream &optional eof)
  (declare (type buffered-stream buffered-stream))
  (with-accessors ((buffer buffer<-)
                   (stream stream<-))
      buffered-stream
    (if (length> buffer 0)
        (prog1
            (elt buffer 0)
          (setf buffer
                (subseq buffer 1)))
        (read-char stream
                   nil eof))))

(defun buffered-stream-read-to (buffered-stream length)
  (declare (type buffered-stream buffered-stream)
           (type (integer (0))
                 length))
  (with-accessors ((buffer buffer<-)
                   (stream stream<-))
      buffered-stream
    (if (or (length= buffer length)
            (length> buffer length))
        (prog1
            (subseq buffer 0 length)
          (setf buffer
                (subseq buffer length)))
        (bind ((length (- length (length buffer)))
               (read (make-string length)))
          (prog1
              ([d]if (< (read-sequence read stream)
                        length)
                  (values (concatenate 'string
                                       buffer (subseq read 0 it))
                          nil)
                (values (concatenate 'string
                                     buffer read)
                        t))
            (setf buffer ""))))))

(defun buffered-stream-unread (buffered-stream object)
  (declare (type buffered-stream buffered-stream))
  (setf (buffer<- buffered-stream)
        (concatenate 'string
                     (string object)
                     (buffer<- buffered-stream)))
  buffered-stream)

(defgeneric parser-call (parser to-parse)
  (:method ((parser function)
            (to-parse buffered-stream))
    (funcall parser to-parse))
  (:method ((parser function)
            (to-parse string))
    (with-input-from-string (stream to-parse)
      (funcall parser (buffered-stream stream))))
  (:method ((parser function)
            (to-parse stream))
    (funcall parser (buffered-stream to-parse))))

;;;; Monadic operations
(defun succeed (x)
  (lambda (s)
    (values x s t)))

(defun fail (e)
  (lambda (s)
    (values e s nil)))

(defun ^$ (p x)
  (lambda (s)
    (bind (((:values r _ success?)
            (parser-call p x)))
      (values r s success?))))

(defun <$~> (fs fe a)
  (declare (type function fs fe a))
  (lambda (s)
    (bind (((:values r ns success?)
            (parser-call a s)))
      (values (if success?
                  (funcall fs r)
                  (funcall fe r))
              ns success?))))

(defun <$> (fs a)
  (declare (type function fs))
  (<$~> fs #'identity a))

(defun <~> (fe a)
  (declare (type function fe))
  (<$~> #'identity fe a))

(defun <$~ (s e a)
  (<$~> (constantly s)
        (constantly e)
        a))

(defun <$ (s a)
  (<$> (constantly s)
       a))

(defun <~ (e a)
  (<~> (constantly e)
       a))

(defun //= (xa fa)
  (declare (type function xa fa))
  (lambda (s)
    (bind (((:values r ns success?)
            (parser-call xa s)))
      (if success?
          (values r ns t)
          (parser-call (funcall fa r)
                       s)))))

(defun // (parser &rest parsers)
  (declare (type function parser))
  (if parsers
      (//= parser (constantly (apply #'//
                                     parsers)))
      parser))

(defmacro //* (left var right)
  (bind ((g!arg (gensym "arg")))
    `(//= ,left (lambda (,g!arg)
                  (bind ((,var ,g!arg))
                    ,right)))))

(defmacro //! (&body bindings)
  (if (rest bindings)
      (bind (((var parser &rest bindings)
              bindings))
        (if (and (symbolp var)
                 (string= (symbol-name var)
                          "_"))
            `(// ,parser (//! ,@bindings))
            `(//* ,parser ,var (//! ,@bindings))))
      (first bindings)))

(defun >>= (xa fa)
  (declare (type function xa fa))
  (lambda (s)
    (bind (((:values r ns success?)
            (parser-call xa s)))
      (if success?
          (parser-call (funcall fa r)
                       ns)
          (values r s nil)))))

(defun >> (parser &rest parsers)
  (declare (type function parser))
  (if parsers
      (>>= parser (constantly (apply #'>>
                                     parsers)))
      parser))

(defmacro >>* (left var right)
  (bind ((g!arg (gensym "arg")))
    `(>>= ,left (lambda (,g!arg)
                  (bind ((,var ,g!arg))
                    ,right)))))

(defmacro >>! (&body bindings)
  (if (rest bindings)
      (bind (((var parser &rest bindings)
              bindings))
        (if (and (symbolp var)
                 (string= (symbol-name var)
                          "_"))
            `(>> ,parser (>>! ,@bindings))
            `(>>* ,parser ,var (>>! ,@bindings))))
      (first bindings)))

(defun ??== (ptest xthen xelse)
  (declare (type function ptest xthen xelse))
  (lambda (s)
    (bind (((:values r ns success?)
            (parser-call ptest s)))
      (parser-call (funcall (if success?
                                xthen
                                xelse)
                            r)
                   ns))))

(defun ??= (ptest xthen pelse)
  (declare (type function ptest xthen pelse))
  (??== ptest xthen (constantly pelse)))

(defun ?? (ptest pthen pelse)
  (declare (type function ptest pthen pelse))
  (??= ptest (constantly pthen)
       pelse))

(defmacro ??! (var ptest pthen pelse)
  (bind ((g!arg (gensym "arg")))
    `(??= ,ptest
          (lambda (,g!arg)
            (declare (ignorable ,g!arg))
            (bind ((,var ,g!arg))
              ,pthen))
          ,pelse)))

(defun <?> (p &optional (d #'identity))
  (declare (type function p d))
  (//* p x (succeed (funcall d x))))

(defun <? (p &optional d)
  (<?> p (constantly d)))

(defun some (p &optional (d "")
                 (f (lambda (a b)
                      (concatenate 'string
                                   a b))))
  (declare (type function p f))
  (>>!
    x p
    xs (// (some p d f)
           (succeed d))
    (succeed (funcall f x xs))))

(defun many (p &optional (d "")
                 (f (lambda (a b)
                      (concatenate 'string
                                   a b))))
  (declare (type function p f))
  (// (some p d f)
      (succeed d)))

(defun ^< (p)
  (declare (type function p))
  (lambda (s)
    (bind (((r ns success?)
            (parser-call p s)))
      (if success?
          (values r s t)
          (values r ns nil)))))

;;;; basic parsers
(let ((eof (gensym "eof")))
  (defun parse-eof ()
    (lambda (s)
      (declare (type buffered-stream s))
      ([d]if (eq (buffered-stream-read-char s eof)
                 eof)
          (values nil s t)
        (progn
          (buffered-stream-unread s it)
          (values nil s nil)))))

  (defun parse-anything ()
    (lambda (s)
      (declare (type buffered-stream s))
      ([d]if (eq (buffered-stream-read-char s eof)
                 eof)
          (values nil s nil)
        (values it s t))))

  (defun parse-unicode-property (property)
    (lambda (s)
      (declare (type buffered-stream s))
      ([d]if (eq (buffered-stream-read-char s eof)
                 eof)
          (values nil s nil)
        (if (has-property it property)
            (values it s t)
            (progn
              (buffered-stream-unread s it)
              (values nil s nil)))))))

(defun parse-constant (string)
  (declare (type string string))
  (lambda (s)
    (declare (type buffered-stream s))
    ([d]if (string= (buffered-stream-read-to s (length string))
                    string)
        (values nil s t)
      (buffered-stream-unread s it)
      (values nil s nil))))

(defun parse-from-list (list)
  (declare (type list list))
  (if list
      (// (parse-constant (first list))
          (parse-from-list (rest list)))
      (fail nil)))

(defun parse-from-set (set)
  (declare (type set set))
  (parse-from-list (sort (convert 'list set)
                         #'length>)))

(defun parse-from-map (map)
  (declare (type map map))
  (<$> (lambda (result)
         (@ map result))
       (parse-from-set (domain map))))

(defun parse-to (parser)
  (?? parser
      (succeed "")
      (>>!
        first (parse-anything)
        rest (parse-to parser)
        (succeed (concatenate 'string
                              first rest)))))

(defun parse-whitespace ()
  (many (// (parse-unicode-property "Whitespace")
            (parse-unicode-property "Control"))
        nil (constantly nil)))

(defun parse-identifier (&optional (reserved (empty-set)))
  (some (>>!
          parsed (parse-anything)
          (bind ((parsed-char (elt parsed 0)))
            (if (or (@ reserved parsed-char)
                    (has-property parsed-char "Number")
                    (has-property parsed-char "Whitespace")
                    (has-property parsed-char "Control"))
                (fail nil)
                (succeed parsed))))))

(defun parse-number ()
  (some (parse-unicode-property "Number")))

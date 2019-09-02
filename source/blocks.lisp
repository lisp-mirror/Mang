(in-package #:mang)

(defun parse-section-header (name)
  (>> (parse-whitespace)
      (parse-constant "#")
      (parse-whitespace-no-newline)
      (parse-constant name)
      (parse-expression-end)))

(defun parse-section (name parser)
  (declare (type string name)
           (type function parser))
  (>> (parse-section-header name)
      parser))

(defun parse-wrapped (before parser after)
  (declare (type string before after)
           (type function parser))
  (>>!
    _ (>> (parse-constant before)
          (parse-whitespace))
    result parser
    _ (>> (parse-whitespace)
          (parse-constant after))
    (succeed result)))

(defun parse-lines (parser &optional (d '())
                             (f #'cons))
  (many (>>!
          _ (parse-whitespace)
          result parser
          _ (parse-expression-end)
          (succeed result))
        d f))

(defun parse-separated (parser separator &optional (d '())
                                           (f #'cons))
  (declare (type function parser f))
  (>>!
    first parser
    rest (many (>> (parse-whitespace)
                   (parse-constant separator)
                   (parse-whitespace)
                   parser)
               d f)
    (succeed (funcall f first rest))))

(defun parse-separated-no-newline (parser separator &optional (d '())
                                                      (f #'cons))
  (declare (type function parser f))
  (>>!
    first parser
    rest (many (>> (parse-whitespace-no-newline)
                   (parse-constant separator)
                   (parse-whitespace-no-newline)
                   parser)
               d f)
    (succeed (funcall f first rest))))

(defun load-by-parser (parser file)
  (with-open-file (stream file)
    (parser-call (>>!
                   result parser
                   _ (>> (parse-whitespace)
                         (parse-eof))
                   (succeed result))
                 stream)))

(defun load-by-parser* (parser file &rest files)
  (labels ((_rec (stream files)
             (if files
                 (with-open-file (filestream (first files))
                   (_rec (make-concatenated-stream stream filestream)
                         (rest files)))
                 (parser-call (>>!
                                result parser
                                _ (>> (parse-whitespace)
                                      (parse-eof))
                                (succeed result))
                              stream))))
    (with-open-file (stream file)
      (_rec stream files))))

(defmacro parser-loop (stream (&rest bindings)
                       &body finally)
  (bind ((g!outer (gensym "outer"))
         (g!inner (gensym "inner"))
         (g!stream (gensym "stream"))
         (g!arg (gensym "arg")))
    `(block ,g!outer
       (bind (,@(mapcar (lambda (binding)
                          (bind (((var _ &optional (initial nil initial?))
                                  binding))
                            (if (consp var)
                                `(,var ,(if initial?
                                            initial
                                            (make-sequence 'list
                                                           (length var))))
                                `(,var ,initial))))
                        bindings)
                (,g!stream ,stream))
         (block ,g!inner
           (loop (parser-call
                  (<~>
                   (// (<$> (>> (parse-whitespace)
                                (parse-eof))
                            (lambda (,g!arg)
                              (declare (ignore ,g!arg))
                              (return-from ,g!inner
                                nil t)))
                       ,@(mapcar
                          (lambda (binding)
                            (bind (((var parser &optional _)
                                    binding))
                              `(<$> ,parser
                                    (lambda (,g!arg)
                                      ,(if (consp var)
                                           (bind
                                               ((nvar
                                                 (mapcar (lambda (sym)
                                                           (gensym (format nil
                                                                           "~A"
                                                                           sym)))
                                                         var)))
                                             `(bind ((,nvar ,g!arg))
                                                ,@(mapcar (lambda (var nvar)
                                                            `(setf ,var
                                                                   ,nvar))
                                                          var nvar)))
                                           `(setf ,var ,g!arg))))))
                          bindings))
                   (lambda (error)
                     (return-from ,g!outer
                       error)))
                  ,g!stream))
           ,@finally)))))

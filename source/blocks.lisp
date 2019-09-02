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

(in-package #:mang)

(defun parse-section (name parser)
  (declare (type string name)
           (type function parser))
  (>> (parse-whitespace)
      (parse-constant "##")
      (parse-whitespace-no-newline)
      (parse-constant name)
      (parse-expression-end)
      parser))

(defun parse-subsection (name parser)
  (declare (type string name)
           (type function parser))
  (>> (parse-whitespace)
      (parse-constant "#")
      (parse-whitespace-no-newline)
      (parse-constant name)
      (parse-expression-end)
      parser))

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
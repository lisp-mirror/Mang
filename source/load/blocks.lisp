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
  (declare (type function parser separator f))
  (>>!
    first parser
    rest (many (>> (parse-whitespace)
                   separator
                   (parse-whitespace)
                   parser)
               d f)
    (succeed (funcall f first rest))))

(defun parse-separated-no-newline (parser separator &optional (d '())
                                                      (f #'cons))
  (declare (type function parser separator f))
  (>>!
    first parser
    rest (many (>> (parse-whitespace-no-newline)
                   separator
                   (parse-whitespace-no-newline)
                   parser)
               d f)
    (succeed (funcall f first rest))))

(defun parse-definition (parser)
  (>>!
    name (parse-identifier *mang-reserved-symbols*)
    _ (>> (parse-whitespace-no-newline)
          (parse-constant ":=")
          (parse-whitespace-no-newline))
    definition parser
    (succeed `(,name ,definition))))

(defun parse-definitions (parser &optional (d (empty-set))
                                   (f (lambda (definition definitions)
                                        (with definitions definition))))
  (declare (type function parser f))
  (parse-lines (parse-definition parser)
               d f))

(defun load-by-parser (parser file)
  (with-open-file (stream file)
    (parser-call parser stream)))

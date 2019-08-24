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

(defun parse-repeated-lines (parser &optional (d "")
                                      (f (lambda (a b)
                                           (concatenate 'string
                                                        a b))))
  (many (>>!
          _ (parse-whitespace)
          result parser
          _ (parse-expression-end))
        d f))

(defun parse-separated (parser separator &optional (d "")
                                           (f (lambda (a b)
                                                (concatenate 'string
                                                             a b))))
  (declare (type function parser separator f))
  (>>!
    first parser
    rest (many (>> (parse-whitespace)
                   separator
                   (parse-whitespace)
                   parser)
               d f)
    (succeed (funcall f first rest))))

(defun parse-separated-no-newline (parser separator &optional (d "")
                                                      (f (lambda (a b)
                                                           (concatenate 'string
                                                                        a b))))
  (declare (type function parser separator f))
  (>>!
    first parser
    rest (many (>> (parse-whitespace-no-newline)
                   separator
                   (parse-whitespace-no-newline)
                   parser)
               d f)
    (succeed (funcall f first rest))))

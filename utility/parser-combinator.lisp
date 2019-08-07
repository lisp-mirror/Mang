(in-package #:mang)

(defun succeed (x)
  (lambda (s)
    (values s x t)))

(defun fail (e)
  (lambda (s)
    (values s e nil)))

(defun <$~> (fs fe a)
  (lambda (s)
    (bind (((:values ns r success?)
            (funcall a s)))
      (values ns (if success?
                     (funcall fs r)
                     (funcall fe r))
              success?))))

(defun <$> (fs a)
  (<$~> fs #'identity a))

(defun <~> (fe a)
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
  (lambda (s)
    (bind (((:values ns r success?)
            (funcall xa s)))
      (if success?
          (values ns r t)
          (funcall (funcall fa r)
                   s)))))

(defun // (parser &rest parsers)
  (declare (type function parser))
  (if parsers
      (//= parser (constantly (apply #'//
                                     parsers)))
      parser))

(defmacro //* (left arg right)
  `(//= ,left (lambda (,arg)
                ,right)))

(defmacro //! (&body bindings)
  (if (rest bindings)
      (bind (((var parser &rest bindings)
              bindings))
        (if (string= (symbol-name var)
                     "_")
            `(// ,parser (//! ,@bindings))
            `(//* ,parser ,var (//! ,@bindings))))
      (first bindings)))

(defun >>= (xa fa)
  (lambda (s)
    (bind (((:values ns r success?)
            (funcall xa s)))
      (if success?
          (funcall (funcall fa r)
                   ns)
          (values ns r nil)))))

(defun >> (parser &rest parsers)
  (declare (type function parser))
  (if parsers
      (>>= parser (constantly (apply #'>>
                                     parsers)))
      parser))

(defmacro >>* (left arg right)
  `(>>= ,left (lambda (,arg)
                ,right)))

(defmacro >>! (&body bindings)
  (if (rest bindings)
      (bind (((var parser &rest bindings)
              bindings))
        (if (string= (symbol-name var)
                     "_")
            `(>> ,parser (>>! ,@bindings))
            `(>>* ,parser ,var (>>! ,@bindings))))
      (first bindings)))

(defun <?> (p &optional (d #'identity))
  (//!
    x p
    (succeed (funcall d x))))

(defun <? (p &optional d)
  (<?> p (constantly d)))

(defun some (p &optional (d "")
                 (f (lambda (a b)
                      (concatenate 'string
                                   a b))))
  (>>!
    x p
    xs (// (some p d f)
           (succeed d))
    (succeed (funcall f x xs))))

(defun many (p &optional (d "")
                 (f (lambda (a b)
                      (concatenate 'string
                                   a b))))
  (// (some p d f)
      (succeed d)))

(defun parse-eof ()
  (lambda (s)
    (declare (type string s))
    (if (string= s "")
        (values "" nil t)
        (values "" nil nil))))

(defun parse-constant (string)
  (declare (type string string))
  (lambda (s)
    (declare (type string s))
    (if (prefix? string s)
        (values (subseq s (length string))
                nil t)
        (values string nil nil))))

(defun parse-unicode-property (property)
  (lambda (s)
    (declare (type string s))
    (if (has-property (elt s 0)
                      property)
        (values (subseq s 1)
                (subseq s 0 1)
                t)
      (values s nil nil))))

(defun parse-prefix-set (prefix-set)
  (declare (type set prefix-set))
  (lambda (s)
    (declare (type string s))
    ([a]if (find-if (lambda (prefix)
                      (prefix? prefix s))
                    prefix-set)
        (values (subseq s (length it))
                it t)
      (values s nil nil))))

(defun parse-whitespace ()
  (many (parse-unicode-property "Whitespace")
        nil (constantly nil)))

(defun parse-identifier ()
  (some (parse-unicode-property "Alphabetic")))

(defun parse-number ()
  (some (parse-unicode-property "Number")))

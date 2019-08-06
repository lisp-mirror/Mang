(in-package #:mang)

(defun succeed (x)
  (lambda (s)
    (values s x t)))

(defun fail (e)
  (lambda (s)
    (values s e nil)))

(defun <$/ (r e a)
  (lambda (s)
    (bind (((:values ns _ success?)
            (funcall a s)))
      (if success?
          (values ns r t)
          (values s e nil)))))

(defun <$ (r a)
  (lambda (s)
    (bind (((:values ns nr success?)
            (funcall a s)))
      (if success?
          (values ns r t)
          (values s nr nil)))))

(defun </ (e a)
  (lambda (s)
    (bind (((:values ns r success?)
            (funcall a s)))
      (if success?
          (values ns r t)
          (values s e nil)))))

(defun <$/> (fs fe a)
  (lambda (s)
    (bind (((:values ns r success?)
            (funcall a s)))
      (values ns (if success?
                     (funcall fs r)
                     (funcall fe r))
              success?))))

(defun <$> (fs a)
  (<$/> fs #'identity a))

(defun </> (fe a)
  (<$/> #'identity fe a))

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

(defun ?? (p d)
  (// p (succeed d)))

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

(defun parse-eof (&key on-success on-error)
  (lambda (s)
    (declare (type string s))
    (if (string= s "")
        (values "" on-success t)
        (values "" on-error nil))))

(defun parse-constant (string &key on-success on-error)
  (declare (type string string))
  (lambda (s)
    (declare (type string s))
    (if (prefix? string s)
        (values (subseq s (length string))
                on-success t)
        (values string on-error nil))))

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

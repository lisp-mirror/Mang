(in-package #:mang)

;;; Parser err ret = String -> (String, Either err ret)
;;; String -> (String, err/ret, Bool)

(defun succeed (x)
  (lambda (s)
    (values x s t)))

(defun fail (e)
  (lambda (s)
    (values e s nil)))

(defun ^$ (p x)
  (lambda (s)
    (bind (((:values r _ success?)
            (funcall p x)))
      (values r s success?))))

(defun <$~> (fs fe a)
  (declare (type function fs fe))
  (lambda (s)
    (bind (((:values r ns success?)
            (funcall a s)))
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
            (funcall xa s)))
      (if success?
          (values r ns t)
          (funcall (funcall fa r)
                   s)))))

(defun // (parser &rest parsers)
  (declare (type function parser))
  (if parsers
      (//= parser (constantly (apply #'//
                                     parsers)))
      parser))

(defmacro //* (left var right)
  (let ((g!arg (gensym "arg")))
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
            (funcall xa s)))
      (if success?
          (funcall (funcall fa r)
                   ns)
          (values r ns nil)))))

(defun >> (parser &rest parsers)
  (declare (type function parser))
  (if parsers
      (>>= parser (constantly (apply #'>>
                                     parsers)))
      parser))

(defmacro >>* (left var right)
  (let ((g!arg (gensym "arg")))
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
            (funcall ptest s)))
      (funcall (funcall (if success?
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
            (funcall p s)))
      (if success?
          (values r s t)
          (values r ns nil)))))

(defun parse-eof ()
  (lambda (s)
    (declare (type string s))
    (if (string= s "")
        (values nil "" t)
        (values nil "" nil))))

(defun parse-anything ()
  (lambda (s)
    (declare (type string s))
    (if (length> s 0)
        (values (subseq s 0 1)
                (subseq s 1)
                t)
        (values nil s nil))))

(defun parse-to (parser)
  (?? parser
      (succeed "")
      (>>!
        first (parse-anything)
        rest (parse-to parser)
        (succeed (concatenate 'string
                              first rest)))))

(defun parse-constant (string)
  (declare (type string string))
  (lambda (s)
    (declare (type string s))
    (if (prefix? string s)
        (values nil (subseq s (length string))
                t)
        (values nil s nil))))

(defun parse-unicode-property (property)
  (lambda (s)
    (declare (type string s))
    (if (> (length s)
           0)
        (if (has-property (elt s 0)
                          property)
            (values (subseq s 0 1)
                    (subseq s 1)
                    t)
            (values nil s nil))
        (values "" s nil))))

(defun parse-from-set (set)
  (declare (type set set))
  (lambda (s)
    (declare (type string s))
    ([a]if (find-if (lambda (prefix)
                      (prefix? prefix s))
                    set)
        (values it (subseq s (length it))
                t)
      (values nil s nil))))

(defun parse-whitespace ()
  (many (parse-unicode-property "Whitespace")
        nil (constantly nil)))

(defun parse-identifier ()
  (some (parse-unicode-property "Alphabetic")))

(defun parse-number ()
  (some (parse-unicode-property "Number")))

(in-package #:mang)

(defgeneric parser-call (parser to-parse)
  (:method ((parser function)
            (to-parse bus))
    (funcall parser to-parse))
  (:method ((parser function)
            (to-parse string))
    (with-input-from-string (stream to-parse)
      (funcall parser (bus stream))))
  (:method ((parser function)
            (to-parse stream))
    (funcall parser (bus to-parse))))

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

(defun <$~> (p fs fe)
  (declare (type function p fs fe))
  (lambda (s)
    (bind (((:values r ns success?)
            (parser-call p s)))
      (values (if success?
                  (funcall fs r)
                  (funcall fe r))
              ns success?))))

(defun <$> (p fs)
  (declare (type function p fs))
  (<$~> p fs #'identity))

(defun <~> (p fe)
  (declare (type function p fe))
  (<$~> p #'identity fe))

(defun <$~ (p s e)
  (<$~> p (constantly s)
        (constantly e)))

(defun <$ (p s)
  (<$> p (constantly s)))

(defun <~ (p e)
  (<~> p (constantly e)))

(defun //= (p pg)
  (declare (type function p pg))
  (lambda (s)
    (bind (((:values r ns success?)
            (parser-call p s)))
      (if success?
          (values r ns t)
          (parser-call (funcall pg r)
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
  (assert bindings)
  (bind ((g!bus (gensym "bus"))
         (g!result (gensym "result"))
         (g!new-bus (gensym "new-bus"))
         (g!success? (gensym "success?"))
         (g!arg (gensym "arg")))
    (if (rest bindings)
        (bind (((var parser &rest bindings)
                bindings))
          (assert (or (symbolp var)
                      (and (consp var)
                           (every #'symbolp
                                  var))))
          `(lambda (,g!bus)
             (bind (((:values ,g!result ,g!new-bus ,g!success?)
                     (funcall (lambda ()
                                (parser-call ,parser ,g!bus)))))
               (if ,g!success?
                   (values ,g!result ,g!new-bus t)
                   (parser-call ,(if (and (symbolp var)
                                          (string= (symbol-name var)
                                                   "_"))
                                     `(//! ,@bindings)
                                     `(funcall (lambda (,g!arg)
                                                 (bind ((,var ,g!arg))
                                                   (//! ,@bindings)))
                                               ,g!result))
                                ,g!bus)))))
        (first bindings))))

(defun >>= (p pg)
  (declare (type function p pg))
  (lambda (s)
    (bind (((:values r ns success?)
            (parser-call p s)))
      (if success?
          (parser-call (funcall pg r)
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
  (assert bindings)
  (bind ((g!bus (gensym "bus"))
         (g!result (gensym "result"))
         (g!new-bus (gensym "new-bus"))
         (g!success? (gensym "success?"))
         (g!arg (gensym "arg")))
    (if (rest bindings)
        (bind (((var parser &rest bindings)
                bindings))
          (assert (or (symbolp var)
                      (and (consp var)
                           (every #'symbolp
                                  var))))
          `(lambda (,g!bus)
             (bind (((:values ,g!result ,g!new-bus ,g!success?)
                     (funcall (lambda ()
                                (parser-call ,parser ,g!bus)))))
               (if ,g!success?
                   (parser-call ,(if (and (symbolp var)
                                          (string= (symbol-name var)
                                                   "_"))
                                     `(>>! ,@bindings)
                                     `(funcall (lambda (,g!arg)
                                                 (bind ((,var ,g!arg))
                                                   (>>! ,@bindings)))
                                               ,g!result))
                                ,g!new-bus)
                   (values ,g!result ,g!bus nil)))))
        (first bindings))))

(defun ??== (ptest pgthen pgelse)
  (declare (type function ptest pgthen pgelse))
  (lambda (s)
    (bind (((:values r ns success?)
            (parser-call ptest s)))
      (parser-call (funcall (if success?
                                pgthen
                                pgelse)
                            r)
                   ns))))

(defun ??= (ptest pgthen pelse)
  (declare (type function ptest pgthen pelse))
  (??== ptest pgthen (constantly pelse)))

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
    (bind (((:values r ns success?)
            (parser-call p s)))
      (if success?
          (values r s t)
          (values r ns nil)))))

;;;; basic parsers
(defun parse-eof ()
  (lambda (s)
    (declare (type bus s))
    (if (nth-value 1 (bus-read s))
        (values nil s nil)
        (values nil s t))))

(defun parse-anything ()
  (lambda (s)
    (declare (type bus s))
    ([av]if (bus-read s)
        (values it (bus-consume s)
                t)
      (values `(:unexpected-eof)
              s nil))))

(defun parse-unicode-property (property)
  (lambda (s)
    (declare (type bus s))
    ([av]if (bus-read s)
        (if (has-property (elt it 0)
                          property)
            (values it (bus-consume s)
                    t)
            (values `(:wrong-properties ,it ,property)
                    s nil))
      (values `(:unexpected-eof)
              s nil))))

(defun parse-constant (string)
  (declare (type string string))
  (lambda (s)
    (declare (type bus s))
    (bind ((length (length string)))
      ([av]if (bus-read s length)
          (if (string= it string)
              (values nil (bus-consume s length)
                      t)
              (values `(:constant-not-found ,string)
                      s nil))
        (values `(:constant-not-found ,string)
                s nil)))))

(defun parse-from-list (list)
  (declare (type list list))
  (if list
      (bind (((curr &rest rest)
              list))
        (// (<$ (parse-constant curr)
                curr)
            (parse-from-list rest)))
      (fail `(:elements-not-found ,@list))))

(defun parse-from-set (set)
  (declare (type set set))
  (parse-from-list (sort (convert 'list set)
                         #'length>)))

(defun parse-from-map (map)
  (declare (type map map))
  (<$> (parse-from-set (domain map))
       (lambda (result)
         (@ map result))))

(defun parse-to (parser)
  (?? parser
      (succeed "")
      (>>!
        first (parse-anything)
        rest (parse-to parser)
        (succeed (concatenate 'string
                              first rest)))))

(defun parse-newline ()
  (parse-constant (string #\Newline)))

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

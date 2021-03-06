(in-package #:mang)

(defun less* (collection items)
  (reduce (lambda (collection item)
            (less collection item))
          items
          :initial-value collection))

(defmethod tree-map (f (tree set))
  (image (lambda (el)
           (tree-map f el))
         tree))

(defmethod expand (fn (collection set))
  (reduce #'union
          (image fn collection)
          :initial-value (empty-set)))

(defmethod expand (fn (collection map))
  (reduce #'union
          (domain (image fn collection))
          :initial-value (empty-set)))

(defmethod convert ((to-type (eql 'map))
                    (collection set)
                    &key (key-fn #'car)
                      (value-fn #'cdr)
                      from-type pair-fn
                      &allow-other-keys)
  (declare (ignore from-type pair-fn))
  (convert 'map (convert 'list collection)
           :key-fn key-fn
           :value-fn value-fn))

(defmethod all-fitting ((collection map)
                        obj)
  (filter (lambda (predicate result)
            (declare (ignore result))
            (@ predicate obj))
          collection))

(defmethod all-fitting ((collection set)
                        obj)
  (filter (lambda (predicate)
            (@ predicate obj))
          collection))

(defun origin (obj map &key (key #'identity))
  (domain (filter (lambda (k v)
                    (declare (ignore k))
                    (equal? obj (funcall key v)))
                  map)))

(defun map-value-order (obj map)
  (size (origin obj map)))

(defun filtering-combine (obj map add mult initial-value)
  (bind ((result initial-value))
    (do-map (k v (all-fitting map obj)
               result)
      (declare (ignore k))
      (setf result (@ add (@ mult (map-value-order v map)
                                  v)
                          result)))))

(defmethod difference ((collection1 set)
                       (collection2 set))
  (set-difference-2 collection1 collection2))

(defmethod difference ((collection1 map)
                       (collection2 map))
  (fset:map-difference-2 collection1 collection2))

(defmethod lookup ((set fset::complement-set)
                   key)
  (not (lookup (complement set)
               key)))

(defmethod intersection ((_ null)
                         collection &key test test-not)
  (declare (ignore _ collection test test-not))
  '())

(defmethod intersection ((list cons)
                         (set set)
                         &key test test-not)
  (declare (ignore test test-not))
  (convert 'list
           (intersection (convert 'set list)
                         set)))

(defmethod map<-set ((f function)
                     (set set)
                     &optional default)
  (with-default
      (gmap :map (lambda (value)
                   (values value (funcall f value)))
            (:set set))
    default))

(defmethod cross-product ((f function)
                          (set1 set)
                          (set2 set))
  (reduce #'union
          (image (lambda (item1)
                   (image (lambda (item2)
                            (funcall f item1 item2))
                          set2))
                 set1)
          :initial-value (empty-set)))

(defmethod deep-union ((collection1 t)
                       (collection2 t)
                       &optional
                         (val-fn (lambda (a b)
                                   (declare (ignore b))
                                   a)))
  (declare (type function val-fn))
  (funcall val-fn collection1 collection2))

(defmethod deep-union ((collection1 set)
                       (collection2 set)
                       &optional
                         (val-fn (lambda (a b)
                                   (declare (ignore b))
                                   a)))
  (declare (type function val-fn)
           (ignore val-fn))
  (union collection1 collection2))

(defmethod deep-union ((collection1 map)
                       (collection2 map)
                       &optional
                         (val-fn (lambda (a b)
                                   (declare (ignore b))
                                   a)))
  (declare (type function val-fn))
  (map-union collection1 collection2
             (lambda (a b)
               (deep-union a b val-fn))))

(defun reduce-nd (fn collection initial-value
                   &key from-end)
  (reduce (lambda (as b)
            (expand (lambda (a)
                      (funcall fn a b))
                    as))
          collection
          :from-end from-end
          :initial-value (set initial-value)))

(defmethod filter ((fn set)
                   (collection map))
  (filter (lambda (k v)
            (declare (ignore v))
            (@ fn k))
          collection))

(defmacro map* (&rest args)
  (bind ((resulting-args '())
         (default nil))
    (do* ((args args (rest args))
          (default? nil (eq curr :default))
          (curr (first args)
                (first args)))
         ((not args))
      (if (and (not default?)
               (consp curr))
          (bind (((mode &rest local-args)
                  curr))
            (bind ((g!map (gensym "map")))
              (case mode
                ('&
                 (bind (((key form)
                         local-args))
                   (setf resulting-args
                         `(,default
                           :default
                           ($ (bind ((,g!map
                                      (map ,@(nreverse resulting-args)))
                                     (,(or (first (cdr-or-nil key))
                                           'it)
                                      (@ ,g!map ,(car-or-x key))))
                                (map ($ ,g!map)
                                     (,(car-or-x key)
                                      ,form))))))))
                (t
                 (push curr resulting-args)))))
          (push curr resulting-args))
      (when default?
        (setf default? nil
              default curr)))
    `(map ,@(nreverse resulting-args))))

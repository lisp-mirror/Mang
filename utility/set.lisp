(in-package #:mang)

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

(defun origin (obj map)
  (domain (filter (lambda (k v)
                    (declare (ignore k))
                    (equal? obj v))
                  map))
  #+nil
  (expand (lambda (k v)
            (if (equal? v obj)
                (set k)
                (empty-set)))
          map))

(defun map-value-order (obj map)
  (size (origin obj map)))

(defun filtering-combine (obj map add mult initial-value)
  (let ((result initial-value))
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

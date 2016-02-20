(in-package #:mang)

(defadt [follow-db]
  (<follow-db> [match] [distribution] [follow-db])
  <no-follow-db>)

(defmatch insert ([follow-db] [follow] integer)
    [follow-db]
  ((fdb _ 0)
   fdb)
  ((fdb (<follow> <nomatch> _)
        _)
   fdb)
  ((fdb (<follow> _ <norope>)
        _)
   fdb)
  ((<no-follow-db> (<follow> m r)
                   w)
   (<follow-db> m (<sure> r w)
                <no-follow-db>))
  (((<follow-db> mf d fdb)
    (<follow> m r)
    w)
   (ecase (compare mf m)
     (:less
      (<follow-db> mf d (insert fdb (<follow> m r)
                                w)))
     (:equal
      (<follow-db> mf (insert d r w)
                   fdb))
     (:greater
      (<follow-db> m (<sure> r w)
                   (<follow-db> mf d fdb))))))

(defmatch extract ([follow-db] [rope])
    [distribution]
  ((<no-follow-db> _)
   (<nodist> (<rglyph> <end-glyph>)))
  (((<follow-db> m d fdb)
    r)
   (let ((rs (size r)))
     (if (match? m (sub-rope r (- rs (size m))
                             rs))
         (merge d (extract fdb r))
         (extract fdb r)))))

(defmethod merge ((x null)
                  y)
  (declare (ignore x))
  y)

(defmethod merge (x (y null))
  (declare (ignore y))
  x)

(defmethod extract ((container null)
                    key)
  (declare (ignore container key))
  nil)

(defmatch extract (cons [rope])
    [distribution]
  ((l r)
   (merge (extract (car l)
                   r)
          (extract (cdr l)
                   r))))

(defmatch learn ([follow-db] [pattern] [rope] integer integer)
    [follow-db]
  ((fdb p r w l)
   (reduce (lambda (fdb f)
             (insert fdb f w))
           (chop p r l)
           :initial-value fdb)))

(defmethod learn ((store null)
                  (pattern [pattern])
                  (rope [rope])
                  (weight integer)
                  (f-length integer))
  (declare (ignore store pattern rope weight f-length))
  nil)

(defmethod learn ((store cons)
                  (pattern [pattern])
                  (rope [rope])
                  (weight integer)
                  (f-length integer))
  (mapcar (lambda (fdb)
            (learn fdb pattern rope weight f-length))
          store))

(defmethod gen-next ((store [follow-db])
                     (rope [rope]))
  (random-from-dist (extract store rope)))

(defmethod complete ((store [follow-db])
                     (rope [rope])
                     &optional (depth 0))
  (cond
    ((> depth 30)
     (append-rope rope <end-glyph>))
    ((equal? (rope-elt rope (1- (size rope)))
             <end-glyph>)
     rope)
    (t
     (let ((next (gen-next store rope)))
       (complete store (append-rope rope next)
                 (1+ depth))))))

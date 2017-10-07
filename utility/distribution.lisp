(in-package #:mang)

(defadt [distribution]
  (<distribution> real             ; weight of left subtree
                  [distribution]   ; left subtree
                  real             ; weight of node
                  t                ; value of node
                  [distribution]   ; right subtree
                  real)            ; weight of right subtree
  <nodist>)

(defmethod print-object ((object <distribution>)
                         stream)
  (if *print-readably*
      (match object
          t
        ((<distribution> _ l w v r _)
         (format stream "~:@<(<DISTRIBUTION>~;~{ ~S~}~;)~:@>"
                 (list l w v r))))
      (match object
          t
        ((<distribution> _ <nodist> w v <nodist> _)
         (format stream "~:@<(#<DISTRIBUTION~;~{ ~S~}~;>)~:@>"
                 (list w v)))
        ((<distribution> _ <nodist> w v r _)
         (format stream "~:@<(#<DISTRIBUTION~;~{ ~S~}~;>)~:@>"
                 (list w v r)))
        ((<distribution> _ l w v <nodist> _)
         (format stream "~:@<(#<DISTRIBUTION~;~{ ~S~}~;>)~:@>"
                 (list l w v)))
        ((<distribution> _ l w v r _)
         (format stream "~:@<(<DISTRIBUTION>~;~{ ~S~}~;)~:@>"
                 (list l w v r))))))

(defmatch depth ([distribution])
    integer
  ((<distribution> _ l _ _ r _)
   (1+ (max (depth l)
            (depth r))))
  (<nodist> 0))

(defmatch weight ([distribution])
    real
  ((<distribution> lw _ w _ _ rw)
   (+ lw rw w))
  (<nodist> 0))

(defmatch expected-cost ([distribution])
    real
  ((<distribution> lw l w _ r rw)
   (/ (+ (* (depth l)
            lw)
         (* (depth r)
            rw))
      (+ lw rw w)))
  (<nodist> 0))

(defmatch distribution-with ([distribution] real t)
    [distribution]
  ((<nodist> w v)
   (<distribution> 0 <nodist> w v <nodist> 0))
  (((<distribution> lw l w v r rw)
    nw nv)
   (ecase (compare v nv)
     (:equal
      (<distribution> lw l (+ w nw)
                      v r rw))
     (:less
      (<distribution> lw l w v (distribution-with r nw nv)
                      (+ rw nw)))
     (:greater
      (<distribution> (+ lw nw)
                      (distribution-with l nw nv)
                      w v r rw)))))

(defmethod with ((collection [distribution])
                 value1 &optional (value2 1))
  (declare (type real value2))
  (distribution-with collection value2 value1))

(defmatch join-distributions ([distribution] [distribution])
    [distribution]
  ((<nodist> d)
   d)
  ((d <nodist>)
   d)
  (((<distribution> lw1 l1 w1 v1 r1 rw1)
    (<distribution> lw2 l2 w2 v2 r2 rw2))
   (if (< (expected-cost (<distribution> lw1 l1 w1 v1 r1 rw1))
          (expected-cost (<distribution> lw2 l2 w2 v2 r2 rw2)))
       (join-distributions (join-distributions (with (<distribution> lw2 l2 w2
                                                                     v2 r2 rw2)
                                                     v1 w1)
                                               l1)
                           r1)
       (join-distributions (join-distributions (with (<distribution> lw1 l1 w1
                                                                     v1 r1 rw1)
                                                     v2 w2)
                                               l2)
                           r2))))

(defmethod union ((d1 [distribution])
                  (d2 [distribution])
                  &key test test-not)
  (declare (ignore test test-not))
  (join-distributions d1 d2))

(defmatch distribution-less ([distribution] t)
    [distribution]
  ((<nodist> _)
   <nodist>)
  (((<distribution> lw l w v r rw)
    dv)
   (case (compare dv v)
     (:equal (union l r))
     (:less (let ((l (distribution-less l dv)))
              (<distribution> (weight l)
                              l w v r rw)))
     (:greater (let ((r (distribution-less r dv)))
                 (<distribution> lw l w v r (weight r)))))))

(defmethod less ((collection [distribution])
                 value1 &optional value2)
  (declare (ignore value2))
  (distribution-less collection value1))

(defmatch distribution-keep ([distribution] t)
    [distribution]
  ((<nodist> _)
   <nodist>)
  (((<distribution> _ l w v r _)
    predicate)
   (let ((l (distribution-keep l predicate))
         (r (distribution-keep r predicate)))
     (if (@ predicate v)
         (<distribution> (weight l)
                         l w v r (weight r))
         (union l r)))))

(defmethod keep (predicate collection)
  (filter predicate collection))

(defmethod keep (predicate (collection [distribution]))
  (distribution-keep collection predicate))

(defmatch mult ([distribution] real)
    [distribution]
  ((<nodist> _)
   <nodist>)
  (((<distribution> lw l w v r rw)
    x)
   (<distribution> (* lw x)
                   (mult l x)
                   (* w x)
                   v (mult r x)
                   (* rw x))))

(defmethod mult ((x real)
                 (dist [distribution]))
  (mult dist x))

(defmatch normalize ([distribution])
    [distribution]
  (<nodist> <nodist>)
  (d (mult d (/ (weight d)))))

(defmethod difference ((collection1 [distribution])
                       (collection2 set))
  (keep (complement collection2)
        collection1))

(defmatch domain ([distribution])
    set
  (<nodist> (empty-set))
  ((<distribution> _ l _ v r _)
   (with (union (domain l)
                (domain r))
         v)))

(defmatch diminish ([distribution] [distribution])
    [distribution]
  ((<nodist> _)
   <nodist>)
  ((d <nodist>)
   d)
  ((d1 d2)
   (let ((d2 (normalize d2)))
     (labels ((_rec (d1 d2)
                (match (d1 d2)
                    [distribution]
                  ((<nodist> _)
                   <nodist>)
                  ((d <nodist>)
                   d)
                  (((<distribution> lw1 l1 w1 v1 r1 rw1)
                    (<distribution> lw2 l2 w2 v2 r2 rw2))
                   (ecase (compare v1 v2)
                     (:equal
                      (let ((l (_rec l1 l2))
                            (r (_rec r1 r2)))
                        (<distribution> (weight l)
                                        l (* w1 (- 1 w2))
                                        v1 r (weight r))))
                     (:less (union
                             (_rec (<distribution> lw1 l1 w1 v1 <nodist> 0)
                                   l2)
                             (_rec r1 (<distribution> lw2 l2 w2 v2 r2 rw2))))
                     (:greater (union
                                (_rec l1 (<distribution> lw2 l2 w2 v2 r2 rw2))
                                (_rec (<distribution> 0 <nodist> w1 v1 r1 rw1)
                                      r2))))))))
       (_rec d1 d2)))))

(defun distribution (&rest args)
  (let ((dist <nodist>))
    (loop :for (weight event) :on args :by #'cddr
       :do (setf dist
                 (with dist event weight))
       :finally (return dist))))

(defmatch list<-distribution ([distribution])
    (or cons null)
  (<nodist> '())
  ((<distribution> _ l w v r _)
   (list* w v (append (list<-distribution l)
                      (list<-distribution r)))))

(defmethod uniform-distribution ((elements null)
                                 &optional weight)
  (declare (ignore elements weight))
  <nodist>)

(defmethod uniform-distribution ((elements cons)
                                 &optional (weight 1))
  (let ((dist <nodist>))
    (loop :for element :in elements
       :do (setf dist
                 (with dist element weight))
       :finally (return dist))))

(defmethod uniform-distribution ((elements set)
                                 &optional (weight 1))
  (uniform-distribution (convert 'list elements)
                        weight))

(defmethod print-object ((object [distribution])
                         stream)
  (format stream "~:@<(DISTRIBUTION~;~{ ~S~}~;)~:@>"
          (list<-distribution object)))

(defmatch extract ([distribution] real)
    t
  (((<distribution> lw l w v r _)
    index)
   (if (< index lw)
       (extract l index)
       (let ((index (- index lw)))
         (if (< index w)
             v
             (extract r (- index w)))))))

(defgeneric extract-random (collection)
  (:method ((collection [distribution]))
    (extract collection (random (float (weight collection)
                                       1d0)))))

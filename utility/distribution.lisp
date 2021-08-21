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

(defmethod size ((collection [distribution]))
  (weight collection))

(defmatch expected-cost ([distribution])
    real
  ((<distribution> lw l w _ r rw)
   (/ (+ (* (1+ (expected-cost l))
            lw)
         (* (1+ (expected-cost r))
            rw))
      (+ lw rw w)))
  (<nodist> 0))

(defmatch distribution-with ([distribution] real t)
          [distribution]
  ((d 0 _)
   d)
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
   (bind ((l (distribution-keep l predicate))
          (r (distribution-keep r predicate)))
     (if (@ predicate v)
         (<distribution> (weight l)
                         l w v r (weight r))
         (union l r)))))

(defmethod keep (predicate collection)
  (filter predicate collection))

(defmethod keep (predicate (collection [distribution]))
  (distribution-keep collection predicate))

(defmatch image (function [distribution])
    [distribution]
  ((_ <nodist>)
   <nodist>)
  ((f (<distribution> _ l w v r _))
   (union (union (image f l)
                 (image f r))
          (distribution w (funcall f v)))))

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
  ((d1 (<distribution> _ <nodist> _ _ <nodist> _))
   (normalize d1))
  ((d1 d2)
   (bind ((d2 (normalize d2)))
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
                      (bind ((l (_rec l1 l2))
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

(defmatch distribution-frequency ([distribution] t)
          real
  ((<nodist> _)
   0)
  (((<distribution> _ l w x r _)
    item)
   (+ (distribution-frequency l item)
      (distribution-frequency r item)
      (if (equal? x item)
          w
          0))))

(defun distribution-minus-item (distribution frequency item)
  (bind ((absolute-frequency (distribution-frequency distribution item)))
    (with (less distribution item)
          item
          (- absolute-frequency frequency))))

(defmatch distribution-minus ([distribution] [distribution])
          [distribution]
  ((d <nodist>)
   d)
  ((d (<distribution> _ l w i r _))
   (distribution-minus (distribution-minus (distribution-minus-item d w i)
                                           l)
                       r)))

(defun distribution (&rest args)
  (bind ((dist <nodist>))
    (loop :for (weight event) :on args :by #'cddr
       :do ([d]setf (with dist event weight))
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
  (bind ((dist <nodist>))
    (loop :for element :in elements
       :do ([d]setf (with dist element weight))
       :finally (return dist))))

(defmethod uniform-distribution ((elements set)
                                 &optional (weight 1))
  (uniform-distribution (convert 'list elements)
                        weight))

(defmethod zipf-distribution ((elements null)
                              (exponent real)
                              (weight real))
  (declare (ignore elements weight exponent))
  <nodist>)

(defmethod zipf-distribution ((elements cons)
                              (exponent real)
                              (weight real))
  (declare (type (real (0))
                 weight exponent))
  (bind ((dist <nodist>)
         (n (size elements))
         (sum (loop :for n :from 1 :to n
                 :sum (/ (expt n exponent)))))
    (loop
      :for element :in elements
      :for k :from 1
      :do ([d]setf (with dist element (* weight
                                         (/ (* (expt k exponent)
                                               sum)))))
      :finally (return dist))))

#+nil
(defmethod yule-distribution ((elements null)
                              (a real)
                              (b real)
                              (c real))
  (declare (ignore elements a b c))
  <nodist>)

#+nil
(defmethod yule-distribution ((elements cons)
                              (a real)
                              (b real)
                              (c real))
  (declare (type (real (0))
                 a b c))
  (bind ((dist <nodist>))
    (loop :for element :in elements
       :for r :from 1
       :do ([d]setf (with dist element (* (/ a (expt r b))
                                          (expt c r))))
       :finally (return dist))))

#+nil
(defmethod beta-distribution ((elements null)
                              (a real)
                              (b real)
                              (c real))
  (declare (ignore elements a b c))
  <nodist>)

#+nil
(defmethod beta-distribution ((elements cons)
                              (a real)
                              (b real)
                              (c real))
  (declare (type (real (0))
                 a b c))
  (bind ((n (length elements))
         (dist <nodist>))
    (loop
      :for element :in elements
      :for r :from 1
      :do ([d]setf (with dist element (* c (/ (expt (+ n 1 r)
                                                    b)
                                              (expt r a)))))
      :finally (return-from beta-distribution dist))))

(defmethod print-object ((object [distribution])
                         stream)
  (if (equal? object <nodist>)
      (format stream "<NODIST>")
      (format stream "~:@<(DISTRIBUTION~;~{ ~S~}~;)~:@>"
              (apply #'append
                     (sort (loop :for (w v)
                                   :on (list<-distribution object)
                                 :by #'cddr
                                 :collect `(,w ,v))
                           #'> :key #'first)))))

(defmatch extract ([distribution] real)
    t
  (((<distribution> lw l w v r _)
    index)
   (if (< index lw)
       (extract l index)
       (bind ((index (- index lw)))
         (if (< index w)
             v
             (extract r (- index w)))))))

(defgeneric extract-random (collection)
  (:method ((collection [distribution]))
    (extract collection (random (float (weight collection)
                                       1d0)))))

(defmethod deep-union ((collection1 [distribution])
                       (collection2 [distribution])
                       &optional
                         (val-fn (lambda (a b)
                                   (declare (ignore b))
                                   a)))
  (declare (type function val-fn)
           (ignore val-fn))
  (union collection1 collection2))

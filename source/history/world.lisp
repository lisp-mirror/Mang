(in-package #:mang)

(defadt [population]
  <no-population>
  (<population> dictionary (integer (0))))

(defmatch size<- ([population])
    (integer 0)
  (<no-population> 0)
  ((<population> _ size)
   size))

(defmatch dictionary<- ([population])
    dictionary
  (<no-population>
   (dictionary))
  ((<population> dictionary _)
   dictionary))

(defmatch apply-sound-change ([population] sound-change)
    [population]
  ((<no-population> _)
   <no-population>)
  (((<population> dictionary size)
    sound-change)
   (<population (apply-sound-change dictionary sound-change)
                size)))

(defclass world ()
  ((%graph :type standard-graph
           :reader graph<-
           :initarg :graph
           :initform (make-standard-graph '(:weighted :undirected)))
   (%populations :type map
                 :reader populations<-
                 :initarg :populations
                 :initform (empty-map <no-population>))))

(defun world ()
  (make-instance 'world))

(defmethod add-population ((world world)
                           (name string)
                           (population [population]))
  (make-instance 'world
                 :graph (add-node (graph<- world)
                                  name)
                 :populations (with (populations<- world)
                                    name population)))

(defmethod split-population ((world world)
                             (population-name string)
                             (descendant1-name string)
                             (descendant1-share real)
                             (descendant2-name string)
                             (descendant2-share real)
                             (weight real))
  (declare (type (real (0))
                 weight))
  (let ((pop (@ (populations<- world)
                population-name)))
    (if (equal? pop <no-population>)
        world
        (let ((size (size<- pop))
              (div (/ (+ descendant1-share descendant2-share))))
          (make-instance
           'world
           :graph (add-edge (add-nodes (rem-node (graph<- world)
                                                 population-name)
                                       descendant1-name descendant2-name)
                            descendant1-name descendant2-name
                            :weight weight)
           :populations
           (with (with (less (populations<- world)
                             population-name)
                       descendant1-name
                       (<population> (dictionary<- pop)
                                            (round (* size descendant1-share div))))
                 descendant2-name
                 (<population> (dictionary<- pop)
                                      (round (* size descendant2-share div)))))))))

(defmethod connect-populations ((world world)
                                (pop1 string)
                                (pop2 string)
                                (weight real))
  (declare (type (real (0))
                 weight))
  (make-instance 'world
                 :graph (add-edge (graph<- world)
                                  pop1 pop2
                                  :weight weight)
                 :populations (populations<- world)))

(defmethod rem-population ((world world)
                           (population string))
  (make-instance 'world
                 :graph (rem-node (graph<- world)
                                  population)
                 :populations (less (populations<- world)
                                    population)))

(defmethod realize-sound-change ((sound-change sound-change)
                                 (starting-pop string)
                                 (strength real)
                                 (world world))
  (declare (type (real (0))
                 strength))
  ;; As I have no idea how to correctly model this, the proliferation
  ;; of sound changes through populations is going to be hacked
  ;; together in some way that seems sufficiently reasonable
  (labels
      ((_rec (to-do done world)
         (declare (type map to-do)
                  (type set done)
                  (type world world))
         (if (empty? to-do)
             world
             (multiple-value-bind (name strength)
                 (arb to-do)
               (let* ((graph (graph<- world))
                      (pops (populations<- world))
                      (pop (@ pops name)))
                 (if (>= strength (random 1.0))
                     (let ((ns (convert 'map
                                        (neighbours graph)
                                        :key-fn #'identity
                                        :value-fn
                                        (lambda (target)
                                          (/ strength
                                             (weight (find-edge graph
                                                                :source
                                                                name
                                                                :target
                                                                target)))))))
                       (_rec (less (map-union to-do ns
                                              :val-fn #'max)
                                   name)
                             (with done name)
                             (make-instance 'world
                                            :graph graph
                                            :populations (with pops
                                                               name
                                                               (apply-sound-change
                                                                pop)))))
                     (_rec (less to-do name)
                           (with done name)
                           world)))))))
    (_rec (map (starting-pop weight))
          (set)
          world)))

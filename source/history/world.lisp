(in-package #:mang)

(defadt [population]
  <no-population>
  (<simple-population> dictionary (integer (0))))

(defmatch size<- ([population])
    (integer 0)
  (<no-population> 0)
  ((<simple-population> _ size)
   size))

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
                       (<simple-population> (dictionary<- pop)
                                            (round (* size descendant1-share div))))
                 descendant2-name
                 (<simple-population> (dictionary<- pop)
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

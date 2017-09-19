(in-package #:mang)

(defclass learner ()
  ((%markov :type map
            :reader markov<-
            :initarg :markov
            :initform (empty-map <nodist>))
   (%template :type set
              :reader markov-template<-
              :initarg :markov-template
              :initform (empty-set))))

(defun learner (markov markov-template)
  (make-instance 'learner
                 :markov (if (typep (map-default markov)
                                    '[distribution])
                             markov
                             (with-default markov
                               <nodist>))
                 :markov-template markov-template))

(defmethod generate ((markov learner)
                     generator)
  (generate generator (markov<- markov)))

(defmethod learn ((learner learner)
                  obj)
  (let ((template (markov-template<- learner)))
    (learner (map-union (markov<- learner)
                        (learn obj template)
                        #'union)
             template)))

(defmethod learn ((markov map)
                  (obj null))
  (declare (ignore obj))
  markov)

(defmethod learn ((markov map)
                  (obj cons))
  (let ((init (butlast obj))
        (to-learn (last obj)))
    (map-union (learn init markov)
               (image (lambda (predicate dist)
                        (declare (ignore dist))
                        (values predicate (if (@ predicate init)
                                              (uniform-distribution
                                               (list to-learn))
                                              <nodist>)))
                      markov)
               #'union)))

(defmethod learn ((markov-template set)
                  (obj null))
  (empty-map <nodist>))

;;; a markov-template is a set of functions taking two arguments: The intro up
;;; to the point where the word is learned and the glyph to be learned. It
;;; returns a set of functions taking one argument (the intro part of the word
;;; that is already generated) which again return a boolean.
(defmethod learn ((markov-template set)
                  (obj cons))
  (let ((intro (butlast obj))
        (to-learn (last obj)))
    (map-union
     (learn intro markov-template)
     (with-default
         (convert 'map
                  (expand (lambda (generator)
                            (image (lambda (matcher)
                                     (cons matcher
                                           (uniform-distribution `(,to-learn))))
                                   (@ generator intro to-learn)))
                          markov-template))
       <nodist>)
     #'union)))

;;;; Here's a few functions to use in markov templates
(let ((memoized (empty-map)))
  (defun match-outro-generator (length &optional to-learn-predicate)
    (if to-learn-predicate
        (lambda (intro to-learn)
          (if (@ to-learn-predicate to-learn)
              (let* ((parameters (list length intro))
                     (found (@ memoized parameters)))
                (if found
                    found
                    (let ((matcher (let ((outro (outro length intro)))
                                     (set (lambda (future)
                                            (equal? (outro length
                                                           future)
                                                    outro))))))
                      (setf memoized
                            (with memoized parameters matcher))
                      matcher)))
              (empty-set)))
        (lambda (intro to-learn)
          (declare (ignore to-learn))
          (let* ((parameters (list length intro))
                 (found (@ memoized parameters)))
            (if found
                found
                (let ((matcher (let ((outro (outro length intro)))
                                 (set (lambda (future)
                                        (equal? (outro length future)
                                                outro))))))
                  (setf memoized
                        (with memoized parameters matcher))
                  matcher)))))))

(let ((f (constantly t)))
  (defun match-everything-generator ()
    (constantly (set f))))

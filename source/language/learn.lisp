(in-package #:mang)

(defmodel markov ()
  ((%markov :type map
            :reader markov<-
            :initarg :content
            :initform (empty-map <nodist>))))

(defmodel sum-markov ()
  ((%in1 :type (or markov sum-markov diminish-markov)
         :accessor in1<-
         :initarg :in1)
   (%in2 :type (or markov sum-markov diminish-markov)
         :accessor in2<-
         :initarg :in2)
   (%out :type map
         :reader markov<-
         :initform (c? (map-union (markov<- (in1<- self))
                                  (markov<- (in2<- self))
                                  #'union)))))

(defmodel diminish-markov ()
  ((%in1 :type (or markov sum-markov diminish-markov)
         :accessor in1<-
         :initarg :in1)
   (%in2 :type (or markov sum-markov diminish-markov)
         :accessor in2<-
         :initarg :in2)
   (%out :type map
         :reader markov<-
         :initform (c? (map-union (markov<- (in1<- self))
                                  (markov<- (in2<- self))
                                  #'diminish)))))

(defclass learner ()
  ((%markov :type (or markov sum-markov diminish-markov)
            :reader markov<-
            :initarg :markov
            :initform (empty-map <nodist>))
   (%template :type set
              :reader markov-template<-
              :initarg :markov-template
              :initform (empty-set))))

(defmethod learner ((markov map)
                    (markov-template set))
  (make-instance 'learner
                 :markov (make-instance 'markov
                                        :content
                                        (if (typep (map-default markov)
                                                   '[distribution])
                                            markov
                                            (with-default markov
                                              <nodist>)))
                 :markov-template markov-template))

(defmethod learner ((markov markov)
                    (markov-template set))
  (make-instance 'learner
                 :markov (if (typep (map-default (markov<- markov))
                                    '[distribution])
                             markov
                             (with-default markov
                               <nodist>))
                 :markov-template markov-template))

(defmethod learner ((markov sum-markov)
                    (markov-template set))
  (make-instance 'learner
                 :markov (if (typep (map-default (markov<- markov))
                                    '[distribution])
                             markov
                             (with-default markov
                               <nodist>))
                 :markov-template markov-template))

(defmethod learner ((markov diminish-markov)
                    (markov-template set))
  (make-instance 'learner
                 :markov (if (typep (map-default (markov<- markov))
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
                        (learn template obj)
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
     (learn markov-template intro)
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
  (defun match-outro-generator (length &key to-learn-predicate ignore-glyphs)
    (if to-learn-predicate
        (lambda (intro to-learn)
          (if (@ to-learn-predicate to-learn)
              (let* ((parameters (list length intro ignore-glyphs))
                     (found (@ memoized parameters)))
                (if found
                    found
                    (let
                        ((matcher (let ((outro (remove ignore-glyphs
                                                       (outro length intro))))
                                    (set (lambda (future)
                                           (equal? (outro length
                                                          (remove ignore-glyphs
                                                                  future))
                                                   outro))))))
                      (setf memoized
                            (with memoized parameters matcher))
                      matcher)))
              (empty-set)))
        (lambda (intro to-learn)
          (declare (ignore to-learn))
          (let* ((parameters (list length intro ignore-glyphs))
                 (found (@ memoized parameters)))
            (if found
                found
                (let ((matcher (let ((outro (remove ignore-glyphs
                                                    (outro length intro))))
                                 (set (lambda (future)
                                        (equal? (outro length
                                                       (remove ignore-glyphs
                                                               future))
                                                outro))))))
                  (setf memoized
                        (with memoized parameters matcher))
                  matcher)))))))

(let ((f (constantly t)))
  (defun match-everything-generator ()
    (constantly (set f))))

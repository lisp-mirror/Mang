(in-package #:mang)

(defmethod learn ((markov map)
                  (obj null))
  (declare (ignore obj))
  markov)

(defmethod learn ((markov map)
                  (obj cons))
  (let ((init (butlast obj))
        (to-learn (last obj)))
    (map-union (learn markov init)
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

(defclass learning-markov ()
  ((%store :type function
           :reader store<-
           :initarg :store)
   (%positive :type set
              :reader positive<-
              :initarg :positive
              :initform (empty-set))
   (%negative :type set
              :reader negative<-
              :initarg :negative
              :initform (empty-set))
   (%learn :type set
           :reader learn<-
           :initarg :learn
           :initform (empty-set))))

(defun learning-markov (store positive &key (negative (empty-set))
                                         (learn positive))
  (declare (type function store)
           (type set positive negative learn))
  (the
   (values learning-markov &rest nil)
   (values
    (make-instance 'learning-markov
                   :store store
                   :positive positive
                   :negative negative
                   :learn learn))))

(defmethod generate (generator (markov learning-markov)
                     &optional negative)
  (declare (ignore negative))
  (let ((store (&<- (store<- markov))))
    (generate generator
              (let ((positive-markov (empty-map <nodist>)))
                (do-set (category (positive<- markov)
                                  positive-markov)
                  (setf positive-markov
                        (map-union (markov<- (@ store category))
                                   positive-markov
                                   #'union))))
              (let ((negative-markov (empty-map <nodist>)))
                (do-set (category (negative<- markov)
                                  negative-markov)
                  (setf negative-markov
                        (map-union (markov<- (@ store category))
                                   negative-markov
                                   #'union)))))))

(defmethod learn ((markov learning-markov)
                  obj)
  (let ((store (store<- markov)))
    (do-set (category (or (learn<- markov)
                          (positive<- markov)))
      (setf (@ (&<- store)
               category)
            (learn (@ (&<- store)
                      category)
                   obj))))
  markov)

(defclass learner ()
  ((%markov :type map
            :accessor markov<-
            :initarg :markov
            :initform (empty-map <nodist>))
   (%template :type set
              :reader markov-template<-
              :initarg :template
              :initform (empty-set))))

(defun learner (template standard)
  (declare (type set template))
  (make-instance 'learner
                 :template template
                 :markov (map (#'true standard)
                              :default <nodist>)))

(defmethod learn ((markov learner)
                  obj)
  (setf (markov<- markov)
        (map-union (learn (markov-template<- markov)
                          obj)
                   (markov<- markov)
                   #'union))
  markov)

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
  (defun match-outro-generator (length &key to-learn-predicate
                                         (ignore-glyphs (empty-set)))
    (declare (type (integer (0))
                   length)
             (type (or function null)
                   to-learn-predicate)
             (type set ignore-glyphs))
    (if to-learn-predicate
        (lambda (intro to-learn)
          (if (@ to-learn-predicate to-learn)
              (let* ((parameters (list length intro ignore-glyphs))
                     (found (@ memoized parameters)))
                (if found
                    found
                    (let ((matcher (let ((outro (without (outro length intro)
                                                         ignore-glyphs)))
                                     (set (lambda (future)
                                            (postfix? outro
                                                      (without future
                                                               ignore-glyphs)))))))
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
                (let ((matcher (let ((outro (without (outro length intro)
                                                     ignore-glyphs)))
                                 (set (lambda (future)
                                        (postfix? outro
                                                  (without future
                                                           ignore-glyphs)))))))
                  (setf memoized
                        (with memoized parameters matcher))
                  matcher)))))))

(defun true (&rest args)
  (declare (ignore args))
  t)

(defun match-everything-generator ()
  (constantly (set #'true)))

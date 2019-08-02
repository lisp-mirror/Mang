(in-package #:mang)

(defmethod learn ((markov map)
                  (obj null))
  (declare (ignore obj))
  markov)

(defmethod learn ((markov map)
                  (obj cons))
  (bind ((init (butlast obj))
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

(defclass learner ()
  ((%template :type set
              :reader markov-template<-
              :initarg :template
              :initform (set))
   (%markov :type map
            :accessor markov<-
            :initarg :markov
            :initform (empty-map <nodist>))))

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

(defmethod copy ((obj learner))
  (make-instance 'learner
                 :template (markov-template<- obj)
                 :markov (markov<- obj)))

(defclass store ()
  ((%categories :type map
                :reader categories<-
                :initarg :categories
                :initform
                (empty-map))))

(defun store (categories)
  (make-instance 'store
                 :categories categories))

(defmethod copy ((obj store))
  (make-instance 'store
                 :categories (image (lambda (category learner)
                                      (values category (copy learner)))
                                    (categories<- obj))))

(defmethod lookup ((collection store)
                   (key set))
  (bind ((categories (categories<- collection))
         (result (empty-map <nodist>)))
    (do-set (category key result)
      (setf result
            (map-union (markov<- (@ categories category))
                       result)))))

(defmethod generate-word ((word-system word-system)
                          (store store)
                          (positive set)
                          &key (negative (set)))
  (word (generate word-system (@ store positive)
                  (@ store negative))))

(defmethod learn-word ((store store)
                       (word word)
                       (learn set))
  (bind ((categories (categories<- store)))
    (do-set (category learn store)
      (bind ((learner (@ categories category)))
        (when learner
          (learn learner word))))))

;;; a markov-template is a set of functions taking two arguments: The intro up
;;; to the point where the word is learned and the glyph to be learned. It
;;; returns a set of functions taking one argument (the intro part of the word
;;; that is already generated) which again return a boolean.
(defmethod learn ((markov-template set)
                  (obj cons))
  (bind ((intro (butlast obj))
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
(bind ((memoized (empty-map)))
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
              (bind ((parameters (list length intro ignore-glyphs))
                     (found (@ memoized parameters)))
                (if found
                    found
                    (bind ((matcher (bind ((outro (without (outro length intro)
                                                           ignore-glyphs)))
                                      (set
                                       (lambda (future)
                                         (postfix? outro
                                                   (without future
                                                            ignore-glyphs)))))))
                      (setf memoized
                            (with memoized parameters matcher))
                      matcher)))
              (empty-set)))
        (lambda (intro to-learn)
          (declare (ignore to-learn))
          (bind ((parameters (list length intro ignore-glyphs))
                 (found (@ memoized parameters)))
            (if found
                found
                (bind ((matcher (bind ((outro (without (outro length intro)
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

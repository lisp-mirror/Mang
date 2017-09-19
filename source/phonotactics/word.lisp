(in-package #:mang)

(defmethod word-forms<-spec ((spec null))
  (set '()))

(defmethod word-forms<-spec ((spec cons))
  (destructuring-bind (min/repeat/syllable &rest spec)
      spec
    (if (integerp min/repeat/syllable)
        (destructuring-bind (max/syllable &rest spec)
            spec
          (if (integerp max/syllable)
              (destructuring-bind (syllable &rest spec)
                  spec
                (expand (lambda (tail)
                          (image (lambda (intro)
                                   (append intro tail))
                                 (var-repeat min/repeat/syllable
                                             max/syllable syllable)))
                        (word-forms<-spec spec)))
              (image (lambda (tail)
                       (append (repeat min/repeat/syllable max/syllable)
                               tail))
                     (word-forms<-spec spec))))
        (image (lambda (tail)
                 (cons min/repeat/syllable tail))
               (word-forms<-spec spec)))))

(defmethod word-forms<-spec ((spec set))
  (expand #'word-forms<-spec
          spec))

(defun words<-spec (spec classes)
  (image (lambda (word)
           (intersperse "" word))
         (tree-map classes (word-forms<-spec spec))))

(defmodel word-system ()
  ((%spec :type (or symbol cons set)
          :accessor spec<-
          :initarg :spec)
   (%glyph-system :type glyph-system
                  :accessor glyph-system<-
                  :initarg :glyph-system)
   (%dfsm :type dfsm
          :reader dfsm<-
          :initform
          (c? (dfsm<- (words<-spec (spec<- self)
                                   (classes<- (glyph-system<- self))))))))

(defun word-system (spec glyph-system)
  (make-instance 'word-system
                 :spec (c_in spec)
                 :glyph-system (c_in glyph-system)))

(defmethod generate ((generator word-system)
                     markov)
  (generate (dfsm<- generator)
            (with markov
                  (constantly t)
                  (uniform-distribution (set "")))))

(defun string<-word (word &optional syllables?)
  (format nil "~{~A~}"
          (if syllables?
              (mapcar (lambda (glyph)
                        (if (string= glyph "")
                            "·"
                            glyph))
                      word)
              word)))

(defmethod learn ((obj null)
                  (markov map))
  (declare (ignore obj))
  markov)

(defmethod learn ((obj cons)
                  (markov map))
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

(defmethod learn ((obj null)
                  (markov-template set))
  (empty-map <nodist>))

;;; a markov-template is a set of functions taking two arguments: The intro up
;;; to the point where the word is learned and the glyph to be learned. It
;;; returns a set of functions taking one argument (the intro part of the word
;;; that is already generated) which again return a boolean.
(defmethod learn ((obj cons)
                  (markov-template set))
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

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

(defmethod learn ((obj cons)
                  (markov map))
  (image (lambda (predicate distribution)
           (let ((dist distribution))
             (loop :for x :below (length obj)
                :when (@ predicate (subseq obj 0 x))
                :do (setf dist
                          (with dist (elt obj x)
                                1)))
             (values predicate dist)))
         markov))

(in-package #:mang)

(defmodel language ()
  ((%glyphs :type map
            :initarg :glyphs
            :reader glyphs<-
            :cell nil)
   (%categories :type map
                :initarg :categories
                :reader categories<-
                :cell nil)
   (%sonority-hierarchy :type list
                        :initarg :sonority-hierarchy
                        :reader sonority-hierarchy<-
                        :cell nil)
   (%matcher :type dfsm
             :initarg :matcher
             :accessor matcher<-
             :cell nil)
   (%generator :type dfsm
               :initarg :generator
               :accessor generator<-
               :cell nil)
   (%markov-spec :type map
                 :initarg :markov-spec
                 :reader markov-spec<-
                 :cell nil)
   (%store :type map
           :initarg :store
           :accessor store<-
           :cell nil)
   (%dictionary :type map
                :initarg :dictionary
                :accessor dictionary<-
                :unchanged-if #'equal?)
   (%unknown-dictionary :type map
                        :initarg :unknown-dictionary
                        :accessor unknown-dictionary<-
                        :cell nil)))

(defun language (&key
                 (glyphs (empty-map))
                 (categories (empty-map))
                 (sonority-hierarchy '())
                 (matcher (dfsm<- ()))
                 (generator matcher)
                 (markov-spec (empty-map (list (empty-map (empty-set))
                                               '())))
                 (store (empty-map (empty-map (empty-map <nodist>)))))
  (make-instance 'language
                 :glyphs glyphs
                 :categories categories
                 :sonority-hierarchy sonority-hierarchy
                 :matcher matcher
                 :generator generator
                 :markov-spec markov-spec
                 :store store
                 :dictionary (c-in (empty-map (empty-map)))
                 :unknown-dictionary (empty-map (empty-map))))

(defun copy-language (language)
  (make-instance 'language
                 :glyphs (glyphs<- language)
                 :categories (categories<- language)
                 :sonority-hierarchy (sonority-hierarchy<- language)
                 :matcher (matcher<- language)
                 :generator (generator<- language)
                 :markov-spec (markov-spec<- language)
                 :store (store<- language)
                 :dictionary (dictionary<- language)
                 :unknown-dictionary (unknown-dictionary<- language)))

(defun less-word (dfsm word)
  (bind ((accepting (accepting-states<- dfsm))
         (dfsm (less (make-instance 'dfsm
                                    :transitions
                                    (map ($ (transitions<- dfsm))
                                         (:begin (map ((map (:begin t))
                                                       (start-state<- dfsm)))))
                                    :start-state :begin
                                    :accepting-states accepting)
                     word))
         (transitions (transitions<- dfsm)))
    (make-instance 'dfsm
                   :transitions (less transitions :begin)
                   :start-state (@ (@ transitions :begin)
                                   (map (:begin t)))
                   :accepting-states accepting)))

(defmethod add-gloss! ((storage language)
                       (word-categories set)
                       (negative-word-categories set)
                       (part-of-speech string)
                       (gloss string)
                       allow-homophones?)
  (setf (unknown-dictionary<- storage)
        (map* ($ (unknown-dictionary<- storage))
              :default (empty-map)
              (& (part-of-speech defs)
                 (map ($ defs)
                      (gloss
                       (if allow-homophones?
                           (lambda (language)
                             (bind ((matcher (matcher<- language))
                                    (word
                                     (generate-word matcher (store<- language)
                                                    (markov-spec<- language)
                                                    word-categories
                                                    negative-word-categories)))
                               (values word word-categories
                                       negative-word-categories matcher
                                       (less-word (generator<- language)
                                                  word)
                                       t)))
                           (lambda (language)
                             (bind ((generator (generator<- language))
                                    (word
                                     (generate-word generator (store<- language)
                                                    (markov-spec<- language)
                                                    word-categories
                                                    negative-word-categories)))
                               (values word word-categories
                                       negative-word-categories
                                       (matcher<- language)
                                       (less-word generator word)
                                       nil)))))))))
  storage)

(defmethod add-word! ((storage language)
                      (word-categories set)
                      (part-of-speech string)
                      (gloss string)
                      (word list)
                      allow-homophones?)
  (bind ((generator (generator<- storage))
         (matcher (matcher<- storage)))
    (when (not (or (@ (@ (unknown-dictionary<- storage)
                         part-of-speech)
                      gloss)
                   (and generator
                        matcher
                        (not (run-dfsm (if allow-homophones?
                                           matcher
                                           generator)
                                       word)))))
      (setf (dictionary<- storage)
            (map* ($ (dictionary<- storage))
                  :default (empty-map)
                  (& (part-of-speech glosses)
                     (with glosses gloss
                           (list word word-categories allow-homophones?))))
            (store<- storage)
            (learn (store<- storage)
                   (markov-spec<- storage)
                   word word-categories)
            (generator<- storage)
            (less-word generator word))
      (unless allow-homophones?
        (setf (matcher<- storage)
              (less-word matcher word)))))
  storage)

(defun language-gen-word (language word-categories
                          &key
                            allow-homophones?
                            (negative-word-categories (empty-set)))
  (generate-word (if allow-homophones?
                     (matcher<- language)
                     (generator<- language))
                 (store<- language)
                 (markov-spec<- language)
                 word-categories negative-word-categories))

(defun glosses-of (language word)
  (declare (type language language)
           (type list word))
  (reduce (lambda (acc pos defs)
            (declare (ignore pos))
            (union (origin word defs
                           :key #'first)
                   acc))
          (dictionary<- language)
          :initial-value (empty-set)))

(defun heterophone? (language word)
  (reduce (lambda (acc pos defs)
            (declare (ignore pos))
            (or acc (reduce (lambda (acc gloss found-word)
                              (declare (ignore gloss))
                              (bind (((found-word _ allow-homophones?)
                                      found-word))
                                (or acc (and (equal? found-word word)
                                             allow-homophones?))))
                            defs)))
          (dictionary<- language)))

(defmethod less-gloss! ((language language)
                        (part-of-speech string)
                        (gloss string))
  (setf (unknown-dictionary<- language)
        (map* ($ (unknown-dictionary<- language))
              :default (empty-map)
              (& (part-of-speech words)
                 (less words gloss))))
  (bind ((word (@ (@ (dictionary<- language)
                     part-of-speech)
                  gloss)))
    (when word
      (setf (dictionary<- language)
            (map* ($ (dictionary<- language))
                  :default (empty-map)
                  (& (part-of-speech words)
                     (less words gloss)))
            (store<- language)
            (unlearn (store<- language)
                     (markov-spec<- language)
                     (first word)
                     (second word)))
      (unless (heterophone? language word)
        (setf (matcher<- language)
              (less-word (matcher<- language)
                         word)))
      (when (empty? (glosses-of language word))
        (setf (generator<- language)
              (less-word (generator<- language)
                         word)))))
  language)

(defun parse-language-file (binary-features valued-features privative-features)
  (>>!
    glyphs (parse-glyph-section binary-features valued-features
                                privative-features)
    categories (parse-category-section glyphs)
    sonority-hierarchy (<? (parse-sonority-hierarchy glyphs categories))
    generator (// (parse-wordgen-section glyphs categories)
                  (parse-clustergen-section glyphs categories))
    (markov-spec store) (parse-markov-section glyphs categories)
    _ (>> (parse-whitespace)
          (parse-eof))
    (succeed (language :glyphs glyphs
                       :categories categories
                       :sonority-hierarchy sonority-hierarchy
                       :matcher generator
                       :markov-spec markov-spec
                       :store store))))

(defun load-language-file (binary-features valued-features privative-features
                           file)
  (with-open-file (stream file)
    (parser-call (parse-language-file binary-features valued-features
                                      privative-features)
                 stream)))

;;;; Debugging
(defmethod write-dot ((stream cons)
                      (graph language)
                      &key
                        (edge-label-key #'identity)
                        (node-label-key #'identity))
  (declare (ignore edge-label-key))
  (bind ((glyphs (glyphs<- graph)))
    (labels ((glyph (phoneme)
               (or (arb (origin phoneme glyphs))
                   "#")))
      (write-dot (car stream)
                 (matcher<- graph)
                 :edge-label-key #'glyph
                 :node-label-key node-label-key)
      (write-dot (cdr stream)
                 (generator<- graph)
                 :edge-label-key #'glyph
                 :node-label-key node-label-key))))

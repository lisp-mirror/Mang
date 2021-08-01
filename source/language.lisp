(in-package #:mang)

(defclass language ()
  ((%name :type string
          :initarg :name
          :reader name<-)
   (%glyphs :type map
            :initarg :glyphs
            :reader glyphs<-)
   (%categories :type map
                :initarg :categories
                :reader categories<-)
   (%sonority-hierarchy :type list
                        :initarg :sonority-hierarchy
                        :reader sonority-hierarchy<-)
   (%matcher :type dfsm
             :initarg :matcher
             :reader matcher<-)
   (%generator :type dfsm
               :initarg :generator
               :reader generator<-)
   (%markov-spec :type map
                 :initarg :markov-spec
                 :reader markov-spec<-)
   (%store :type map
           :initarg :store
           :reader store<-)
   (%dictionary :type map
                :initarg :dictionary
                :reader dictionary<-)
   (%unknown-dictionary :type map
                        :initarg :unknown-dictionary
                        :reader unknown-dictionary<-)))

(defun language (name
                 &key
                   (glyphs (empty-map))
                   (categories (empty-map))
                   (sonority-hierarchy '())
                   (matcher (dfsm<- ()))
                   (generator matcher)
                   (markov-spec (empty-map (list (empty-map (empty-set))
                                                 '())))
                   (store (empty-map (empty-map (empty-map <nodist>)))))
  (make-instance 'language
                 :name name
                 :glyphs glyphs
                 :categories categories
                 :sonority-hierarchy sonority-hierarchy
                 :matcher matcher
                 :generator generator
                 :markov-spec markov-spec
                 :store store
                 :dictionary (empty-map (empty-map))
                 :unknown-dictionary (empty-map (empty-map))))

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

(defmethod add-gloss ((storage language)
                      (word-categories set)
                      (negative-word-categories set)
                      (part-of-speech string)
                      (gloss string))
  (make-instance 'language
                 :name (name<- storage)
                 :glyphs (glyphs<- storage)
                 :categories (categories<- storage)
                 :sonority-hierarchy (sonority-hierarchy<- storage)
                 :matcher (matcher<- storage)
                 :generator (generator<- storage)
                 :markov-spec (markov-spec<- storage)
                 :store (store<- storage)
                 :dictionary (dictionary<- storage)
                 :unknown-dictionary
                 (map* ($ (unknown-dictionary<- storage))
                       :default (empty-map)
                       (& (part-of-speech defs)
                          (map ($ defs)
                               (gloss (cons word-categories
                                            negative-word-categories)))))))

(defmethod add-word ((storage language)
                     (word-categories set)
                     (part-of-speech string)
                     (gloss string)
                     (word list)
                     allow-homophones?)
  (bind ((matcher (matcher<- storage))
         (generator (generator<- storage))
         (markov-spec (markov-spec<- storage))
         (found-word-categories (@ (@ (unknown-dictionary<- storage)
                                      part-of-speech)
                                   gloss)))
    (if (and (or (not found-word-categories)
                 (equal? (car found-word-categories)
                         word-categories))
             (run-dfsm (if allow-homophones?
                           matcher
                           generator)
                       (rest word)))
        (values (make-instance 'language
                               :name (name<- storage)
                               :glyphs (glyphs<- storage)
                               :categories (categories<- storage)
                               :sonority-hierarchy
                               (sonority-hierarchy<- storage)
                               :matcher (if allow-homophones?
                                            matcher
                                            (less-word matcher word))
                               :generator (less-word generator word)
                               :markov-spec markov-spec
                               :store (learn (store<- storage)
                                             markov-spec word word-categories)
                               :dictionary
                               (map* ($ (dictionary<- storage))
                                     :default (empty-map)
                                     (& (part-of-speech words)
                                        (with words
                                              gloss (list word word-categories
                                                          allow-homophones?))))
                               :unknown-dictionary
                               (map* ($ (unknown-dictionary<- storage))
                                     :default (empty-map)
                                     (& (part-of-speech unwords)
                                        (less unwords gloss))))
                t)
        (values storage nil))))

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

(defun prompt-gen-word (language part-of-speech gloss word-categories
                        &key
                          allow-homophones?
                          (negative-word-categories (empty-set)))
  (bind ((generator (if allow-homophones?
                        (matcher<- language)
                        (generator<- language))))
    (add-word language word-categories part-of-speech gloss
              (loop
                :as word
                  := (generate-word generator (store<- language)
                                    (markov-spec<- language)
                                    word-categories negative-word-categories)
                :until (y-or-n-p "Use <~A> for gloss \"~A\"?"
                                 (string<-word (glyphs<- language)
                                               word
                                               :computer-readable? nil)
                                 gloss)
                :do
                   (setf generator
                         (less-word generator word))
                :finally
                   (return word))
              allow-homophones?)))

(defun prompt-gen-known-gloss (language part-of-speech gloss
                               &key
                                 allow-homophones?)
  ([a]if (@ (@ (unknown-dictionary<- language)
               part-of-speech)
            gloss)
      (values (prompt-gen-word language part-of-speech gloss (car it)
                               :negative-word-categories (cdr it)
                               :allow-homophones? allow-homophones?)
              t)
    (values language nil)))

(defun parse-language-header ()
  (>> (parse-whitespace)
      (parse-constant "##")
      (parse-whitespace-no-newline)
      (parse-identifier)))

(defun parse-language-file (binary-features valued-features privative-features)
  (>>!
    name (parse-language-header)
    glyphs (parse-glyph-section binary-features valued-features
                                privative-features)
    categories (parse-category-section glyphs)
    sonority-hierarchy (<? (parse-sonority-hierarchy glyphs categories))
    generator (// (parse-wordgen-section glyphs categories)
                  (parse-clustergen-section glyphs categories))
    (markov-spec store) (parse-markov-section glyphs categories)
    _ (>> (parse-whitespace)
          (parse-eof))
    (succeed (language name
                       :glyphs glyphs
                       :categories categories
                       :sonority-hierarchy sonority-hierarchy
                       :matcher generator
                       :markov-spec markov-spec
                       :store store))))

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

(in-package #:mang)

(defclass language ()
  ((%glyphs :type map
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
                :reader dictionary<-)))

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
                 :dictionary (empty-map (empty-map))))

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

(defmethod add-word ((storage language)
                     (word-categories set)
                     (part-of-speech string)
                     (gloss string)
                     (word list)
                     &optional
                       (allow-homophones? t))
  (bind ((matcher (matcher<- storage))
         (generator (generator<- storage))
         (markov-spec (markov-spec<- storage)))
    (if (run-dfsm (if allow-homophones?
                      matcher
                      generator)
                  (rest word))
        (make-instance 'language
                       :glyphs (glyphs<- storage)
                       :categories (categories<- storage)
                       :sonority-hierarchy (sonority-hierarchy<- storage)
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
                                      gloss (list word word-categories)))))
        storage)))

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
    (add-word *language* word-categories part-of-speech gloss
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

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
                :cell t)
   (%unknown-dictionary :type map
                        :initarg :unknown-dictionary
                        :accessor unknown-dictionary<-
                        :cell t)))

(defun language (&key
                 (glyphs (empty-map))
                 (categories (empty-map))
                 (sonority-hierarchy '())
                 matcher (generator matcher)
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
                 :unknown-dictionary (c-in (empty-map (empty-map)))))

(defun copy-language (language
                      &key
                        glyphs (new-glyphs (empty-map))
                        categories (new-categories (empty-map))
                        (sonority-hierarchy nil sonority-hierarchy?)
                        (matcher nil matcher?)
                        (generator nil generator?)
                        (markov-spec nil markov-spec?)
                        store
                        dictionary new-dictionary
                        unknown-dictionary new-unknown-dictionary)
  (make-instance 'language
                 :glyphs (or glyphs (map-union (glyphs<- language)
                                               new-glyphs))
                 :categories (or categories
                                 (map-union (categories<- language)
                                            new-categories))
                 :sonority-hierarchy (if sonority-hierarchy?
                                         sonority-hierarchy
                                         (sonority-hierarchy<- language))
                 :matcher (if matcher?
                              matcher
                              (matcher<- language))
                 :generator (if generator?
                                generator
                                (generator<- language))
                 :markov-spec (if markov-spec?
                                  markov-spec
                                  (markov-spec<- language))
                 :store (or store
                            (if markov-spec?
                                (empty-map (empty-map (empty-map <nodist>)))
                                (store<- language)))
                 :dictionary (if dictionary
                                 (if (typep dictionary 'cell)
                                     dictionary
                                     (c-in dictionary))
                                 (if new-dictionary
                                     (c_? (map-union (dictionary<- language)
                                                     new-dictionary
                                                     #'map-union))
                                     (c_? (dictionary<- language))))
                 :unknown-dictionary
                 (if unknown-dictionary
                     (if (typep unknown-dictionary 'cell)
                         unknown-dictionary
                         (c-in unknown-dictionary))
                     (if new-unknown-dictionary
                         (c_? (map-union (unknown-dictionary<- language)
                                         new-unknown-dictionary
                                         #'map-union))
                         (c_? (unknown-dictionary<- language))))))

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

(defmethod add-word! ((storage language)
                      (word-categories set)
                      (part-of-speech string)
                      (gloss string)
                      (word list)
                      allow-homophones?)
  (bind ((generator (generator<- storage))
         (matcher (matcher<- storage)))
    (unless (or (@ (@ (unknown-dictionary<- storage)
                      part-of-speech)
                   gloss)
                (and generator
                     matcher
                     (not (run-dfsm (if allow-homophones?
                                        matcher
                                        generator)
                                    (rest word)))))
      (setf (dictionary<- storage)
            (map* ($ (dictionary<- storage))
                  :default (empty-map)
                  (& (part-of-speech glosses)
                     (with glosses gloss
                           (list word word-categories
                                 allow-homophones?))))
            (generator<- storage)
            (when generator
              (less-word generator word)))
      ([d]setf (learn (store<- storage)
                      (markov-spec<- storage)
                      word word-categories))
      (unless (or allow-homophones? (not matcher))
        (setf (matcher<- storage)
              (less-word matcher word)))))
  storage)

(defun get-known-word (language pos gloss)
  (@ (@ (dictionary<- language)
        pos)
     gloss))

(defun make-unknown-word (language part-of-speech gloss allow-homophones?
                          word-categories negative-word-categories)
  (bind ((rejected (empty-set)))
    (lambda ()
      (bind ((word (generate-word (less* (if allow-homophones?
                                             (matcher<- language)
                                             (generator<- language))
                                         rejected)
                                  (store<- language)
                                  (markov-spec<- language)
                                  word-categories
                                  negative-word-categories)))
        (values word (lambda ()
                       (if (run-dfsm (if allow-homophones?
                                         (matcher<- language)
                                         (generator<- language))
                                     (rest word))
                           (progn
                             (setf (dictionary<- language)
                                   (map* ($ (dictionary<- language))
                                         :default (empty-map)
                                         (& (part-of-speech glosses)
                                            (with glosses gloss
                                                  (list word word-categories
                                                        allow-homophones?))))
                                   (unknown-dictionary<- language)
                                   (filter (lambda (k v)
                                             (declare (ignore k))
                                             (not (empty? v)))
                                           (map* ($ (unknown-dictionary<-
                                                     language))
                                                 :default (empty-map)
                                                 (& (part-of-speech defs)
                                                    (less defs gloss)))))
                             ([d]setf (learn (store<- language)
                                             (markov-spec<- language)
                                             word word-categories)
                                      (less (generator<- language)
                                            word))
                             (unless allow-homophones?
                               ([d]setf (less (matcher<- language)
                                              word)))
                             (values language
                                     (lambda (stream computer-readable?)
                                       (format stream
                                               "~A ~A ~A ~A {~{~A~^,~}}~%"
                                               gloss part-of-speech
                                               (string<-word (glyphs<- language)
                                                             word
                                                             :computer-readable?
                                                             computer-readable?)
                                               (if allow-homophones?
                                                   "y"
                                                   "n")
                                               (convert 'list
                                                        word-categories)))))
                           (values language nil)))
                (lambda ()
                  (setf rejected (with rejected (rest word)))
                  (values language nil))
                allow-homophones? word-categories negative-word-categories
                language)))))

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
                      (gloss (make-unknown-word storage part-of-speech gloss
                                                allow-homophones?
                                                word-categories
                                                negative-word-categories))))))
  storage)

(defun get-unknown-word (language pos gloss)
  (@ (@ (unknown-dictionary<- language)
        pos)
     gloss))

(defun get-word (language pos gloss)
  (mv-or
    (values (get-known-word language pos gloss)
            :known)
    (values (get-unknown-word language pos gloss)
            :unknown)))

(defmethod known-gloss-candidate ((storage language)
                                  (part-of-speech string)
                                  (gloss string))
  ([a]when (get-unknown-word storage part-of-speech gloss)
    (funcall it)))

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

(defun pos+glosses-of (language word)
  (declare (type language language)
           (type list word))
  (reduce (lambda (acc pos defs)
            (union (image (lambda (gloss)
                            (cons pos gloss))
                          (origin word defs
                                  :key #'first))
                   acc))
          (dictionary<- language)))

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

(defmethod less-known-gloss! ((language language)
                              (part-of-speech string)
                              (gloss string))
  (bind ((word (@ (@ (dictionary<- language)
                     part-of-speech)
                  gloss)))
    (when word
      (setf (dictionary<- language)
            (map* ($ (dictionary<- language))
                  :default (empty-map)
                  (& (part-of-speech words)
                     (less words gloss))))
      ([d]setf (unlearn (store<- language)
                        (markov-spec<- language)
                        (first word)
                        (second word)))
      (unless (heterophone? language word)
        ([d]setf (less-word (matcher<- language)
                            word)))
      (when (empty? (glosses-of language word))
        ([d]setf (less-word (generator<- language)
                            word)))))
  language)

(defmethod less-unknown-gloss! ((language language)
                                (part-of-speech string)
                                (gloss string))
  (setf (unknown-dictionary<- language)
        (map* ($ (unknown-dictionary<- language))
              :default (empty-map)
              (& (part-of-speech words)
                 (less words gloss))))
  language)

(defmethod less-gloss! ((language language)
                        (part-of-speech string)
                        (gloss string))
  (less-known-gloss! (less-unknown-gloss! language part-of-speech gloss)
                     part-of-speech gloss))

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

(defun ask-known-gloss! (language part-of-speech gloss)
  ([a]when (@ (@ (unknown-dictionary<- language)
                 part-of-speech)
              gloss)
    (bind (((:values word accept reject)
            (funcall it)))
      (if (y-or-n-p "Accept <~A> for gloss \"~A\"?"
                    (string<-word (glyphs<- language)
                                  word
                                  :computer-readable? nil)
                    gloss)
          (funcall accept)
          (funcall reject)))))

(defun ask-arbitrary-known-gloss! (language)
  (bind (((:values pos defs)
          (arb (filter (lambda (k v)
                         (declare (ignore k))
                         (not (empty? v)))
                       (unknown-dictionary<- language)))))
    (when pos
      (ask-known-gloss! language pos (arb defs)))))

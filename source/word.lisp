(in-package #:mang)

(defun parse-word (glyphs)
  (declare (type map glyphs))
  (<$> (some (<$> (parse-glyph glyphs)
                  #'second)
             '() #'cons)
       (lambda (word)
         (append `(,(map (:begin t)))
                 word `(,(map (:end t)))))))

(defun parse-defined-entry (glyphs store markov-spec parts-of-speech)
  (declare (type map glyphs store markov-spec)
           (type set parts-of-speech))
  (>>!
    gloss (parse-identifier (with *mang-reserved-symbols* "-"))
    _ (parse-whitespace)
    part-of-speech (parse-from-set parts-of-speech)
    _ (parse-whitespace)
    word (parse-word glyphs)
    _ (parse-whitespace)
    categories
    (<? (parse-wrapped "{" (parse-separated (parse-from-map markov-spec)
                                            "," (empty-set)
                                            (lambda (cat cats)
                                              (with cats (first cat))))
                       "}")
        (empty-set))
    (succeed `(,(map (part-of-speech (map (gloss `(,word ,categories))))
                     :default (empty-map))
                ,(learn store markov-spec word categories)))))

(defun parse-generated-entry (dfsm store markov-spec parts-of-speech)
  (declare (type map store markov-spec)
           (type dfsm dfsm)
           (type set parts-of-speech))
  (>>!
    gloss (parse-identifier (with *mang-reserved-symbols* "-"))
    _ (parse-whitespace)
    part-of-speech (parse-from-set parts-of-speech)
    _ (>> (parse-whitespace)
          (parse-constant "#")
          (parse-whitespace))
    categories
    (parse-wrapped "{" (parse-separated (parse-from-map markov-spec)
                                        "," (empty-set)
                                        (lambda (cat cats)
                                          (with cats (first cat))))
                   "}")
    negative-categories
    (>> (parse-whitespace)
        (<? (parse-wrapped "{" (parse-separated (parse-from-map markov-spec)
                                                "," (empty-set)
                                                (lambda (cat cats)
                                                  (with cats (first cat))))
                           "}")
            (empty-set)))
    (bind ((word (generate-word dfsm store markov-spec categories
                                negative-categories)))
      (succeed `(,(map (part-of-speech (map (gloss `(,word ,categories))))
                       :default (empty-map))
                  ,(learn store markov-spec word categories))))))

(defun parse-entry (glyphs dfsm store markov-spec parts-of-speech)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm)
           (type set parts-of-speech))
  (// (parse-defined-entry glyphs store markov-spec parts-of-speech)
      (parse-generated-entry dfsm store markov-spec parts-of-speech)))

(defun parse-entries (glyphs dfsm store markov-spec parts-of-speech)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm)
           (type set parts-of-speech))
  (>>!
    (front store)
    (parse-entry glyphs dfsm store markov-spec parts-of-speech)
    (back store)
    (<? (>> (parse-expression-end)
            (parse-whitespace)
            (parse-entries glyphs dfsm store markov-spec parts-of-speech))
        `(,(empty-map (empty-map))
           ,store))
    (succeed `(,(map-union back front #'map-union)
                ,store))))

(defun parse-dictionary (glyphs dfsm store markov-spec)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm))
  (parse-section "dictionary"
                 (>>!
                   parts-of-speech (parse-separated (parse-identifier
                                                     *mang-reserved-symbols*)
                                                    "," (empty-set)
                                                    (lambda (pos poss)
                                                      (with poss pos)))
                   _ (parse-whitespace)
                   (dictionary store)
                   (parse-entries glyphs dfsm store markov-spec
                                  parts-of-speech)
                   (succeed `(,dictionary ,store)))))

(defun write-dictionary (stream glyphs dictionary
                         &optional sort-by-gloss?)
  (declare (type map dictionary))
  (format stream "# dictionary")
  (terpri stream)
  (format stream "~{~A~^,~}"
          (convert 'list
                   (domain dictionary)))
  (bind ((dict (sort (convert
                      'list
                      (reduce
                       #'map-union
                       (convert
                        'set
                        dictionary
                        :pair-fn
                        (lambda (pos map)
                          (image (lambda (gloss def)
                                   (bind (((word categories)
                                           def))
                                     (values gloss
                                             (list pos
                                                   (reduce
                                                    (lambda (s1 s2)
                                                      (concatenate
                                                       'string
                                                       s1 "<"
                                                       (arb (origin s2 glyphs))
                                                       ">"))
                                                    (butlast (rest word))
                                                    :initial-value "")
                                                   categories))))
                                 map)))
                       :initial-value (empty-map)))
                     #'string<
                     :key (if sort-by-gloss?
                              #'first
                              #'third))))
    (loop :for (gloss pos word categories)
       :in dict
       :do
         (terpri stream)
         (if (empty? categories)
             (format stream "~A ~A ~A"
                     gloss pos word)
             (format stream "~A ~A ~A {~{~A~^,~}}"
                     gloss pos word (convert 'list
                                             categories))))))

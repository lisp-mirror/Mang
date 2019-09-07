(in-package #:mang)

(defun parse-word (glyphs)
  (declare (type map glyphs))
  (<$> (some (<$> (parse-glyph glyphs)
                  #'second)
             '() #'cons)
       (lambda (word)
         (append `(,(map (:begin t)))
                 word `(,(map (:end t)))))))

(defun parse-gloss (glyphs dfsm store markov-spec parts-of-speech)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm)
           (type set parts-of-speech))
  (>>!
    gloss (parse-identifier *mang-reserved-symbols*)
    _ (parse-whitespace)
    part-of-speech (parse-from-set parts-of-speech)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    word (// (parse-word glyphs)
             (parse-constant "#"))
    _ (parse-whitespace)
    (categories negative-categories)
    (<? (>>!
          categories (parse-wrapped "{" (parse-separated (parse-from-map
                                                          markov-spec)
                                                         "," (empty-set)
                                                         (lambda (cat cats)
                                                           (with cats
                                                                 (first cat))))
                                    "}")
          negative-categories
          (<? (>> (parse-whitespace)
                  (parse-wrapped "{" (parse-separated (parse-from-map store)
                                                      "," (empty-set)
                                                      (lambda (cat cats)
                                                        (with cats
                                                              (first cat))))
                                 "}")))
          (succeed `(,categories ,negative-categories)))
        `(nil nil))
    (if word
        (if negative-categories
            (fail `(:save-with-negative-categories ,gloss ,word
                                                   ,negative-categories))
            (succeed `(,(map (part-of-speech (map (gloss word))))
                        ,(learn store markov-spec word
                                (or categories (empty-set))))))
        (if categories
            (bind ((word (generate-word dfsm store categories
                                        (or negative-categories (empty-set)))))
              (succeed `(,(map (gloss (list word categories)))
                          ,(learn store markov-spec word categories))))
            (fail `(:generate-without-categories ,gloss))))))

(defun parse-glosses (glyphs dfsm store markov-spec parts-of-speech)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm)
           (type set parts-of-speech))
  (>>!
    (front store)
    (parse-gloss glyphs dfsm store markov-spec parts-of-speech)
    (back store)
    (<? (>> (parse-expression-end)
            (parse-whitespace)
            (parse-glosses glyphs dfsm store markov-spec parts-of-speech)))
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
                   (dictionary store)
                   (parse-glosses glyphs dfsm store markov-spec
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
                              #'second
                              #'third))))
    (loop :for (gloss pos word categories)
       :in dict
       :do
         (terpri stream)
         (if (empty? categories)
             (format stream "~A ~A := ~A"
                     gloss pos word)
             (format stream "~A ~A := ~A {~{~A~^,~}}"
                     gloss pos word (convert 'list
                                             categories))))))

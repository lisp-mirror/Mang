(in-package #:mang)

(defun parse-word (glyphs)
  (declare (type map glyphs))
  (<$> (some (<$> (parse-glyph glyphs)
                  #'second)
             '() #'cons)
       (lambda (word)
         (append `(,(map (:begin t)))
                 word `(,(map (:end t)))))))

(defun parse-gloss ()
  (some (>>* (parse-anything)
             parsed (if (find-if (lambda (parsed-char)
                                   (or (has-property parsed-char "Whitespace")
                                       (has-property parsed-char "Control")
                                       (@ (set #\-)
                                          parsed-char)))
                                 parsed)
                        (fail nil)
                        (succeed parsed)))))

(defun parse-defined-entry (glyphs store markov-spec parts-of-speech)
  (declare (type map glyphs store markov-spec)
           (type set parts-of-speech))
  (>>!
    gloss (parse-gloss)
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

(defun string<-word (glyphs word
                   &key
                     (computer-readable? t))
  (reduce (lambda (string phoneme)
            (concatenate 'string
                         string (if computer-readable?
                                    "<"
                                    "")
                         (arb (origin phoneme glyphs))
                         (if computer-readable?
                             ">"
                             "")))
          (butlast (rest word))
          :initial-value ""))

(defun parse-generated-entry (glyphs dfsm store markov-spec parts-of-speech
                              &optional interactive?)
  (declare (type map store markov-spec)
           (type dfsm dfsm)
           (type set parts-of-speech))
  (>>!
    gloss (parse-gloss)
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
        (// (parse-wrapped "{" (parse-separated (parse-from-map markov-spec)
                                                "," (empty-set)
                                                (lambda (cat cats)
                                                  (with cats (first cat))))
                           "}")
            (>> (parse-constant "{}")
                (succeed (empty-set)))))
    (bind ((word (if interactive?
                     (if (integerp interactive?)
                         (bind ((words (loop
                                         :for n :below interactive?
                                         :as word
                                           := (generate-word dfsm store
                                                             markov-spec
                                                             categories
                                                             negative-categories)
                                         :do
                                            (format t "~D. ~A~%"
                                                    n (string<-word glyphs word
                                                                    :computer-readable?
                                                                    nil))
                                         :collect word)))
                           (format t "Choose one of the generated options for ~
                              gloss \"~A\"? "
                                   gloss)
                           (loop
                             :as choice
                               := (parser-call (parse-number)
                                               *standard-input*)
                             :when (>= choice interactive?)
                               :do
                                  (format t "Not a valid index, please provide ~
                              a number below ~D: "
                                          interactive?)
                             :until (< choice interactive?)
                             :finally
                                (return (nth choice words))))
                         (loop
                           :as word
                             := (generate-word dfsm store markov-spec categories
                                               negative-categories)
                           :while
                           (not (y-or-n-p "Use <~A> for gloss \"~A\"?"
                                          (string<-word glyphs word
                                                        :computer-readable? nil)
                                          gloss))
                           :finally
                              (return word)))
                     (generate-word dfsm store markov-spec categories
                                    negative-categories))))
      (format t "Generated <~A> for gloss \"~A\".~%"
              (string<-word glyphs word
                            :computer-readable? nil)
              gloss)
      (succeed `(,(map (part-of-speech (map (gloss `(,word ,categories))))
                       :default (empty-map))
                 ,(learn store markov-spec word categories))))))

(defun parse-entry (glyphs dfsm store markov-spec parts-of-speech
                    &optional interactive?)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm)
           (type set parts-of-speech))
  (// (parse-defined-entry glyphs store markov-spec parts-of-speech)
      (parse-generated-entry glyphs dfsm store markov-spec parts-of-speech
                             interactive?)))

(defun parse-entries (glyphs dfsm store markov-spec parts-of-speech
                      &optional interactive?)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm)
           (type set parts-of-speech))
  (>>!
    (front store)
    (parse-entry glyphs dfsm store markov-spec parts-of-speech interactive?)
    (back store)
    (<? (>> (parse-expression-end)
            (parse-whitespace)
            (parse-entries glyphs dfsm store markov-spec parts-of-speech interactive?))
        `(,(empty-map (empty-map))
           ,store))
    (succeed `(,(map-union back front #'map-union)
                ,store))))

(defun parse-dictionary (glyphs dfsm store markov-spec &optional interactive?)
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
                                  parts-of-speech interactive?)
                   (succeed `(,dictionary ,store)))))

(defun write-dictionary (stream glyphs dictionary
                         &key
                           (sort-by-gloss? t)
                           (computer-readable? t)
                           (sorting-predicate (lambda (s1 s2)
                                                (string< (string-downcase s1)
                                                         (string-downcase s2)))))
  (declare (type map glyphs dictionary)
           (type boolean sort-by-gloss? computer-readable?)
           (type (function (string string)
                           boolean)
                 sorting-predicate))
  (format stream "# dictionary~%")
  (format stream "~{~A~^,~}~%"
          (convert 'list
                   (domain dictionary)))
  (bind ((dict
          (sort (convert
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
                                              (string<-word glyphs word
                                                            :computer-readable?
                                                            computer-readable?)
                                              categories))))
                            map)))
                  :initial-value (empty-map)))
                sorting-predicate
                :key (if sort-by-gloss?
                         #'first
                         #'third))))
    (terpri)
    (loop
      :for (gloss pos word categories)
        :in dict
      :do
         (if (empty? categories)
             (format stream "~A ~A ~A~%"
                     gloss pos word)
             (format stream "~A ~A ~A {~{~A~^,~}}~%"
                     gloss pos word (convert 'list
                                             categories))))))

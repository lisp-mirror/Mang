(in-package #:mang)

;;;; Dwarf data
(defparameter *urwormdwarf-phonemes*
  (glyph-system (map ('consonant (set "m" "n̪" "ɳ" "ɴ" "b" "d̪" "ɢ" "v" "v̪͆" "ð"
                                      "ʁ" "ɦ" "r"))
                     ('begin (set "m" "n̪" "ɴ" "b" "d̪" "ɢ" "v̪͆" "ʁ" "ɦ" "ɮ̪" "r"))
                     ('middle (set "m" "ɳ" "ɴ" "b" "d̪" "ɢ" "v" "v̪͆" "ð" "ʁ" "ɦ"
                                   "ɮ̪" "r" "ð"))
                     ('end (set "m" "n̪" "ɳ" "ɴ" "v̪͆" "ʁ"))
                     ('unstressed (set "ɨ" "u" "ɒ"))
                     ('stressed (set "ɨ" "ɨ:" "u" "u:" "ɒ" "ɒ:" "ɨ̰" "ṵ" "ɒ̰")))))

(defparameter *urwormdwarf-romanization*
  (map ("m" "m")
       ("n̪" "n")
       ("ɳ" "ṇ")
       ("ɴ" "ň")
       ("b" "b")
       ("d̪" "d")
       ("ɢ" "g")
       ("v" "v")
       ("v̪͆" "ẑ")
       ("ð" "z")
       ("ʁ" "x")
       ("ɦ" "h")
       ("ɮ̪" "l")
       ("r" "r")
       ("ɨ" "y")
       ("ɨ:" "ȳ")
       ("u" "u")
       ("u:" "ū")
       ("ɒ" "a")
       ("ɒ:" "ā")
       ("ɨ̰" "ÿ")
       ("ṵ" "ü")
       ("ɒ̰" "ä")))

(defparameter *urwormdwarf-words*
  (word-system (set (list `(begin stressed ,(set 'end nil)))
                    (list `(begin stressed)
                          `(middle unstressed ,(set 'end nil)))
                    (list `(begin stressed)
                          `(middle unstressed)
                          `(middle stressed ,(set 'end nil)))
                    (list `(begin stressed)
                          `(middle unstressed)
                          `(middle stressed)
                          `(middle unstressed ,(set 'end nil))))
               *urwormdwarf-phonemes*))

(defparameter *urwormdwarf-store*
  (let ((dist (uniform-distribution (glyphs<- *urwormdwarf-phonemes*)
                                    100)))
    (store (image (lambda (category template)
                    (values category (learner template dist)))
                  (map
                   (:count
                    (set (match-everything-generator)))
                   (:everything
                    (set (match-outro-generator 4
                                                :ignore-glyphs (set ""))
                         (match-outro-generator 3)
                         (match-outro-generator 2
                                                :ignore-glyphs (set ""))))
                   (:noun
                    (set (match-outro-generator 4
                                                :ignore-glyphs (set ""))
                         (match-outro-generator 3)
                         (match-outro-generator 2
                                                :ignore-glyphs (set ""))))
                   (:verb
                    (set (match-outro-generator 4
                                                :ignore-glyphs (set ""))
                         (match-outro-generator 3)
                         (match-outro-generator 2
                                                :ignore-glyphs (set ""))))
                   (:short
                    (set (match-outro-generator 2)))
                   (:positive
                    (set (match-outro-generator 5
                                                :ignore-glyphs (set ""))
                         (match-outro-generator 4)
                         (match-outro-generator 3
                                                :ignore-glyphs (set ""))))
                   (:negative
                    (set (match-outro-generator 5
                                                :ignore-glyphs (set ""))
                         (match-outro-generator 4)
                         (match-outro-generator 3
                                                :ignore-glyphs (set "")))))))))

(defparameter *urwormdwarf-dictionary*
  (dictionary))

(defun learn-urwormdwarf-word (form gloss
                               &optional (learn (set :count :everything)))
  (let ((word (word form)))
    (unless (run-dfsm *urwormdwarf-words* form)
      (error "Form ~S is not a valid urwormdwarf word"
             form))
    (unless (empty? (@ *urwormdwarf-dictionary* form))
      (warn "Homophones for ~S:~% ~S ~S~%~{~{ ~S~}~}"
            form gloss learn
            (convert 'list
                     (image (lambda (entry)
                              (list (gloss<- entry)
                                    (learn<- entry)))
                            (@ *urwormdwarf-dictionary* form)))))
    (learn-word *urwormdwarf-store* word learn)
    (setf *urwormdwarf-dictionary*
          (with *urwormdwarf-dictionary*
                (dictionary-entry word gloss learn)))))

(defun generate-urwormdwarf-word (categories &optional (negative (set)))
  (generate-word *urwormdwarf-words* *urwormdwarf-store* categories
                 :negative negative))

;;;; Nouns
(learn-urwormdwarf-word '("d̪" "ɒ:" "" "m" "ɨ")  ; dāmy
                        "person"
                        (set :count :everything :noun))

(learn-urwormdwarf-word '("ɢ" "ṵ" "" "d̪" "ɒ" "ɴ")  ; güdaň
                        "rock"
                        (set :count :everything :noun))

(learn-urwormdwarf-word '("ɢ" "ɒ̰" "" "ɮ̪" "u" "m")  ; gälum
                        "body"
                        (set :count :everything :noun))

(learn-urwormdwarf-word '("ɴ" "ɒ:" "" "v̪͆" "ɨ" "v̪͆")  ; ňāẑyẑ
                        "word"
                        (set :count :everything :noun))

;;;; Verbs
(learn-urwormdwarf-word '("ʁ" "ɨ" "ɳ")  ; xyṇ
                        "be"
                        (set :count :everything :verb :short))

(learn-urwormdwarf-word '("m" "u:" "" "b" "ɨ")  ; mūby
                        "live"
                        (set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ɢ" "ɒ" "" "d̪" "ɨ" "ɴ")  ; gadyň
                        "die"
                        (set :count :everything :verb :negative))

(learn-urwormdwarf-word '("m" "ɒ̰" "ʁ")  ; mäx
                        "quake"
                        (set :count :everything :verb :negative))

(learn-urwormdwarf-word '("r" "ɒ̰" "" "ɦ" "ɒ" "m")  ; räham
                        "hear"
                        (set :count :everything :verb))

(learn-urwormdwarf-word '("v̪͆" "u:" "" "ɳ" "ɒ" "" "ɦ" "ɨ" "ɴ")  ; ẑūṇahyň
                        "say"
                        (set :count :everything :verb))

(learn-urwormdwarf-word '("m" "u:" "" "m" "ɨ" "m")  ; mūmym
                        "feel (emotion)"
                        (set :count :everything :verb))

(learn-urwormdwarf-word '("ɴ" "ṵ" "" "ɴ" "ɒ")  ; ňüňa
                        "want"
                        (set :count :everything :verb :positive :negative))

(learn-urwormdwarf-word '("ʁ" "ṵ" "" "ɴ" "ɨ" "ʁ")  ; xüňyx
                        "know"
                        (set :count :everything :verb :positive))

(learn-urwormdwarf-word '("n̪" "ɨ" "" "m" "u" "ɳ")  ; nymuṇ
                        "eat"
                        (set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ɮ̪" "u" "" "v̪͆" "ɨ" "ɳ")  ; luẑyṇ
                        "drink"
                        (set :count :everything :verb :positive))

(learn-urwormdwarf-word '("v̪͆" "ɒ̰" "" "ɢ" "ɨ" "ʁ")  ; ẑägyx
                        "touch/feel"
                        (set :count :everything :verb))

(learn-urwormdwarf-word '("ɴ" "ɒ" "" "b" "u" "n̪")  ; ňabun
                        "work"
                        (set :count :everything :verb))

(learn-urwormdwarf-word '("ɢ" "ɨ" "" "v" "ɒ" "n̪")  ; gyvan
                        "have"
                        (set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ɴ" "ɒ:" "" "ɦ" "ɨ" "n̪")  ; ňāhyn
                        "give"
                        (set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ʁ" "ɨ" "" "ɢ" "ɒ" "v̪͆")  ; xygaẑ
                        "go"
                        (set :count :everything :verb))

;;;; Numerals
(learn-urwormdwarf-word '("d̪" "u:" "ɳ")  ; dūṇ
                        "one"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("n̪" "ɒ:" "m")  ; nām
                        "two"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ɴ" "u:" "ʁ")  ; ňūx
                        "three"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("d̪" "ṵ" "n̪")  ; dün
                        "few"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("d̪" "ɒ:" "" "ɦ" "ɒ" "n̪")  ; dāhan
                        "many (assessable)"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("v̪͆" "ɒ̰" "" "b" "ɨ" "ʁ")  ; ẑäbyx
                        "many (not assessable)"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ɢ" "ṵ" "" "m" "ɒ" "ɳ")  ; gümaṇ
                        "much"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("v̪͆" "u:")  ; ẑū
                        "all"
                        (set :count :everything :short))

;;; Pronouns

;;;; Conjunctions
(learn-urwormdwarf-word '("ɴ" "ɨ" "n̪")  ; ňyn
                        "and"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ɮ̪" "u" "ɴ")  ; luň
                        "or (inclusive)"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ɦ" "ɨ" "ɳ")  ; hyṇ
                        "or (exclusive)"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("b" "ɨ:" "m")  ; bȳm
                        "therefore"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("v̪͆" "u" "ʁ")  ; ẑux
                        "when"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("v̪͆" "ɒ:" "n̪")  ; ẑān
                        "before"
                        (set :count :everything :short))

;;;; Adposition
(learn-urwormdwarf-word '("ʁ" "ɨ")  ; xy
                        "near"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ʁ" "ɒ" "v̪͆")  ; xaẑ
                        "of"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("b" "ɒ" "" "ɴ" "ɒ" "ɳ")  ; baňaṇ
                        "towards"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("d̪" "ɨ:" "v̪͆")  ; dȳẑ
                        "above"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("n̪" "u:" "m")  ; nūm
                        "below"
                        (set :count :everything :short))

;;;; Particles

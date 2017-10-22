(in-package #:mang)

(defparameter *urkobold-phonemes*
  (glyph-system (map ('c (set "p" "b" "t" "d" "k" "g" "m" "n" "ŋ" "ɸ" "β" "s"
                              "z" "ʃ" "ʒ" "ɕ" "ʑ" "ɹ" "l"))
                     ('v (set "i" "y" "u" "e" "ɵ" "o" "ə" "a"))
                     ('t (set "p" "t" "k"))
                     ('d (set "b" "d" "g"))
                     ('p (set "p" "b"))
                     ('f (set "ɸ" "ʃ" "s" "ɕ" "β" "z" "ʒ" "ʑ"))
                     ('s (set "ɸ" "ʃ" "s" "ɕ" "ɹ" "l"))
                     ('z (set "β" "z" "ʒ" "ʑ" "ɹ" "l"))
                     ('l (set "ɹ" "l"))
                     ('ll (set "ɻ" "ɭ"))
                     ('r (set "ɳ" "ɻ" "ɭ"))
                     ('n (set "m" "n" "ŋ"))
                     ('tongue (set "^")))))

(defparameter *urkobold-words*
  (word-system (set (list (set 'v
                               `(c v)
                               `(t s v)
                               `(d z v)
                               `(f n v)
                               `(v l n)
                               `(r tongue v)
                               `(p ll tongue v))
                          1 3 (set `(c v)
                                   `(t s v)
                                   `(d z v)
                                   `(f n v)
                                   `(v l n)
                                   `(r tongue v)
                                   `(p ll tongue v)))
                    (list (set `(c v)
                               `(t s v)
                               `(d z v)
                               `(f n v)
                               `(v l n)
                               `(r tongue v)
                               `(p ll tongue v))))
               *urkobold-phonemes*))

(defparameter *urkobold-store*
  (let ((dist (uniform-distribution (glyphs<- *urkobold-phonemes*)))
        (consonants (set ($ (@ (classes<- *urkobold-phonemes*)
                               'c))
                         ""))
        (vowels (set ($ (@ (classes<- *urkobold-phonemes*)
                           'v))
                     "")))
    (store (image (lambda (category template)
                    (values category (learner template dist)))
                  (map
                   (:count
                    (set (match-everything-generator)))
                   (:everything
                    (set (match-outro-generator 3)
                         (match-outro-generator 4 :ignore-glyphs (set ""))))
                   (:noun
                    (set (match-outro-generator 2)
                         (match-outro-generator 3 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:verb
                    (set (match-outro-generator 2)
                         (match-outro-generator 3 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:adjective
                    (set (match-outro-generator 2)
                         (match-outro-generator 3 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:affix
                    (set (match-outro-generator 1)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:suffix
                    (set (match-outro-generator 1)
                         (match-outro-generator 1 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:prefix
                    (set (match-outro-generator 1)
                         (match-outro-generator 1 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:person
                    (set (match-outro-generator 3)
                         (match-outro-generator 3 :ignore-glyphs consonants)
                         (match-outro-generator 4 :ignore-glyphs vowels)))
                   (:animal
                    (set (match-outro-generator 3)
                         (match-outro-generator 3 :ignore-glyphs consonants)
                         (match-outro-generator 4 :ignore-glyphs vowels)))
                   (:plant
                    (set (match-outro-generator 3)
                         (match-outro-generator 3 :ignore-glyphs consonants)
                         (match-outro-generator 4 :ignore-glyphs vowels)))
                   (:object
                    (set (match-outro-generator 3)
                         (match-outro-generator 3 :ignore-glyphs consonants)
                         (match-outro-generator 4 :ignore-glyphs vowels)))
                   (:mass
                    (set (match-outro-generator 3)
                         (match-outro-generator 3 :ignore-glyphs consonants)
                         (match-outro-generator 4 :ignore-glyphs vowels)))
                   (:abstract
                    (set (match-outro-generator 3)
                         (match-outro-generator 3 :ignore-glyphs consonants)
                         (match-outro-generator 4 :ignore-glyphs vowels)))
                   (:good
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:bad
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:funny
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:sad
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:light
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:dark
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:living
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:dead
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:active
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:inert
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:big
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels)))
                   (:small
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 4 :ignore-glyphs consonants)
                         (match-outro-generator 5 :ignore-glyphs vowels))))))))

(defparameter *urkobold-dictionary*
  (dictionary))

(defun learn-urkobold-word (form gloss learn)
  (let ((word (word form)))
    (unless (empty? (@ *urkobold-dictionary* form))
      (warn "Homophones for ~S:~% ~S ~S~%~{~{ ~S~}~}"
            form gloss learn
            (convert 'list
                     (image (lambda (entry)
                              (list (gloss<- entry)
                                    (learn<- entry)))
                            (@ *urkobold-dictionary* form)))))
    (learn-word *urkobold-store* word learn)
    (setf *urkobold-dictionary*
          (with *urkobold-dictionary*
                (dictionary-entry word gloss learn)))))

(learn-urkobold-word '("l" "i" "" "n" "ə")  ; -lin·
                     ;; gender: person
                     ;; suffix
                     "PERSON"
                     (set :count :everything :affix :suffix :person))

(learn-urkobold-word '("a" "n")  ; -an
                     ;; gender: animal
                     ;; suffix
                     "ANIMAL"
                     (set :count :everything :affix :suffix :animal))

(learn-urkobold-word '("ɳ" "^" "u")  ; -n'u
                     ;; gender: plant
                     ;; suffix
                     "PLANT"
                     (set :count :everything :affix :suffix :plant))

(learn-urkobold-word '("o" "k")  ; -ok
                     ;; gender: object
                     ;; suffix
                     "OBJECT"
                     (set :count :everything :affix :suffix :object))

(learn-urkobold-word '("ɵ" "" "ɕ" "a")  ; -öśa
                     ;; gender: mass
                     ;; suffix
                     "MASS"
                     (set :count :everything :affix :suffix :mass))

(learn-urkobold-word '("ɻ" "^" "y")  ; -r'y
                     ;; gender: abstract
                     ;; suffix
                     "ABSTRACT"
                     (set :count :everything :affix :suffix :abstract))

(learn-urkobold-word '("ʃ" "a")  ; ša
                     ;; first person pronoun
                     "PRONOUN.1"
                     (set :count :everything))

(learn-urkobold-word '("ɕ" "a")  ; -śa
                     ;; first person suffix
                     "PRONOUN.1.SUFFIX"
                     (set :count :everything :affix :suffix))

(learn-urkobold-word '("ʒ" "u")  ; žu
                     ;; second person pronoun
                     "PRONOUN.2"
                     (set :count :everything))

(learn-urkobold-word '("ʑ" "u")  ; -źu
                     ;; second person suffix
                     "PRONOUN.2.SUFFIX"
                     (set :count :everything :affix :suffix))

(learn-urkobold-word '("t" "y")  ; ty
                     ;; third person pronoun: person gender
                     "PRONOUN.3.PERSON"
                     (set :count :everything :person))

(learn-urkobold-word '("a" "ɹ" "n")  ; arn
                     ;; third person pronoun: animal gender
                     "PRONOUN.3.ANIMAL"
                     (set :count :everything :animal))

(learn-urkobold-word '("ɳ" "^" "o")  ; n'o
                     ;; third person pronoun: plant gender
                     "PRONOUN.3.PLANT"
                     (set :count :everything :plant))

(learn-urkobold-word '("o" "" "g" "i")  ; ogi
                     ;; third person pronoun: object gender
                     "PRONOUN.3.OBJECT"
                     (set :count :everything :object))

(learn-urkobold-word '("ɹ" "ɵ")  ; rö
                     ;; third person pronoun: mass gender
                     "PRONOUN.3.MASS"
                     (set :count :everything :mass))

(learn-urkobold-word '("p" "ɻ" "^" "o")  ; pr'o
                     ;; third person pronoun: abstract gender
                     "PRONOUN.3.ABSTRACT"
                     (set :count :everything :abstract))

(learn-urkobold-word '("ɹ" "i")  ; -ri
                     ;; third person suffix
                     "PRONOUN.3.SUFFIX"
                     (set :count :everything :affix :suffix))

(learn-urkobold-word '("d" "ɹ" "i")  ; -dri
                     ;; abessive suffix
                     "ABESSIVE"
                     (set :count :everything :affix :suffix :small))

(learn-urkobold-word '("ɸ" "o")  ; -fo
                     ;; plural suffix
                     "PLURAL"
                     (set :count :everything :affix :suffix :big))

(learn-urkobold-word '("k" "ɵ")  ; -kö
                     ;; collective suffix
                     "COLLECTIVE"
                     (set :count :everything :affix :suffix :big))

(learn-urkobold-word '("a" "l" "ŋ")  ; -alň
                     ;; indefinite suffix
                     "INDEFINITE"
                     (set :count :everything :affix :suffix))

(learn-urkobold-word '("ɳ" "^" "a")  ; n'a-
                     ;; benefactive case
                     ;; who is the one something belongs to?
                     ;; prefix
                     "BENEFACTIVE"
                     (set :count :everything :affix :prefix :good))

(learn-urkobold-word '("p" "ɸ" "a")  ; pfa-
                     ;; possessive case
                     ;; what is possessed?
                     ;; prefix
                     "POSSESSIVE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("p" "ɹ" "a")  ; pra
                     ;; inalienable possessive case – prefix
                     ;; what is possessed and can't be taken away?
                     ;; for body parts, parents, children and siblings
                     ;; prefix
                     "INALIENABLE"
                     (set :count :everything :affix :prefix))

#|
(learn (learning-markov (& *urkobold-store*)
                        (set :everything :good)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :good))
       ;; ornative case – prefix
       ;; applies to clothing and worn tools
       ;; the person wears a cap
       ;; ORNATIVE-cap-INDEF-PERSON-PR.3.SUFF BENE-person-OBJECT-PR.3.SUFF
       ;; śi-gežöpšu-n'ö-lin·-r'o             śa-ölndol'eka-pa-r'o
       ;; śigežöpšun'ölin·r'o śaölndol'ekapar'o
       '("ɕ" "i"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :person)
                        :negative (set :count :everything :affix
                                       :animal :plant :object :mass :abstract)
                        :learn (set :count :everything :affix :person))
       ;; vocative case – prefix
       ;; lo-
       '("l" "o"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :practical :object)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :practical :object))
       ;; instrumentive case – prefix
       ;; he uses his feet to go to the hut
       ;; INSTRUMENTIVE-foot-PLURAL-PERSON-PR.3.SUFF PR.3 ALLATIVE-hut-PERSON-PR.3
       ;; r'e-voilm-gly-lin·-r'o                     aly  gi-mufi-lin·-r'o
       ;; r'evoilmglylin·r'o aly gimufilin·r'o
       ;; r'e-
       '("ɻ" "^" "e"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :movement)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :movement))
       ;; perlative case – prefix
       ;; I go through the woods to my friend.
       ;; PERLATIVE-tree-COLL-PERSON-PR.1.SUFF BENE-PR.1-PERSON-PR.3.SUFF ALLATIVE-POSS-friend-PERSON-PR.1.SUFF
       ;; z·-eln-glö-lin·-vi                   śa-fi-lin·-r'o             gi-źi-ilňl'u-lin·-vi
       ;; z·elnglölin·vi śafilin·r'o giźiilňl'ulin·vi
       ;; z·-
       '("z" "ə"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :movement)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :movement))
       ;; allative case – prefix
       ;; I go through the woods to it's friend.
       ;; to my friend I go through the woods
       ;; ALLATIVE-POSS-friend-PERSON-PR.1.SUFF BENE-PR.1-PERSON-PR.3.SUFF PERLATIVE-tree-COLL-PERSON-PR.1.SUFF
       ;; gi-źi-ilňl'u-lin·-vi                  śa-fi-lin·-r'o             z·-eln-glö-lin·-vi
       ;; giźiilňl'ulin·vi śafilin·r'o z·elnglölin·vi
       ;; gi-
       '("g" "i"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :bad)
                        :negative (set :count :everything :affix
                                       :good :funny)
                        :learn (set :count :everything :affix :bad))
       ;; aversive case – prefix
       ;; you go to the tree avoiding the stone
       ;; avoiding the stone to the tree you go
       ;; AVERSIVE-stone(obj)-PERSON-PR.2.SUFF ALLATIVE-tree-PERSON-PR.2.SUFF PR.2
       ;; ulň-pśopl'egy-lin·-ksu               gi-eln-lin·-ksu                gzu
       ;; ulňpśopl'egylin·kse gielnlin·ksu gzu
       ;; ulň-
       '("u" "l" "ŋ"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :movement)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :movement))
       ;; egressive case – prefix
       ;; It [an animal] moves from the woods to the plains
       ;; from the woods it goes to the plains
       ;; EGRESSIVE-tree-COLL-ANIMAL-PR.3.SUFF PR.3.ANIMAL ALLATIVE-plain(terrain)-ANIMAL-PR.3.SUFF
       ;; n'o-eln-glö-ölm-r'o                  ogži        gi-l'ekal'a-ölm-r'o
       ;; n'oelnglöölmr'o ogži gil'ekal'aölmr'o
       ;; n'o-
       '("n" "^" "o"))


(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :good :beautiful :light :person)
                        :negative (set :verb :adjective :bad :ugly :dark)
                        :learn
                        (set :count :everything :noun :good :beautiful :light
                             :person))
       ;; sun – person gender
       ;; gźal'a
       '("g" "ʑ" "a" "" "ɭ" "^" "a"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :good :beautiful :dark :person)
                        :negative (set :verb :adjective :bad :ugly :light)
                        :learn
                        (set :count :everything :noun :good :beautiful :dark
                             :person))
       ;; moon – person gender
       ;; orn
       '("o" "ɹ" "n"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :person :funny)
                        :negative (set :verb :adjective :bad :ugly :sad)
                        :learn (set :count :everything :noun :person))
       ;; person – person gender
       ;; ölndol'eka
       '("ɵ" "l" "n" "" "d" "o" "" "ɭ" "^" "e" "" "k" "a"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :person :good :funny :abstract)
                        :negative (set :verb :adjective :bad :sad)
                        :learn (set :count :everything :noun :person :good))
       ;; friend – person gender
       ;; ilňl'u
       '("i" "l" "ŋ" "" "ɭ" "^" "u"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :plant)
                        :negative (set :verb :adjective)
                        :learn (set :count :everything :noun :plant))
       ;; plant – plant gender
       ;; pl'a
       '("p" "ɭ" "^" "a"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :plant)
                        :negative (set :verb :adjective :bad :ugly)
                        :learn (set :count :everything :noun :plant))
       ;; tree – plant gender
       ;; eln
       '("e" "l" "n"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :object)
                        :negative (set :verb :adjective)
                        :learn (set :count :everything :noun :object))
       ;; stone (the object) – object gender
       ;; pśopl'egy
       '("p" "ɕ" "o" "" "p" "ɭ" "^" "e" "" "g" "y"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :object :good :dark :practical)
                        :negative (set :verb :adjective :bad)
                        :learn (set :count :everything :noun :object
                                    :good :dark :practical))
       ;; hut – object gender
       ;; mufi
       '("m" "u" "" "ɸ" "i"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :object :person :practical)
                        :negative (set :verb :adjective :bad :ugly :sad)
                        :learn (set :count :everything
                                    :noun :object :practical))
       ;; arm – object gender
       ;; ilňgžo
       '("i" "l" "ŋ" "" "g" "ʒ" "o"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :object :person :movement)
                        :negative (set :verb :adjective :bad :ugly :sad)
                        :learn (set :count :everything
                                    :noun :object :movement))
       ;; foot – object gender
       ;; voilm
       '("β" "o" "" "i" "l" "m"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :object :practical)
                        :negative (set :verb :adjective)
                        :learn (set :count :everything :object :practical))
       ;; cap (clothing) – object gender
       ;; gežöpšu
       '("g" "e" "" "ʒ" "ɵ" "" "p" "ʃ" "u"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :mass :practical)
                        :negative (set :verb :adjective :bad)
                        :learn (set :count :everything :noun :mass :practical))
       ;; earth(ground)/soil – mass gender
       ;; l'öd·
       '("ɭ" "^" "ɵ" "" "d" "ə"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :mass :bad)
                        :negative (set :verb :adjective :practical)
                        :learn (set :count :everything :noun :mass :bad))
       ;; stone(ground) – mass gender
       ;; pšepl'ete
       '("p" "ʃ" "e" "" "p" "ɭ" "^" "e" "" "t" "e"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :abstract :movement)
                        :negative (set :verb :adjective :object)
                        :learn (set :count :everything :noun :abstract :movement))
       ;; air – abstract gender
       ;; šufepše
       '("ʃ" "u" "" "ɸ" "e" "" "p" "ʃ" "e"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :abstract :good :movement)
                        :negative (set :verb :adjective :funny)
                        :learn (set :count :everything :abstract :good))
       ;; plain(terrain) – abstract gender
       ;; l'ekal'a
       '("ɭ" "^" "e" "" "k" "a" "" "ɭ" "^" "a"))
|#

(in-package #:mang)

(defparameter *urkobold-phonemes*
  (glyph-system (map ('c (set "p" "b" "t" "d" "k" "g" "m" "n" "ŋ" "ɸ" "β" "s"
                              "z" "ʃ" "ʒ" "ɕ" "ʑ" "ɹ" "l"))
                     ('v (set "i" "y" "u" "e" "ɵ" "o" "ə" "a"))
                     ('t (set "p" "t" "k"))
                     ('d (set "b" "d" "g"))
                     ('p (set "p" "b"))
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
                               `(v l n)
                               `(r tongue v)
                               `(p ll tongue v))
                          1 3 (set `(c v)
                                   `(t s v)
                                   `(d z v)
                                   `(v l n)
                                   `(r tongue v)
                                   `(p ll tongue v)))
                    (list (set `(c v)
                               `(t s v)
                               `(d z v)
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
                         (match-outro-generator 5 :ignore-glyphs vowels))))))))

(defparameter *urkobold-dictionary*
  (dictionary))

(let ((word (word '("l" "i" "" "n" "ə")))
      ;; -lin·
      (learn (set :count :everything :affix :person)))
  ;; gender: person
  ;; suffix
  (learn-word *urkobold-store* word learn)
  (setf *urkobold-dictionary*
        (with *urkobold-dictionary*
              (dictionary-entry word "PERSON" learn))))

(let ((word (word '("a" "n")))
      ;; -an
      (learn (set :count :everything :affix :animal)))
  ;; gender: animal
  ;; suffix
  (learn-word *urkobold-store* word learn)
  (setf *urkobold-dictionary*
        (with *urkobold-dictionary*
              (dictionary-entry word "ANIMAL" learn))))

#|
(learn (learning-markov (& *urkobold-store*)
                        (set :everything)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :animal))
       ;; animal gender – suffix on agreement
       ;; -ölm
       '("ɵ" "l" "m"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :plant)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :plant))
       ;; plant gender – suffix on agreement
       ;; plant belonging to person
       ;; POSS-plant-PERSON-PR.3.SUFF BENE-person-PLANT-PR.3.SUFF
       ;; źi-pl'a-lin·-r'o            śa-ölndol'eka-·rň-r'o
       ;; źipl'alin·r'o śaölndol'eka·rňr'o
       ;; -·rň
       '("ə" "ɹ" "ŋ"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :object)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :object))
       ;; object gender – suffix on agreement
       ;; -pa
       '("p" "a"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :mass)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :mass))
       ;; mass gender – suffix on agreement
       ;; -tro
       '("t" "ɹ" "o"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :abstract)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :abstract))
       ;; abstract gender – suffix on agreement
       ;; -ši
       '("ʃ" "i"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :person)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.1
       ;; fi
       '("ɸ" "i"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :person)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.1.SUFF
       ;; vi
       '("β" "i"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :person)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.2
       ;; gzu
       '("g" "z" "u"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :person)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.2.SUFF
       ;; ksu
       '("k" "s" "u"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :person)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.3.PERSON
       ;; aly
       '("a" "" "l" "y"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :animal)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.3.ANIMAL
       ;; ogži
       '("o" "" "g" "ʒ" "i"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :plant)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.3.PLANT
       ;; apl'o
       '("a" "" "p" "ɭ" "^" "o"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :object)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.3.OBJECT
       ;; eorn
       '("e" "" "o" "ɹ" "n"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :mass)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.3.MASS
       ;; oso
       '("o" "" "s" "o"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :person :animal :plant :object :mass)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; PR.3.SUFF
       ;; r'o
       '("ɻ" "^" "o"))

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

(learn (learning-markov (& *urkobold-store*)
                        (set :everything)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; plural – suffix
       ;; trees
       ;; tree-PL
       ;; eln-gly
       ;; -gly
       '("g" "l" "y"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; collective – suffix
       ;; woods
       ;; tree-COLL
       ;; eln-glö
       ;; -glö
       '("g" "l" "ɵ"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :bad))
       ;; abessive – suffix
       ;; no tree
       ;; tree-ABESS
       ;; eln-pše
       ;; -pše
       '("p" "ʃ" "e"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; indefinite marking – suffix
       ;; the null marking is the definite
       ;; a tree
       ;; tree-INDEF
       ;; eln-n'ö
       ;; -n'ö
       '("ɳ" "^" "ɵ"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :good)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :good))
       ;; benefactive case – prefix
       ;; plant belonging to person
       ;; POSS-plant-PERSON-PR.3.SUFF BENE-person-PLANT-PR.3.SUFF
       ;; źi-pl'a-lin·-r'o            śa-ölndol'eka-·rň-r'o
       ;; źipl'alin·r'o śaölndol'eka·rṇr'o
       ;; śa-
       '("ɕ" "a"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :practical)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix :practical))
       ;; possesive case – prefix
       ;; plant belonging to person
       ;; POSS-plant-PERSON-PR.3.SUFF BENE-person-PLANT-PR.3.SUFF
       ;; źi-pl'a-lin·-r'o            śa-ölndol'eka-·rň-r'o
       ;; źipl'alin·r'o śaölndol'eka·rňr'o
       ;; źi-
       '("ʑ" "i"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :person :good)
                        :negative (set :count :everything :affix)
                        :learn (set :count :everything :affix))
       ;; inalienable possesive case – prefix
       ;; applies to body parts, parents and children
       ;; person's arm
       ;; IPOSS-arm-PERSON-PR.3.SUFF BENE-person-OBJECT-PR.3.SUFF
       ;; mi-ilňgžo-lin·-r'o         śa-ölndol'eka-pa-r'o
       ;; miilňgžolin·r'o śaölndol'ekapar'o
       '("m" "i"))

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
|#

;;; plant belonging to person
;;; POSS-plant-PERSON-PR.3.SUFF BENE-person-PLANT
;;; źi-pl'a-lin·-r'o            śa-ölndol'eka-·rň
;;; źipl'alin·r'o śaölndol'eka·rn

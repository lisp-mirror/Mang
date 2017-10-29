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
                               `(f ,(set 'n 'l)
                                   v)
                               `(v ,(set 'l nil)
                                   n)
                               `(r tongue v)
                               `(p ll tongue v))
                          1 2 (set `(c v)
                                   `(t s v)
                                   `(d z v)
                                   `(f n v)
                                   `(v ,(set 'l nil)
                                       n)
                                   `(r tongue v)
                                   `(p ll tongue v)))
                    (list (set `(c v)
                               `(t s v)
                               `(d z v)
                               `(f ,(set 'n 'l)
                                   v)
                               `(v ,(set 'l nil)
                                   n)
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
    (unless (run-dfsm *urkobold-words* form)
      (error "Form ~S is not a valid urkobold word"
             form))
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

;;;; Cases
(learn-urkobold-word '("m" "e")  ; me-
                     ;; causative
                     ;; prefix
                     "CAUSATIVE"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("n" "ɵ")  ; nö-
                     ;; resultive
                     ;; prefix
                     "RESULTIVE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɭ" "^" "e")  ; l'e-
                     ;; adessive
                     ;; prefix
                     "ADESSIVE"
                     (set :count :everything :affix :prefix :inert))

(learn-urkobold-word '("e" "l" "m")  ; elm-
                     ;; ablative
                     ;; prefix
                     "ABLATIVE"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("i" "l" "m")  ; ilm-
                     ;; egressive
                     ;; prefix
                     "EGRESSIVE"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("l" "e")  ; le-
                     ;; lative
                     ;; prefix
                     "LATIV"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("ʑ" "n" "a")  ; źna-
                     ;; perlative
                     ;; prefix
                     "PERLATIVE"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("ə" "l" "n")  ; ·eln-
                     ;; aversive
                     ;; prefix
                     "AVERSIVE"
                     (set :count :everything :affix :prefix :active :bad))

(learn-urkobold-word '("n" "o")  ; no-
                     ;; possessive
                     ;; prefix
                     "POSSESSIVE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɳ" "^" "o")  ; n'o-
                     ;; inalienable possessive
                     ;; prefix
                     "POSSESSIVE(I)"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("l" "o")  ; lo-
                     ;; ornative
                     ;; prefix
                     "ORNATIVE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("n" "a")  ; na-
                     ;; possessor
                     ;; prefix
                     "POSSESSOR"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɳ" "^" "a")  ; n'a-
                     ;; inalienable possessor
                     ;; prefix
                     "POSSESSOR(I)"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("l" "a")  ; la-
                     ;; benefactive
                     ;; prefix
                     "BENEFAKTIVE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("t" "e")  ; te-
                     ;; instrumental
                     ;; prefix
                     "INSTRUMENTAL"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("d" "a")  ; da-
                     ;; instructive
                     ;; prefix
                     "INSTRUCTIVE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɕ" "a")  ; śa-
                     ;; comitative
                     ;; prefix
                     "COMITATIVE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("z" "e")  ; ze-
                     ;; sociative
                     ;; prefix
                     "SOCIATIVE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɹ" "u")  ; ru-
                     ;; abessive
                     ;; prefix
                     "ABESSIVE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("a" "ɹ" "m")  ; arm-
                     ;; adverbial
                     ;; prefix
                     "ADVERBIAL"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("k" "ɵ")  ; kö-
                     ;; translative"
                     ;; prefix
                     "TRANSLATIVE"
                     (set :count :everything :affix :prefix))

;;;; genders
(learn-urkobold-word '("ɸ" "l" "ə")  ; -fl·
                     ;; person gender
                     ;; suffix
                     "PERSON"
                     (set :count :everything :affix :suffix :person :living))

(learn-urkobold-word '("a" "ŋ")  ; -aň
                     ;; animal gender
                     ;; suffix
                     "ANIMAL"
                     (set :count :everything :affix :suffix :animal :living))

(learn-urkobold-word '("y")  ; -y
                     ;; plant gender
                     ;; suffix
                     "PLANT"
                     (set :count :everything :affix :suffix :plant :living))

(learn-urkobold-word '("g" "e")  ; -ge
                     ;; object gender
                     ;; suffix
                     "OBJECT"
                     (set :count :everything :affix :suffix :object))

(learn-urkobold-word '("n" "ɵ")  ; -nö
                     ;; mass gender
                     ;; suffix
                     "MASS"
                     (set :count :everything :affix :suffix :mass))

(learn-urkobold-word '("p" "ɻ" "^" "a")  ; -pr'a
                     ;; abstract gender
                     ;; suffix
                     "ABSTRACT"
                     (set :count :everything :affix :suffix :abstract))

;;;; personal pronouns
(learn-urkobold-word '("ɸ" "ə")  ; f·
                     "1s"
                     (set :count :everything :person))

(learn-urkobold-word '("β" "e")  ; ve
                     "2s"
                     (set :count :everything :person))

(learn-urkobold-word '("β" "a")  ; va
                     "3s.PERSON"
                     (set :count :everything :person))

(learn-urkobold-word '("ŋ" "a")  ; ňa
                     "3s.ANIMAL"
                     (set :count :everything :animal))

(learn-urkobold-word '("n" "y")  ; ny
                     "3s.PLANT"
                     (set :count :everything :plant))

(learn-urkobold-word '("k" "a")  ; ka
                     "3s.OBJECT"
                     (set :count :everything :object))

(learn-urkobold-word '("β" "ɵ")  ; vö
                     "3s.MASS"
                     (set :count :everything :mass))

(learn-urkobold-word '("p" "a")  ; pa
                     "3s.ABSTRACT"
                     (set :count :everything :abstract))

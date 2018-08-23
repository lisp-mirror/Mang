(in-package #:mang)

(defparameter *urkobold-phonemes*
  (glyph-system (map ('c (set "p" "b" "t" "d" "k" "g" "m" "n" "ŋ" "ɸ" "β" "s"
                              "z" "ʃ" "ʒ" "ɕ" "ʑ" "ɹ" "l"))
                     ('v (set "i" "y" "u" "e" "ɞ" "o" "ə" "a"))
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
                     ('tongue (set "¡")))))

(defparameter *urkobold-romanization*
  (map ("p" "p")
       ("b" "b")
       ("t" "t")
       ("d" "d")
       ("k" "k")
       ("g" "g")
       ("m" "m")
       ("n" "n")
       ("ŋ" "ň")
       ("ɸ" "f")
       ("β" "v")
       ("s" "s")
       ("z" "z")
       ("ʃ" "š")
       ("ʒ" "ž")
       ("ɕ" "ś")
       ("ʑ" "ź")
       ("ɹ" "r")
       ("l" "l")
       ("ɳ" "n")
       ("ɻ" "r")
       ("ɭ" "l")
       ("i" "i")
       ("y" "y")
       ("u" "u")
       ("e" "e")
       ("ɞ" "ö")
       ("o" "o")
       ("ə" "·")
       ("a" "a")
       ("¡" "'")))

(defparameter *urkobold-words*
  (word-system (set (list (set 'v
                               `(tongue v)
                               `(c v)
                               `(t s v)
                               `(d z v)
                               `(f ,(set 'n 'l)
                                   v)
                               `(v ,(set 'l nil)
                                   n)
                               `(r tongue v)
                               `(p ll tongue v))
                          1 3 (set `(c v)
                                   `(t s v)
                                   `(d z v)
                                   `(f ,(set 'n 'l)
                                       v)
                                   `(v ,(set 'l nil)
                                       n)
                                   `(n ,(set 'l '(ll tongue))
                                       v)
                                   `(r tongue v)
                                   `(p ll tongue v)))
                    (list `(,(set nil 'c 'tongue
                                  `(t s)
                                  `(d z)
                                  `(f ,(set 'n 'l))
                                  `(r tongue)
                                  `(p ll tongue))
                             v
                             ,(set nil 'c `(l n)))))
               *urkobold-phonemes*))

(defparameter *urkobold-store*
  (let ((dist (union (yule-distribution '("l" "n" "m" "ɕ" "ɹ" "ŋ" "k" "t" "g"
                                          "ʑ" "β" "ʃ" "ʒ" "d" "ɸ" "s" "p" "z"
                                          "b" "")
                                        19 1.02 1.03)
                     (yule-distribution '("o" "i" "a" "e" "y" "ə" "u" "ɞ" "¡"
                                          "")
                                        8 1.01 1.04)))
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
                    (set (match-outro-generator 4)
                         (match-outro-generator 3 :ignore-glyphs (set ""))))
                   (:noun
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:verb
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:interjection
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:adjective
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:affix
                    (set (match-outro-generator 2)
                         (match-outro-generator 1 :ignore-glyphs (set ""))
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
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:animal
                    (set (match-outro-generator 3)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:body-part
                    (set (match-outro-generator 3)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:plant
                    (set (match-outro-generator 3)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:object
                    (set (match-outro-generator 3)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:mass
                    (set (match-outro-generator 3)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:abstract
                    (set (match-outro-generator 3)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:food
                    (set (match-outro-generator 3)
                         (match-outro-generator 4)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:sense
                    (set (match-outro-generator 3)
                         (match-outro-generator 4)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:sound
                    (set (match-outro-generator 3)
                         (match-outro-generator 4)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:smell
                    (set (match-outro-generator 3)
                         (match-outro-generator 4)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:sight
                    (set (match-outro-generator 3)
                         (match-outro-generator 4)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:tactile
                    (set (match-outro-generator 3)
                         (match-outro-generator 4)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:taste
                    (set (match-outro-generator 3)
                         (match-outro-generator 4)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:emotion
                    (set (match-outro-generator 3)
                         (match-outro-generator 4)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:good
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:bad
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:funny
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:sad
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:light
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:dark
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:living
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:dead
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:active
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:inert
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:big
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels)))
                   (:small
                    (set (match-outro-generator 4)
                         (match-outro-generator 5)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 2 :ignore-glyphs vowels))))))))

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

(defun generate-urkobold-word (categories &optional (negative (set)))
  (generate-word *urkobold-words* *urkobold-store* categories
                 :negative negative))

(defun print-urkobold-words (n categories
                             &optional (negative (set)))
  (let ((words (set)))
    (loop :for x :below n
       :do
       (let ((word (generate-urkobold-word categories negative)))
	 (when (and (empty? (@ *urkobold-dictionary*
			       (form<- word)))
		    (not (@ words word)))
	   (setf words
		 (with words word))
	   (format t "~S ~A~%"
		   (form<- word)
		   (string<-word (image (lambda (glyph)
					  (or (@ *urkobold-romanization* glyph)
					      glyph))
					(form<- word)))))))))

;;;; Affixes
(learn-urkobold-word '("b" "ɭ" "¡" "a")  ; -bl'a
                     ;; collective
                     ;; suffix
                     "COL"
                     (set :count :everything :affix :suffix))

(learn-urkobold-word '("p" "ɭ" "¡" "i")  ; -pl'i
                     ;; plural
                     ;; suffix
                     "PL"
                     (set :count :everything :affix :suffix))

(learn-urkobold-word '("m" "e")  ; me-
                     ;; agent
                     ;; prefix
                     "AGE"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("n" "ɞ")  ; nö-
                     ;; patient
                     ;; prefix
                     "PAT"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("a" "ŋ")  ; aň-
                     ;; intentional
                     ;; prefix
                     "INTEN"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɭ" "¡" "e")  ; l'e-
                     ;; adessive
                     ;; prefix
                     "ADE"
                     (set :count :everything :affix :prefix :inert))

(learn-urkobold-word '("e" "l" "m")  ; elm-
                     ;; ablative
                     ;; prefix
                     "ABL"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("i" "l" "m")  ; ilm-
                     ;; egressive
                     ;; prefix
                     "EGRE"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("l" "e")  ; le-
                     ;; lative
                     ;; prefix
                     "LAT"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("ʑ" "n" "a")  ; źna-
                     ;; perlative
                     ;; prefix
                     "PER"
                     (set :count :everything :affix :prefix :active))

(learn-urkobold-word '("ə" "l" "n")  ; ·eln-
                     ;; aversive
                     ;; prefix
                     "AVRS"
                     (set :count :everything :affix :prefix :active :bad))

(learn-urkobold-word '("n" "o")  ; no-
                     ;; alienable possessive
                     ;; prefix
                     "POSS.AL"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɳ" "¡" "o")  ; n'o-
                     ;; inalienable possessive
                     ;; prefix
                     "POSS.INAL"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("l" "o")  ; lo-
                     ;; momentary possessive
                     ;; prefix
                     "POSS.MOM"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("n" "a")  ; na-
                     ;; alienable possessor
                     ;; prefix
                     "POS.AL"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɳ" "¡" "a")  ; n'a-
                     ;; inalienable possessor
                     ;; prefix
                     "POS.INAL"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("l" "a")  ; la-
                     ;; momentary possessor
                     ;; prefix
                     "POS.MOM"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("t" "e")  ; te-
                     ;; instrumental
                     ;; prefix
                     "INS"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("d" "a")  ; da-
                     ;; instructive
                     ;; prefix
                     "INSC"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɕ" "a")  ; śa-
                     ;; comitative
                     ;; prefix
                     "COM"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("z" "e")  ; ze-
                     ;; sociative
                     ;; prefix
                     "SOC"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("ɹ" "u")  ; ru-
                     ;; abessive
                     ;; prefix
                     "ABE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("a" "ɹ" "m")  ; arm-
                     ;; adjectival agent
                     ;; prefix
                     "ADJ.AGE"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("s" "u")  ; su-
                     ;; adjectival patient
                     ;; prefix
                     "ADJ.PAT"
                     (set :count :everything :affix :prefix))

(learn-urkobold-word '("k" "ɞ")  ; kö-
                     ;; translative"
                     ;; prefix
                     "TRANSL"
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

(learn-urkobold-word '("n" "ɞ")  ; -nö
                     ;; mass gender
                     ;; suffix
                     "MASS"
                     (set :count :everything :affix :suffix :mass))

(learn-urkobold-word '("p" "ɻ" "¡" "a")  ; -pr'a
                     ;; abstract gender
                     ;; suffix
                     "ABSTRACT"
                     (set :count :everything :affix :suffix :abstract))

;;;; roots
(learn-urkobold-word '("b" "ɭ" "¡" "ə" "" "ɕ" "i")  ; bl'·śi
                     "able"
                     (set :count :everything :adjective
                          :good :living :active))

(learn-urkobold-word '("o" "ŋ" "" "a" "n")  ; oňan
                     "angry"
                     (set :count :everything :adjective
                          :emotion :bad :active))

(learn-urkobold-word '("o" "" "ŋ" "a" "" "ʃ" "o")  ; oňašo
                     "animal"
                     (set :count :everything :noun
                          :animal :living :active))

(learn-urkobold-word '("d" "a" "" "ɕ" "ə")  ; daś·
                     "arm"
                     (set :count :everything :noun
                          :body-part :tactile :living :active))

(learn-urkobold-word '("ʒ" "e" "" "p" "a")  ; žepa
                     "bad"
                     (set :count :everything :adjective
                          :bad))

(learn-urkobold-word '("g" "z" "a" "" "ɭ" "¡" "o")  ; gzal'o
                     "bitter"
                     (set :count :everything :adjective
                          :food :test :bad))

(learn-urkobold-word '("m" "a" "" "z" "ŋ" "ɞ")  ; mazňö
                     "bowl"
                     (set :count :everything :noun
                          :object :inert :big))

(learn-urkobold-word '("k" "o" "" "ɭ" "¡" "i" "" "d" "e")  ; kol'ide
                     "person"
                     (set :count :everything :noun
                          :person :living :active))

(learn-urkobold-word '("g" "β" "a" "" "β" "e" "" "k" "o")  ; gvaveko
                     "rare/uncommon"
                     (set :count :everything :adjective
                          :small))

(learn-urkobold-word '("l" "a" "" "n" "i")  ; lani
                     "raw"
                     (set :count :everything :adjective
                          :food))

(learn-urkobold-word '("o" "m" "" "k" "a")  ; omka
                     "big"
                     (set :count :everything :adjective
                          :big))

(learn-urkobold-word '("ɕ" "m" "i" "" "k" "ɕ" "o")  ; śmikśo
                     "ripe"
                     (set :count :everything :adjective
                          :plant :food :taste :good :living))

(learn-urkobold-word '("p" "l" "y" "" "ʑ" "a")  ; plyźa
                     "river"
                     (set :count :everything :noun
                          :mass :good :active :big))

(learn-urkobold-word '("u" "l" "ŋ" "" "s" "ɹ" "ə")  ; ulňsr·
                     "rotten"
                     (set :count :everything :adjective
                          :food :taste :bad :dead :inert))

(learn-urkobold-word '("l" "e" "" "ɹ" "y" "" "g" "o")  ; lerygo
                     "sad"
                     (set :count :everything :adjective
                          :emotion :bad :sad :inert))

(learn-urkobold-word '("β" "i" "" "d" "ʒ" "e")  ; vidže
                     "salty"
                     (set :count :everything :adjective
                          :food :sense :taste))

(learn-urkobold-word '("ŋ" "ə" "" "d" "o")  ; ň·do
                     "shadow"
                     (set :count :everything :noun
                          :mass :sight :dark))

(learn-urkobold-word '("i" "" "p" "ɭ" "¡" "a")  ; ipl'a
                     "seed"
                     (set :count :everything :noun
                          :plant :good :living :inert :small))

(learn-urkobold-word '("ʑ" "ŋ" "y" "" "d" "ʒ" "ə")  ; źňydž·
                     "sex"
                     (set :count :everything :noun
                          :abstract :sense :tactile :emotion :good :funny
                          :living :active))

(learn-urkobold-word '("ɳ" "¡" "o" "" "y" "m")  ; n'oym
                     "sibling"
                     (set :count :everything :noun
                          :person :good :living))

(learn-urkobold-word '("ʑ" "a" "" "ə" "n")  ; źa·n
                     "sky"
                     (set :count :everything :noun
                          :abstract :light :big))

(learn-urkobold-word '("a" "" "ɭ" "¡" "a")  ; al'a
                     "small"
                     (set :count :everything :adjective
                          :small))

(learn-urkobold-word '("t" "ɕ" "a" "" "ɻ" "¡" "o")  ; tśar'o
                     "sour"
                     (set :count :everything :adjective
                          :food :sense :taste :funny))

(learn-urkobold-word '("d" "ʑ" "e" "" "k" "ʃ" "e")  ; dźekše
                     "spice"
                     (set :count :everything :noun
                          :mass :food :smell :taste :good))

(learn-urkobold-word '("ɕ" "l" "i" "" "ɭ" "¡" "a")  ; ślil'a
                     "spicy"
                     (set :count :everything :adjective
                          :food :sense :taste :active))

(learn-urkobold-word '("t" "a" "" "β" "a")  ; tava
                     "sun"
                     (set :count :everything :noun
                          :person :sight :good :light :active))

(learn-urkobold-word '("z" "m" "a" "" "t" "ɕ" "a")  ; zmatśa
                     "sweet"
                     (set :count :everything :adjective
                          :food :sense :taste :good :funny :inert))

(learn-urkobold-word '("o" "" "ʑ" "ŋ" "ə" "" "a" "ŋ")  ; oźň·aň
                     "testicle"
                     (set :count :everything :noun
                          :body-part :tactile :funny :living :inert))

(learn-urkobold-word '("ɳ" "¡" "ə" "" "t" "ʃ" "ɞ")  ; n'·tšö
                     "thing"
                     (set :count :everything :noun
                          :object :inert))

(learn-urkobold-word '("g" "a" "" "ŋ" "u")  ; gaňu
                     "throat"
                     (set :count :everything :noun
                          :body-part :sound :emotion))

(learn-urkobold-word '("ɭ" "¡" "a" "" "l" "o")  ; l'alo
                     "tongue"
                     (set :count :everything :noun
                          :body-part :food :sense :sound :taste :emotion
                          :funny))

(learn-urkobold-word '("ɹ" "o" "" "ɹ" "ə")  ; ror·
                     "tool"
                     (set :count :everything :noun
                          :object :good :dead :active :inert))

(learn-urkobold-word '("b" "o" "" "ɹ" "ə")  ; bor·
                     "torso"
                     (set :count :everything :noun
                          :body-part :emotion :good :funny :living))

(learn-urkobold-word '("g" "l" "y" "" "o" "l" "m")  ; glyolm
                     "moon"
                     (set :count :everything :noun
                          :person :sight :emotion :good :light :dark :living))

(learn-urkobold-word '("y" "l" "m")  ; ylm
                     "tree"
                     (set :count :everything :noun
                          :plant :good :dark :living :inert :big))

(learn-urkobold-word '("a" "" "l" "u" "" "ʑ" "a")  ; aluźa
                     "vulva"
                     (set :count :everything :noun
                          :body-part :sense :smell :tactile :taste :emotion
                          :good :funny :living :active :inert :small))

(learn-urkobold-word '("p" "l" "i" "" "ɕ" "n" "o")  ; pliśno
                     "water"
                     (set :count :everything :noun
                          :mass :food :good :active))

(learn-urkobold-word '("m" "o" "" "l" "o")  ; molo
                     "word"
                     (set :count :everything :noun
                          :abstract :sound :active))

(learn-urkobold-word '("l" "e" "" "t" "ɸ" "y")  ; letfy
                     "wound"
                     (set :count :everything :noun
                          :object :tactile :bad :sad))

(learn-urkobold-word '("β" "e")  ; ve
                     "you"
                     (set :count :everything :affix))

(learn-urkobold-word '("p" "ʃ" "a" "" "i" "m")  ; pšaim
                     "ocean"
                     (set :count :everything :noun
                          :mass :sound :dead :active :big))

(learn-urkobold-word '("p" "ʃ" "a" "" "ʃ" "ɹ" "o")  ; pšašro
                     "parent"
                     (set :count :everything :noun
                          :person :good :big))

(learn-urkobold-word '("a" "" "o" "m" "" "ɹ" "a")  ; aomra
                     "penis"
                     (set :count :everything :noun
                          :body-part :sense :smell :tactile :taste :emotion
                          :good :funny :living :active :inert :small))

(learn-urkobold-word '("g" "ɹ" "o" "" "n" "ə")  ; gron·
                     "neck"
                     (set :count :everything :noun
                          :body-part :sound))

(learn-urkobold-word '("m" "a" "" "n" "o" "" "t" "i")  ; manoti
                     "willow"
                     (set :count :everything :noun
                          :plant :emotion :good :sad :living :active))

(learn-urkobold-word '("ɹ" "a" "" "k" "s" "a")  ; raksa
                     "achoo"
                     (set :count :everything :interjection
                          :sound :bad :funny))

(learn-urkobold-word '("n" "a" "" "ʒ" "l" "a")  ; nažla
                     "old"
                     (set :count :everything :adjective))

(learn-urkobold-word '("o" "m" "" "n" "e")  ; omne
                     "dark"
                     (set :count :everything :adjective
                          :sight :dark))

(learn-urkobold-word '("s" "e" "" "k" "ɕ" "o")  ; sekśo
                     "nice"
                     (set :count :everything :adjective
                          :emotion :good))

(learn-urkobold-word '("m" "ɞ" "" "k" "i")  ; möki
                     "nipple"
                     (set :count :everything :noun
                          :body-part :tactile :emotion :good :funny :inert
                          :small))

(learn-urkobold-word '("b" "ɭ" "¡" "a" "k")  ; bl'ak
                     "Nuss"
                     (set :count :everything :noun
                          :plant :food :inert :small))

(learn-urkobold-word '("b" "ɻ" "¡" "o" "" "g" "ʒ" "ə")  ; br'ogž·
                     "hill/mountain"
                     (set :count :everything :noun
                          :object :inert :big))

(learn-urkobold-word '("m" "u" "" "p" "a")  ; mupa
                     "mouth"
                     (set :count :everything :noun
                          :body-part :sense :taste :emotion :active))

(learn-urkobold-word '("l" "i" "" "ɸ" "i")  ; lifi
                     "low"
                     (set :count :everything :adjective
                          :small))

(learn-urkobold-word '("ə" "" "m" "o" "" "a" "n")  ; ·moan
                     "man"
                     (set :count :everything :noun
                          :person))

(learn-urkobold-word '("g" "ʒ" "y" "" "ɕ" "i")  ; gžyśi
                     "moist"
                     (set :count :everything :adjective
                          :tactile))

(learn-urkobold-word '("o" "ɹ" "m" "" "ŋ" "u")  ; ormňu
                     "bread"
                     (set :count :everything :noun
                          :object :food :inert))

(learn-urkobold-word '("ɸ" "ə" "" "l" "a" "" "y" "n")  ; f·layn
                     "woman"
                     (set :count :everything :noun
                          :person))

(learn-urkobold-word '("a" "" "m" "a" "" "l" "i")  ; amali
                     "breast"
                     (set :count :everything :noun
                          :body-part :food :sense :tactile :taste :emotion :good
                          :funny))

(learn-urkobold-word '("p" "ɭ" "¡" "ɞ" "" "ŋ" "ɞ")  ; pl'öňö
                     "butt"
                     (set :count :everything :noun
                          :body-part :sense :smell :tactile :funny))

(learn-urkobold-word '("ŋ" "u" "" "ɹ" "o")  ; ňuro
                     "calm"
                     (set :count :everything :noun
                          :abstract :emotion :inert))

(learn-urkobold-word '("g" "a" "" "ʃ" "a")  ; gaša
                     "child/young person"
                     (set :count :everything :noun
                          :person :small))

(learn-urkobold-word '("g" "ʒ" "u" "" "t" "u")  ; gžutu
                     "child/offspring"
                     (set :count :everything :noun
                          :person :emotion :good :small))

(learn-urkobold-word '("ɸ" "l" "a" "" "ŋ" "a")  ; flaňa
                     "cloud"
                     (set :count :everything :noun
                          :mass :dark :active :big))

(learn-urkobold-word '("t" "ɕ" "e" "" "ɕ" "ɹ" "e")  ; tśeśre
                     "coast"
                     (set :count :everything :noun
                          :abstract :sound :sight :big))

(learn-urkobold-word '("ŋ" "a" "" "i" "m")  ; ňaim
                     "corpse"
                     (set :count :everything :noun
                          :person :small :bad :sad :dead :inert))

(learn-urkobold-word '("ɕ" "ɹ" "a" "" "k" "i")  ; śraki
                     "cut"
                     (set :count :everything :adjective
                          :tactile))

(learn-urkobold-word '("ʑ" "a" "" "ɹ" "o")  ; źaro
                     "ear/gill"
                     (set :count :everything :noun
                          :body-part :sense :sound))

(learn-urkobold-word '("g" "o" "" "ɭ" "¡" "i")  ; gol'i
                     "eye"
                     (set :count :everything :noun
                          :body-part :sense :sight))

(learn-urkobold-word '("k" "ɕ" "a" "" "d" "e")  ; kśade
                     "fast"
                     (set :count :everything :adjective
                          :active))

(learn-urkobold-word '("ɕ" "l" "a" "" "ɹ" "o")  ; ślaro
                     "fire"
                     (set :count :everything :noun
                          :mass :sight :tactile :light :active))

(learn-urkobold-word '("ʃ" "e" "" "ɕ" "o")  ; šeśo
                     "fish"
                     (set :count :everything :noun
                          :animal :food :funny :living))

(learn-urkobold-word '("k" "a" "" "ʃ" "m" "o")  ; kašmo
                     "meat/flesh"
                     (set :count :everything :noun
                          :mass :food :good :active))

(learn-urkobold-word '("k" "ʃ" "a")  ; kša
                     "wolf"
                     (set :count :everything :noun
                          :animal :bad :active))

(learn-urkobold-word '("g" "ɞ" "" "s" "a")  ; gösa
                     "light"
                     (set :count :everything :adjective
                          :sense :tactile :small))

(learn-urkobold-word '("d" "ʒ" "u" "" "i" "n")  ; džuin
                     "life"
                     (set :count :everything :noun
                          :abstract :good :funny :living :active))

(learn-urkobold-word '("t" "o" "" "ʃ" "ə")  ; toš·
                     "leg"
                     (set :count :everything :noun
                          :body-part :active))

(learn-urkobold-word '("p" "ɕ" "u" "" "ɳ" "¡" "e")  ; pśun'e
                     "lake"
                     (set :count :everything :noun
                          :mass :inert :big))

(learn-urkobold-word '("n" "a" "" "ŋ" "ɹ" "i")  ; naňri
                     "knife"
                     (set :count :everything :noun
                          :object :food :tactile))

(learn-urkobold-word '("a" "n" "" "ʑ" "i")  ; anźi
                     "world"
                     (set :count :everything :noun
                          :mass :big))

(learn-urkobold-word '("l" "o" "" "ŋ" "o")  ; loňo
                     "nothing"
                     (set :count :everything :noun
                          :abstract :dark :inert))

;;; goat
;;;
;;; physical:
;;; brightly colored
;;; porcupine quills
;;;
;;; behaviour:
;;; notices and avoids ambushes and traps
;;; collect materials to build artificial hills to climb on
(learn-urkobold-word '("o" "" "ŋ" "a" "" "k" "ə")  ; oňak·
                     "goat"
                     (set :count :everything :noun
                          :animal :funny :living :active))

(learn-urkobold-word '("g" "o" "" "ʑ" "l" "a" "" "i" "n")  ; goźlain
                     "fishing rod"
                     (set :count :everything :noun
                          :object :food))

(learn-urkobold-word '("ŋ" "a" "l" "ŋ")  ; ňalň
                     "mossy rock"
                     (set :count :everything :noun
                          :plant :living :inert))

(learn-urkobold-word '("m" "a")  ; ma
                     "what"
                     (set :count :everything :noun
                          :abstract))

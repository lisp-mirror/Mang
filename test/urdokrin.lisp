(in-package #:mang)

(defparameter *urdokrin-phonemes*
  (glyph-system (map ('c (set "m" "ɲ" "ɴ" "p" "pʋ" "pj" "pɰ" "c" "cʋ" "cj" "cɰ"
                              "q" "qʋ" "qj" "qɰ" "ɸ" "ç" "χ" "ʋ" "j" "ɰ"))
                     ('e (set "m" "ɲ" "ɴ" "p" "c" "q" "ɸ" "ç" "χ" "ʋ" "j" "ɰ"))
                     ('v (set "i" "u" "o" "ɛ" "a" "ɒ")))))

(defparameter *urdokrin-words*
  (word-system (list 0 3 `(c v)
                     `(c v ,(set 'e nil)))
               *urdokrin-phonemes*))

(defparameter *urdokrin-store*
  (let ((dist (union (yule-distribution '("c" "ç" "j" "ɰ" "m" "p" "q" "ɲ" "ʋ"
                                          "ɸ" "ɴ" "χ" "qʋ" "pɰ" "pj" "qj" "cʋ"
                                          "cɰ" "pʋ" "cj" "qɰ" "")
                                        100 1.09 1.06)
                     (yule-distribution '("i" "ɛ" "a" "ɒ" "o" "u" "")
                                        60 1.1 1.04)))
        (consonants (set ($ (@ (classes<- *urdokrin-phonemes*)
                               'c))
                         ""))
        (vowels (set ($ (@ (classes<- *urdokrin-phonemes*)
                           'v))
                     "")))
    (store (image (lambda (category template)
                    (values category (learner template dist)))
                  (map
                   (:count
                    (set (match-everything-generator)))
                   (:everything
                    (set (match-outro-generator 4)
                         (match-outro-generator 2 :ignore-glyphs consonants)))
                   (:noun
                    (set (match-outro-generator 4 :ignore-glyphs (set ""))
                         (match-outro-generator 3)))
                   (:verb
                    (set (match-outro-generator 4 :ignore-glyphs (set ""))
                         (match-outro-generator 3)))
                   (:mobile
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:material
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:immobile
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:positive
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:negative
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:mysterious
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   )))))

(defparameter *urdokrin-dictionary*
  (dictionary))

(defparameter *urdokrin-romanization*
  (map ("m" "m")
       ("ɲ" "n")
       ("ɴ" "ň")
       ("p" "p")
       ("c" "t")
       ("q" "k")
       ("ɸ" "f")
       ("ç" "h")
       ("χ" "x")
       ("ʋ" "v")
       ("j" "j")
       ("ɰ" "r")
       ("i" "i")
       ("u" "u")
       ("o" "o")
       ("ɛ" "e")
       ("a" "a")
       ("ɒ" "å")
       ("pʋ" "pv")
       ("pj" "pj")
       ("pɰ" "pr")
       ("cʋ" "tv")
       ("cj" "tj")
       ("cɰ" "tr")
       ("qʋ" "kv")
       ("qj" "kj")
       ("qɰ" "kr")))

(defun learn-urdokrin-word (form gloss learn)
  (let ((word (word form)))
    (unless (run-dfsm *urdokrin-words* form)
      (error "Form ~S is not a valid urdokrin word"
             form))
    (unless (empty? (@ *urdokrin-dictionary* form))
      (warn "Homophones for ~S:~% ~S ~S~%~{~{ ~S~}~}"
            form gloss learn
            (convert 'list
                     (image (lambda (entry)
                              (list (gloss<- entry)
                                    (learn<- entry)))
                            (@ *urdokrin-dictionary* form)))))
    (learn-word *urdokrin-store* word learn)
    (setf *urdokrin-dictionary*
          (with *urdokrin-dictionary*
                (dictionary-entry word gloss learn)))))

(defun generate-urdokrin-word (categories &optional (negative (set)))
  (generate-word *urdokrin-words* *urdokrin-store* categories
                 :negative negative))


(defun print-urdokrin-words (n categories
                             &optional (negative (set)))
  (let ((words (set)))
    (loop :for x :below n
       :do
       (let ((word (generate-urdokrin-word categories negative)))
	 (when (and (empty? (@ *urdokrin-dictionary*
			       (form<- word)))
		    (not (@ words (form<- word))))
	   (setf words
		 (with words (form<- word)))
	   (format t "~S ~A~%"
		   (form<- word)
		   (string<-word (image (lambda (glyph)
					  (or (@ *urdokrin-romanization* glyph)
					      glyph))
					(form<- word)))))))))

;;;; roots
(learn-urdokrin-word '("c" "ɒ" "" "qɰ" "i" "ɲ")  ; tåkrin
                     "person"
                     (set :count :everything :noun :mobile))

(learn-urdokrin-word '("m" "ɒ" "" "c" "ɒ" "q")  ; måtåk
                     "corpse"
                     (set :count :everything :noun :immobile :negative))

(learn-urdokrin-word '("j" "i" "" "q" "ɒ" "ʋ")  ; jikåv
                     "fresh cadaver"
                     (set :count :everything :noun :immobile))

(learn-urdokrin-word '("ɴ" "ɒ" "" "cʋ" "i" "j")  ; ňåtvij
                     "ripe cadaver"
                     (set :count :everything :noun :immobile :positive))

(learn-urdokrin-word '("p" "a" "" "m" "u" "c")  ; pamut
                     "rotten cadaver"
                     (set :count :everything :noun :immobile :negative))

(learn-urdokrin-word '("ɴ" "i" "" "q" "i" "ɸ")  ; ňikif
                     "poisonous cadaver"
                     (set :count :everything :noun :immobile :negative))

(learn-urdokrin-word '("m" "a" "" "j" "ɛ" "" "p" "ɒ" "c")  ; majepåt
                     "elephant"
                     (set :count :everything :noun :mobile :mysterious))

(learn-urdokrin-word '("ɰ" "a" "" "c" "a" "q")  ; ratak
                     "snake"
                     (set :count :everything :noun :mobile :negative
                          :mysterious))

(learn-urdokrin-word '("q" "i" "" "pʋ" "ɒ" "q")  ; kipvåk
                     "tree"
                     (set :count :everything :noun :immobile))

(learn-urdokrin-word '("cɰ" "o")  ; tro
                     "this"
                     (set :count :everything))

(learn-urdokrin-word '("qʋ" "a" "c")  ; kvat
                     "that"
                     (set :count :everything))

(learn-urdokrin-word '("ç" "i" "" "q" "a" "c")  ; hikat
                     "fresh water"
                     (set :count :everything :noun :mobile :material :positive))

(learn-urdokrin-word '("j" "ɒ" "" "c" "i" "q")  ; jåtik
                     "stagnant water"
                     (set :count :everything :noun :material :negative))

(learn-urdokrin-word '("ç" "a" "ɴ")  ; haň
                     "eat"
                     (set :count :everything :verb :positive))

(learn-urdokrin-word '("ç" "i" "c")  ; hit
                     "drink"
                     (set :count :everything :verb :positive))

(learn-urdokrin-word '("q" "a" "p")  ; kap
                     "bite"
                     (set :count :everything :verb :negative))

(learn-urdokrin-word '("ɰ" "a" "" "ɲ" "ɒ")  ; ranå
                     "sleep"
                     (set :count :everything :verb :immobile))

(learn-urdokrin-word '("ç" "ɒ" "" "c" "a" "p")  ; håtap
                     "walk"
                     (set :count :everything :verb :mobile))

(learn-urdokrin-word '("c" "ɒ" "" "q" "a")  ; tåka
                     "sit"
                     (set :count :everything :verb :immobile :positive))

(learn-urdokrin-word '("χ" "a" "p")  ; xap
                     "fire"
                     (set :count :everything :noun :mobile :mysterious))

(learn-urdokrin-word '("qʋ" "a" "ç")  ; kvah
                     "danger"
                     (set :count :everything :noun :negative))

(learn-urdokrin-word '("m" "a" "" "cj" "u" "c")  ; matjut
                     "animal"
                     (set :count :everything :noun :mobile))

(learn-urdokrin-word '("m" "ɛ" "c")  ; met
                     "dangerous animal"
                     (set :count :everything :noun :mobile :negative))

(learn-urdokrin-word '("j" "a" "" "χ" "a" "ç")  ; jaxah
                     "plant"
                     (set :count :everything :noun :immobile))

(learn-urdokrin-word '("m" "ɒ" "" "ɸ" "ɒ" "" "ʋ" "a" "m")  ; måfåvam
                     "fern"
                     (set :count :everything :noun :immobile))

(learn-urdokrin-word '("c" "a" "" "p" "a" "" "ç" "a")  ; tapaha
                     "spice"
                     (set :count :everything :noun :immobile))

(learn-urdokrin-word '("χ" "ɒ" "j")  ; xåj
                     "meat"
                     (set :count :everything :noun :immobile :positive))

(learn-urdokrin-word '("j" "o" "" "c" "ɒ" "" "q" "a" "m")  ; jotåkam
                     "like"
                     (set :count :everything :verb :positive))

(learn-urdokrin-word '("ç" "a" "" "c" "a")  ; hata
                     "love"
                     (set :count :everything :verb :positive))

(learn-urdokrin-word '("q" "ɒ" "" "c" "a" "j")  ; kåtaj
                     "hate"
                     (set :count :everything :verb :negative))

(learn-urdokrin-word '("j" "ɒ" "" "c" "a" "p")  ; jåtap
                     "have"
                     (set :count :everything :verb))

(learn-urdokrin-word '("c" "ɒ" "" "q" "a" "p")  ; tåkap
                     "give"
                     (set :count :everything :verb))

(learn-urdokrin-word '("c" "a" "" "m" "a" "ɰ")  ; tamar
                     "body"
                     (set :count :everything :noun))

(learn-urdokrin-word '("m" "a" "" "cɰ" "ɒ" "ʋ")  ; matråv
                     "head"
                     (set :count :everything :noun))

(learn-urdokrin-word '("ɴ" "ɒ" "c")  ; ňåt
                     "neck"
                     (set :count :everything :noun))

(learn-urdokrin-word '("qj" "ɒ" "" "ʋ" "ɒ" "j")  ; kjåvåj
                     "chest"
                     (set :count :everything :noun))

(learn-urdokrin-word '("c" "a" "" "p" "o" "χ")  ; tapox
                     "belly"
                     (set :count :everything :noun))

(learn-urdokrin-word '("c" "a" "" "m" "ɛ" "j")  ; tamej
                     "arm"
                     (set :count :everything :noun))

(learn-urdokrin-word '("m" "ɒ" "j")  ; måj
                     "hand"
                     (set :count :everything :noun))

(learn-urdokrin-word '("pɰ" "i" "ɲ")  ; prin
                     "finger"
                     (set :count :everything :noun))

(learn-urdokrin-word '("pɰ" "ɒ" "" "ç" "a" "m")  ; pråham
                     "leg"
                     (set :count :everything :noun :mobile))

(learn-urdokrin-word '("cj" "ɒ" "" "pʋ" "a" "c")  ; tjåpvat
                     "foot"
                     (set :count :everything :noun :mobile))

(learn-urdokrin-word '("p" "a" "" "p" "ɒ" "" "c" "a")  ; papåta
                     "toe"
                     (set :count :everything :noun))

(learn-urdokrin-word '("p" "i" "ɰ")  ; pir
                     "fingernail/toenail"
                     (set :count :everything :noun))

(learn-urdokrin-word '("ɴ" "a" "ç")  ; ňah
                     "tongue"
                     (set :count :everything :noun))

(learn-urdokrin-word '("q" "a" "" "m" "a")  ; kama
                     "ground"
                     (set :count :everything :noun :material :immobile))

(learn-urdokrin-word '("q" "a" "" "ɰ" "ɒ" "j")  ; karåj
                     "be.red"
                     (set :count :everything :verb :material))

(learn-urdokrin-word '("j" "ɒ" "" "q" "a" "ç")  ; jåkah
                     "be.blue"
                     (set :count :everything :verb :material))

(learn-urdokrin-word '("qɰ" "a" "" "j" "a")  ; kraja
                     "be.black"
                     (set :count :everything :verb :material))

(learn-urdokrin-word '("m" "a" "" "χ" "a" "j")  ; maxaj
                     "be.white"
                     (set :count :everything :verb :material))

(learn-urdokrin-word '("j" "a" "" "ɸ" "ɛ" "q")  ; jafek
                     "take"
                     (set :count :everything :verb))

(learn-urdokrin-word '("q" "u" "c")  ; kut
                     "need"
                     (set :count :everything :verb :negative))

(learn-urdokrin-word '("c" "i" "q")  ; tik
                     "see"
                     (set :count :everything :verb))

(learn-urdokrin-word '("j" "ɛ" "c")  ; jet
                     "do"
                     (set :count :everything :verb))

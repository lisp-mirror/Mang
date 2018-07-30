(in-package #:mang)

;;;; Dwarf data
(defparameter *urwormdwarf-phonemes*
  (glyph-system (map ('consonant (set "m" "n̪" "ɳ" "ɴ" "b" "d̪" "ɢ" "v" "ɦ̪͆" "ð"
                                      "ʐ" "ʁ" "ɦ" "r" "ɮ̪" "bβ" "ɖʐ" "ɢʁ"))
                     ('begin (set "m" "n̪" "ɴ" "b" "d̪" "ɢ" "ɦ̪͆" "ð" "ʐ" "ʁ" "ɦ"
				  "ɮ̪" "r" "ɖʐ" "ɢʁ"))
                     ('middle (set "m" "ɳ" "ɴ" "b" "d̪" "ɢ" "v" "ɦ̪͆" "ð" "ʐ" "ʁ"
				   "ɦ" "ɮ̪" "r" "ð" "bβ" "ɖʐ"))
                     ('end (set "m" "n̪" "ɳ" "ɴ" "ɦ̪͆" "ʁ"))
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
       ("ɦ̪͆" "ẑ")
       ("ð" "z")
       ("ʐ" "ž")
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
       ("ɒ̰" "ä")
       ("bβ" "bv")
       ("ɖʐ" "dž")
       ("ɢʁ" "gx")))

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
  (let ((dist (union (yule-distribution '("d̪" "m" "ʁ" "ɦ̪͆" "r" "ɢ" "b"
					  "n̪" "ɴ" "ð" "v" "ɖʐ" "ɳ" "ʐ"
					  "ɢʁ" "ɦ" "bβ" "ɮ̪" "")
					18 1.07 1.02)
		     (yule-distribution '("ɒ:" "ɒ" "u:" "ɒ̰" "ɨ" "u"
					  "ɨ:" "ṵ" "ɨ̰" "")
					9 1.02 1.07))))
    (store (image (lambda (category template)
                    (values category (learner template dist)))
                  (map
                   (:count
                    (set (match-everything-generator)))
                   (:everything
                    (set (match-outro-generator 4
                                                :ignore-glyphs (set
								""))
                         (match-outro-generator 3)
                         (match-outro-generator 2
                                                :ignore-glyphs (set
								""))
			 (match-outro-generator
			  2 :ignore-glyphs (set "ɒ:" "ɒ" "u:" "ɒ̰" "ɨ"
						"u" "ɨ:" "ṵ" "ɨ̰" ""))
			 (match-outro-generator
			  1 :ignore-glyphs (set "d̪" "m" "ʁ" "ɦ̪͆" "r"
						"ɢ" "b" "n̪" "ɴ" "ð"
						"v" "ɖʐ" "ɳ" "ʐ" "ɢʁ"
						"ɦ" "bβ" "ɮ̪" ""))))
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
                                                :ignore-glyphs (set
                                                                ""))))
                   (:dangerous
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

(defun print-urwormdwarf-words (n categories
				&optional (negative (set)))
  (let ((words (set)))
    (loop :for x :below n
       :do
       (let ((word (generate-urwormdwarf-word categories negative)))
	 (when (and (empty? (@ *urwormdwarf-dictionary*
			       (form<- word)))
		    (not (@ words word)))
	   (setf words
		 (with words word))
	   (format t "~S ~A~%"
		   (form<- word)
		   (string<-word (image (lambda (glyph)
					  (or (@ *urwormdwarf-romanization* glyph)
					      glyph))
					(form<- word)))))))))

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

(learn-urwormdwarf-word '("ɴ" "ɒ:" "" "ɦ̪͆" "ɨ" "ɦ̪͆")  ; ňāẑyẑ
                        "word"
                        (set :count :everything :noun))

(learn-urwormdwarf-word '("m" "ɨ" "" "d̪" "ɨ" "ɳ")  ; mydyṇ
			"now"
			(set :count :everything :noun))

(learn-urwormdwarf-word '("n̪" "ɨ" "" "ð" "ɨ" "ɦ̪͆")  ; nyzyẑ
			"importance"
			(set :count :everything :noun :positive))

(learn-urwormdwarf-word '("ɢʁ" "ɒ" "n̪")  ; gxan
			"gravel"
			(set :count :everything :noun))

(learn-urwormdwarf-word '("d̪" "ɨ" "" "ɦ" "u" "ɦ̪͆")  ; dyhuẑ
			"mother"
			(set :count :everything :noun :positive))

(learn-urwormdwarf-word '("ʐ" "ɨ:" "" "ð" "ɒ" "ɴ")  ; žȳzaň
			"intention"
			(set :count :everything :noun))

(learn-urwormdwarf-word '("d̪" "ɨ" "" "ʐ" "u" "" "ɦ" "ɨ" "m")  ; dyžuhym
			"perfection"
			(set :count :everything :noun :positive))

(learn-urwormdwarf-word '("d̪" "ɨ" "" "ɮ̪" "ɒ" "ɦ̪͆")  ; dylaẑ
			"the good"
			(set :count :everything :noun :positive))

(learn-urwormdwarf-word '("m" "ɨ" "" "ɴ" "ɒ" "ɴ")  ; myňaň
			"mushroom"
			(set :count :everything :noun :positive))

(learn-urwormdwarf-word '("m" "ɒ" "" "m" "ɒ" "" "ʐ" "ɒ" "ɴ")  ; mamažaň
                        "beetle"
                        (set :count :everything :noun :positive))

;;; beetle
;;;
;;; physical:
;;; half the size of a wormdwarf
;;; has antlers
;;;
;;; behavior:
;;; eats rock
;;; makes false lairs filled with fake rock eggs
(learn-urwormdwarf-word '("ɦ̪͆" "ɨ" "" "m" "ɒ" "ɦ̪͆")  ; ẑymaẑ
                        "horned beetle"
                        (set :count :everything :noun :dangerous))

;;;; Verbs
(learn-urwormdwarf-word '("ʁ" "ɨ" "ɳ")  ; xyṇ
                        "be"
                        (set :count :everything :verb :short))

(learn-urwormdwarf-word '("ɖʐ" "ɒ:" "" "ʁ" "ɒ" "ɦ̪͆")  ; džāxaẑ
			"be wrong"
			(set :count :everything :verb :negative))

(learn-urwormdwarf-word '("m" "u:" "" "b" "ɨ")  ; mūby
                        "live"
                        (set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ɢ" "ɒ" "" "d̪" "ɨ" "ɴ")  ; gadyň
                        "die"
                        (set :count :everything :verb :negative))

(learn-urwormdwarf-word '("m" "ɒ̰" "ʁ")  ; mäx
                        "quake"
                        (set :count :everything :verb :negative :dangerous))

(learn-urwormdwarf-word '("r" "ɒ̰" "" "ɦ" "ɒ" "m")  ; räham
                        "hear"
                        (set :count :everything :verb))

(learn-urwormdwarf-word '("ɦ̪͆" "u:" "" "ɳ" "ɒ" "" "ɦ" "ɨ" "ɴ")  ; ẑūṇahyň
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

(learn-urwormdwarf-word '("ɮ̪" "u" "" "ɦ̪͆" "ɨ" "ɳ")  ; luẑyṇ
                        "drink"
                        (set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ɦ̪͆" "ɒ̰" "" "ɢ" "ɨ" "ʁ")  ; ẑägyx
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

(learn-urwormdwarf-word '("ʁ" "ɨ" "" "ɢ" "ɒ" "ɦ̪͆")  ; xygaẑ
                        "go"
                        (set :count :everything :verb))

(learn-urwormdwarf-word '("ɮ̪" "ṵ" "ɴ")  ; lüň
			"can"
			(set :count :everything :verb :short :positive))

(learn-urwormdwarf-word '("ɮ̪" "ɒ̰" "" "m" "ɨ" "ɦ̪͆")  ; lämyẑ
			"help"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ʁ" "u" "" "m" "ɨ" "ɦ̪͆")  ; xumyẑ
			"make/work"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ɴ" "ɒ:" "" "v" "ɒ" "n̪")  ; ňāvan
			"begin"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɦ̪͆" "ɒ̰" "m")  ; ẑäm
			"bite"
			(set :count :everything :verb :negative :dangerous))

(learn-urwormdwarf-word '("ʐ" "u:" "" "ɦ" "u" "ɦ̪͆")  ; žūhuẑ
			"breathe"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("n̪" "ɨ:" "" "ɢ" "u" "" "ɴ" "ɨ" "m")  ; nȳguňym
			"choose"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ʁ" "ɨ:" "" "ɮ̪" "ɨ" "ʁ")  ; xȳlyx
			"clean"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("m" "ɨ:" "" "b" "ɨ" "ɴ")  ; mȳbyň
			"cry"
			(set :count :everything :verb :negative))

(learn-urwormdwarf-word '("m" "ɨ:" "" "ɦ" "ɒ")  ; mȳha
			"be sad"
			(set :count :everything :verb :negative))

(learn-urwormdwarf-word '("r" "u" "ʁ")  ; rux
			"fail"
			(set :count :everything :verb :negative :dangerous))

(learn-urwormdwarf-word '("ʐ" "ɒ:" "" "b" "ɨ" "ɳ")  ; žābyṇ
			"find"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("r" "u" "" "ʐ" "u" "ɴ")  ; ružuň
			"finish"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ɦ̪͆" "u:" "" "ɴ" "ɨ")  ; ẑūňy
			"hold"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɦ" "ɨ" "m")  ; hym
			"laugh"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("m" "u:" "" "d̪" "ɨ" "" "m" "ɨ:")  ; mūdymȳ
			"like"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("r" "u:" "" "ɴ" "ɨ" "ʁ")  ; rūňyx
			"meet"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ɮ̪" "ɒ̰" "" "ʁ" "ɒ" "ʁ")  ; läxax
			"promise"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("b" "ɒ̰" "" "ɢ" "u" "ɦ̪͆")  ; bäguẑ
			"beat/hit/punch"
			(set :count :everything :verb :negative :dangerous))

(learn-urwormdwarf-word '("ʐ" "ɒ̰" "" "ʐ" "ɒ" "ɳ")  ; žäžaṇ
			"search"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("m" "ɨ̰" "" "ð" "ɨ" "ɦ̪͆")  ; mÿzyẑ
			"sleep"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɮ̪" "ɒ:" "" "ɴ" "ɨ" "m")  ; lāňym
			"succeed"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("r" "ɒ̰" "" "b" "ɒ" "n̪")  ; räban
			"talk"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɮ̪" "ɒ:" "" "b" "ɨ" "m")  ; lābym
			"taste"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ð" "ɨ" "n̪")  ; zyn
			"use"
			(set :count :everything :verb :short))

(learn-urwormdwarf-word '("ʁ" "ɨ:" "" "ɮ̪" "u" "" "ð" "ɨ" "n̪")  ; xȳluzyn
			"answer"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ʐ" "ɒ" "" "r" "ɒ")  ; žara
			"ask"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɴ" "ɒ:" "" "m" "ɨ" "n̪")  ; ňāmyn
			"arrive"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ʁ" "ɒ:" "" "ɴ" "ɒ" "" "v" "u:" "m")  ; xāňavūm
			"avoid"
			(set :count :everything :verb :negative :dangerous))

(learn-urwormdwarf-word '("ɢ" "ɒ̰" "" "bβ" "ɨ" "ʁ")  ; gäbvyx
			"wait"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɖʐ" "ɒ:" "ʁ")  ; džāx
			"become"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɢ" "u:" "" "ɦ" "ɨ" "ʁ")  ; gūhyx
			"build"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ʁ" "ṵ" "" "m" "ɒ" "ɦ̪͆")  ; xümaẑ
			"call (use name)"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɖʐ" "ɒ" "ʁ")  ; džax
			"grow"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("m" "ṵ" "" "ɦ" "ɨ" "m")  ; mühym
			"remember"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("ɴ" "ɨ" "" "ɦ̪͆" "ɨ" "m")  ; ňyẑym
			"be cold"
			(set :count :everything :verb :negative))

(learn-urwormdwarf-word '("ɢ" "ɒ" "" "ɮ̪" "ɒ" "m")  ; galam
			"be dry/thirsty"
			(set :count :everything :verb :negative))

(learn-urwormdwarf-word '("ɦ" "ɨ" "" "m" "ɒ" "ɦ̪͆")  ; hymaẑ
			"be hungry"
			(set :count :everything :verb :negative))

(learn-urwormdwarf-word '("ɮ̪" "u" "" "ʁ" "ɒ" "ɳ")  ; luxaṇ
			"burn"
			(set :count :everything :verb :dangerous))

(learn-urwormdwarf-word '("ʐ" "ɒ" "" "b" "u" "ʁ")  ; žabux
			"carry"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɖʐ" "ɨ̰" "ʁ")  ; džÿx
			"cough"
			(set :count :everything :verb :negative))

(learn-urwormdwarf-word '("ɢʁ" "ṵ" "" "ɖʐ" "ɨ")  ; gxüdžy
			"cut"
			(set :count :everything :verb :dangerous))

(learn-urwormdwarf-word '("ɢ" "ɨ" "" "d̪" "ɨ" "ɴ")  ; gydyň
			"dig"
			(set :count :everything :verb :positive))

(learn-urwormdwarf-word '("r" "ɒ̰" "m")  ; räm
			"feel pain"
			(set :count :everything :verb :negative))

(learn-urwormdwarf-word '("b" "u" "" "bβ" "u" "n̪")  ; bubvun
			"fill"
			(set :count :everything :verb))

(learn-urwormdwarf-word '("ɢ" "ɒ" "" "ɖʐ" "ɒ" "ʁ")  ; gadžax
			"hate"
			(set :count :everything :verb :negative))

(learn-urwormdwarf-word '("r" "ɒ̰" "" "ɖʐ" "ɒ" "n̪")  ; rädžan
			"oppose"
			(set :count :everything :verb :negative))

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

(learn-urwormdwarf-word '("ɦ̪͆" "ɒ̰" "" "b" "ɨ" "ʁ")  ; ẑäbyx
                        "many (not assessable)"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ɢ" "ṵ" "" "m" "ɒ" "ɳ")  ; gümaṇ
                        "much"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ɦ̪͆" "u:")  ; ẑū
                        "all"
                        (set :count :everything :short))

;;; Pronouns
(learn-urwormdwarf-word '("m" "ɒ:")  ; mā
			"1s"
			(set :count :everything :short))

(learn-urwormdwarf-word '("ɢ" "ɒ" "ɳ")  ; gaṇ
			"2s"
			(set :count :everything :short))

(learn-urwormdwarf-word '("ɮ̪" "u:" "ɴ")  ; lūň
			"3s"
			(set :count :everything :short))

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

(learn-urwormdwarf-word '("ɦ̪͆" "u" "ʁ")  ; ẑux
                        "when"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ɦ̪͆" "ɒ:" "n̪")  ; ẑān
                        "before"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ʁ" "ɒ" "ɳ")  ; xaṇ
			"which"
			(set :count :everything :short))

;;;; Adposition
(learn-urwormdwarf-word '("ʁ" "ɨ")  ; xy
                        "near"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("b" "ɒ" "" "ɴ" "ɒ" "ɳ")  ; baňaṇ
                        "towards"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("d̪" "ɨ:" "ɦ̪͆")  ; dȳẑ
                        "above"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("n̪" "u:" "m")  ; nūm
                        "below"
                        (set :count :everything :short))

(learn-urwormdwarf-word '("ʁ" "u" "ɦ̪͆")  ; xuẑ
			"through"
			(set :count :everything :short))

(learn-urwormdwarf-word '("n̪" "ɨ̰" "ɦ̪͆")  ; nÿẑ
			"at/beside"
			(set :count :everything :short))

(learn-urwormdwarf-word '("b" "u" "ʁ")  ; bux
			"between"
			(set :count :everything :short))

(learn-urwormdwarf-word '("r" "u" "ɴ")  ; ruň
			"until"
			(set :count :everything :short))

(learn-urwormdwarf-word '("ɢ" "ɨ")  ; gy
			"with"
			(set :count :everything :short :positive))

(learn-urwormdwarf-word '("n̪" "ɒ̰" "ɦ̪͆")  ; näẑ
			"without"
			(set :count :everything :short :negative))

(learn-urwormdwarf-word '("ɦ" "ɒ" "n̪")  ; han
			"of/genitive"
			(set :count :everything :short))

;;;; Particles
(learn-urwormdwarf-word '("d̪" "u" "ɳ")  ; duṇ
			"OPP"
			(set :count :everything :short :negative))

(learn-urwormdwarf-word '("ɦ̪͆" "ɨ" "ʁ")  ; ẑyx
			"ADJ"
			(set :count :everything :short))

(learn-urwormdwarf-word '("d̪" "u:" "m")  ; dūm
			"QU"
			(set :count :everything :short))

(learn-urwormdwarf-word '("m" "ɒ̰" "ɦ̪͆")  ; mäẑ
			"something"
			(set :count :everything :short))

(learn-urwormdwarf-word '("ʁ" "ɒ:" "ɦ̪͆")  ; xāẑ
			"would"
			(set :count :everything :short))

(learn-urwormdwarf-word '("r" "ɒ" "m")  ; ram
			"VOC"
			(set :count :everything :short))

;;;; Example sentences

;;; Hello
;;; VOC 2
;;; ram gaṇ
;;; rɒm ɢɒɳ

;;; How are you?
;;; feel.emot 2   QU   something
;;; mūmym     gaṇ dūm  mäẑ
;;; mu:mɨm    ɢɒɳ d̪u:m mɒ̰ɦ̪͆

;;; I am called Mushroom Finder
;;; call   1   VOC-find -mushroom
;;; xümaẑ  mā  ram-žābyṇ-myňaň
;;; ʁṵmɒɦ̪͆ mɒ: rɒmʐɒ:bɨɳmɨɴɒɴ

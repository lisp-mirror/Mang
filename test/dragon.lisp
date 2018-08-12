(in-package #:mang)

(defparameter *dragon-phonemes*
  (glyph-system (map ('c (set "ɳ" "n" "ŋ" "t" "tʼ" "d" "ʈ" "ʈʼ" "ɖ" "k" "g" "ʔ"
                              "θ" "ð" "s" "z" "ʂ" "ʐ" "ɕ" "ʑ" "x" "ʀ" "h" "h̪͆"
                              "ɦ̪͆" "t͡s" "ʈʂ" "k͡x" "ɬ" "ɮ"))
                     ('p (set "t" "d" "ʈ" "ɖ" "q" "ɢ"))
                     ('l (set "ʀ" "ɬ" "ɮ"))
                     ('n (set "ɳ" "ȵ" "ŋ"))
                     ('f (set "θ" "ð" "s" "z" "ʂ" "ʐ" "ɕ" "ʑ" "χ" "ʀ"
                              "h" "h̪͆" "ɦ̪͆"))
                     ('v (set "i" "ɯ" "e̞" "a")))))

(defparameter *dragon-words*
  (word-system (list `(,(set 'c '(f n)
                             '(p l)
                             nil)
                        v)
                     0 3 `(,(set 'c '(f n)
                                 '(p l))
                            v))
               *dragon-phonemes*))

(defparameter *dragon-store*
  (let ((dist (yule-distribution '("e̞" "a" "ɯ" "i" "x" "h̪͆" "θ" "t" "ʂ" "ɕ" "h"
                                   "k" "s" "ŋ" "ʔ" "ð" "tʼ" "t̪θ" "ʈʂ" "ʀ" "ʐ"
                                   "ʈʼ" "n" "d" "ɦ̪͆" "ɖ" "ɳ" "g" "ʈ" "z" "ʑ" "k͡x"
                                   "t͡ɕ" "t͡s" "ɬ" "ɮ" "")
                                 100 1.04 1.03))
        (consonants (set ($ (@ (classes<- *dragon-phonemes*)
                               'c))
                         ""))
        (vowels (set ($ (@ (classes<- *dragon-phonemes*)
                           'v))
                     "")))
    (store (image (lambda (category template)
                    (values category (learner template dist)))
                  (map
                   (:count
                    (set (match-everything-generator)))
                   (:everything
                    (set (match-everything-generator)
                         (match-outro-generator 3 :ignore-glyphs (set ""))
                         (match-outro-generator 2)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:affix
                    (set (match-everything-generator)
                         (match-outro-generator 3 :ignore-glyphs (set ""))
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 2)
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:noun
                    (set (match-outro-generator 4)
                         (match-outro-generator 3 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:verb
                    (set (match-outro-generator 4)
                         (match-outro-generator 3 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:adjective
                    (set (match-outro-generator 4)
                         (match-outro-generator 3 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:positive
                    (set (match-outro-generator 3)
                         (match-outro-generator 2)))
                   (:negative
                    (set (match-outro-generator 3)
                         (match-outro-generator 2)))
                   (:interesting
                    (set (match-outro-generator 3)
                         (match-outro-generator 2)))
                   (:boring
                    (set (match-outro-generator 3)
                         (match-outro-generator 2)))
                   )))))

(defparameter *dragon-dictionary*
  (dictionary))

(defparameter *dragon-romanization*
  (map ("n" "Ν")
       ("ɳ" "Ν̬")
       ("ŋ" "Ν̱")
       ("t" "Τ")
       ("tʼ" "Τ̣")
       ("d" "Δ")
       ("ʈ" "Ͳ")
       ("ʈʼ" "Ͳ̣")
       ("ɖ" "Δ̬")
       ("k" "Κ")
       ("g" "Γ")
       ("ʔ" "·")
       ("θ" "Σ̭")
       ("ð" "Ζ̭")
       ("s" "Σ")
       ("z" "Ζ")
       ("ʂ" "Σ̬")
       ("ʐ" "Ζ̬")
       ("ɕ" "Σ̱")
       ("ʑ" "Ζ̱")
       ("x" "Χ")
       ("ʀ" "Γ̩")
       ("h" "Ͱ")
       ("h̪͆" "Ͱ̭")
       ("ɦ̪͆" "Ͱ̩")
       ("t͡s" "Τ̤")
       ("ʈʂ" "Ͳ̤")
       ("k͡x" "Κ̤")
       ("ɬ" "Λ")
       ("ɮ" "Λ̩")
       ("i" "Ι")
       ("ɯ" "Υ")
       ("e̞" "Ε")
       ("a" "Α")))

(defun learn-dragon-word (form gloss learn)
  (let ((word (word form)))
    (unless (run-dfsm *dragon-words* form)
      (error "Form ~S is not a valid dragon word"
             form))
    (unless (empty? (@ *dragon-dictionary* form))
      (warn "Homophones for ~S:~% ~S ~S~%~{~{ ~S~}~}"
            form gloss learn
            (convert 'list
                     (image (lambda (entry)
                              (list (gloss<- entry)
                                    (learn<- entry)))
                            (@ *dragon-dictionary* form)))))
    (learn-word *dragon-store* word learn)
    (setf *dragon-dictionary*
          (with *dragon-dictionary*
                (dictionary-entry word gloss learn)))))

(defun generate-dragon-word (categories &optional (negative (set)))
  (generate-word *dragon-words* *dragon-store* categories
                 :negative negative))


(defun print-dragon-words (n categories
                             &optional (negative (set)))
  (let ((words (set)))
    (loop :for x :below n
       :do
       (let ((word (generate-dragon-word categories negative)))
	 (when (and (empty? (@ *dragon-dictionary*
			       (form<- word)))
		    (not (@ words (form<- word))))
	   (setf words
		 (with words (form<- word)))
	   (format t "~S ~A~%"
		   (form<- word)
		   (string<-word (image (lambda (glyph)
					  (or (@ *dragon-romanization* glyph)
					      glyph))
					(form<- word)))))))))

;;;; affixes
;;; participation
(learn-dragon-word '("ʈ" "e̞" "" "x" "ɯ")  ;  ͲΕΧΥ
                   "DRPAC.ACT"  ; direct active participation
                   (set :everything :affix :interesting))

(learn-dragon-word '("d" "i")  ; ΔΙ
                   "DRPAC.PASS"  ; direct passive participation
                   (set :everything :affix :interesting))

;;; sensory
(learn-dragon-word '("θ" "i")  ; Σ̭Ι
                   "SENS"  ; sensory
                   (set :everything :affix :interesting))

;;; deductive
(learn-dragon-word '("t͡s" "ɯ")  ; Τ̤Υ
                   "DED"  ; deductive
                   (set :everything :affix :boring))

;;; hearsay
(learn-dragon-word '("ɦ̪͆" "a")  ; Ͱ̩Α
                   "HSY"  ; hearsay
                   (set :everything :affix :boring))

;;; internal
(learn-dragon-word '("ʀ" "ɯ")  ; Γ̩Υ
                   "ITRL"  ; internal
                   (set :everything :affix))

;;; dubitative
(learn-dragon-word '("ɯ" "" "θ" "e̞")  ; ΥΣ̭Ε
                   "DUB"  ; dubitative
                   (set :everything :affix :negative))

;;; contrafactual
(learn-dragon-word '("ɯ" "" "x" "ɯ")  ; ΥΧΥ
                   "CFAC"  ; contrafactual
                   (set :everything :affix))

;;; infinitive
(learn-dragon-word '("ɯ")  ; Υ
                   "INF"
                   (set :everything :affix))

;;; present
(learn-dragon-word '("n" "a")  ; ΝΑ
                   "PRE"
                   (set :everything :affix))

;;; past
(learn-dragon-word '("k͡x" "ɯ")  ; Κ̤Υ
                   "PAS"
                   (set :everything :affix))

;;; gnomic
(learn-dragon-word '("e̞" "" "x" "ɯ")  ; ΕΧΥ
                   "GNO"
                   (set :everything :affix :interesting :boring))

;;; progressive
(learn-dragon-word '("θ" "a")  ; Σ̭Α
                   "PROG"
                   (set :everything :affix))

;;; frequentative
(learn-dragon-word '("θ" "e̞")  ; Σ̭Ε
                   "FREQ"
                   (set :everything :affix :boring))

;;; inceptive
(learn-dragon-word '("a")  ; Α
                   "INCEP"
                   (set :everything :affix))

;;; perfective
(learn-dragon-word '("ɯ" "" "z" "a")  ; ΥΖΑ
                   "PFV"
                   (set :everything :affix))

;;; volitional
(learn-dragon-word '("x" "ɯ")  ; ΧΥ
                   "VOL"
                   (set :everything :affix :positive))

;;; desiderative
(learn-dragon-word '("ɯ" "" "ɦ̪͆" "a")  ; ΥͰ̩Α
                   "DES"
                   (set :everything :affix :interesting))

;;; frustrative
(learn-dragon-word '("ŋ" "e̞")  ; Ν̱Ε
                   "FRUST"
                   (set :everything :affix :negative))

;;; non-voluntary
(learn-dragon-word '("ɯ" "" "d" "ɯ")  ; ΥΔΥ
                   "NVOL"
                   (set :everything :affix))

;;; subject
(learn-dragon-word '("x" "i")  ; ΧΙ
                   "SUB"
                   (set :everything :affix))

;;; accusative
(learn-dragon-word '("ɦ̪͆" "ɯ")  ; Ͱ̩Υ
                   "ACC"
                   (set :everything :affix))

;;; dative
(learn-dragon-word '("ʂ" "ɯ")  ; Σ̬Υ
                   "DAT"
                   (set :everything :affix))

;;; experiencer
(learn-dragon-word '("tʼ" "a")  ; Τ̣Α
                   "EXP"
                   (set :everything :affix :interesting))

;;; vocative
(learn-dragon-word '("z" "a")  ; ΖΑ
                   "VOC"
                   (set :everything :affix))

;;; proximative
(learn-dragon-word '("x" "a")  ; ΧΑ
                   "PROXI"
                   (set :everything :affix))

;;; allative
(learn-dragon-word '("tʼ" "a" "" "x" "ɯ")  ; Τ̣ΑΧΥ
                   "ALL"
                   (set :everything :affix))

;;; sublative
(learn-dragon-word '("θ" "ɯ" "" "tʼ" "a")  ; Σ̭ΥΤ̣Α
                   "SUBL"
                   (set :everything :affix))

;;; genitive
(learn-dragon-word '("ɕ" "ɯ" "" "x" "ɯ")  ; Σ̱ΥΧΥ
                   "GEN"
                   (set :everything :affix))

;;; temporal
(learn-dragon-word '("tʼ" "ɯ")  ; Τ̣Υ
                   "TEMP"
                   (set :everything :affix))

;;; plural
(learn-dragon-word '("i" "" "ɖ" "a")  ; ΙΔ̬Α
                   "PL"
                   (set :everything :affix))

;;; dimunitive
(learn-dragon-word '("i" "" "ɮ" "a")  ; ΙΛ̩Α
                   "DIM"
                   (set :everything :affix))

;;; augmentative
(learn-dragon-word '("a" "" "x" "ɯ")  ; ΑΧΥ
                   "AUG"
                   (set :everything :affix))

;;;; roots
(learn-dragon-word '("h̪͆" "ɯ" "" "ɖ" "a" "" "x" "a")  ; ĤUḌAXA
                   "voice"
                   (set :everything :noun))

(learn-dragon-word '("ʈʼ" "i" "" "ŋ" "a")  ; Ͳ̣IŇA
                   "friend"
                   (set :everything :noun :positive))

(learn-dragon-word '("k" "e̞")  ; KE
                   "I"
                   (set :everything :noun :boring :positive))

(learn-dragon-word '("h" "e̞" "" "tʼ" "a")  ; HEṬA
                   "you (friendly)"
                   (set :everything :noun :interesting :positive))

(learn-dragon-word '("ʀ" "a")  ; RA
                   "you (pejorative)"
                   (set :everything :noun :interesting :negative))

(learn-dragon-word '("a" "" "h̪͆" "e̞")  ; AĤE
                   "he/she/it (friendly)"
                   (set :everything :noun :interesting :positive))

(learn-dragon-word '("e̞" "" "x" "i")  ; EXI
                   "he/she/it (pejorative)"
                   (set :everything :noun :interesting :negative))

(learn-dragon-word '("θ" "ɯ")  ; ŜU
                   "he/she/it (boring)"
                   (set :everything :noun :boring))

(learn-dragon-word '("ɖ" "ʀ" "ɯ" "" "ɦ̪͆" "a")  ; ḌRUĦA
                   "dragon"
                   (set :everything :noun :boring :negative))

(learn-dragon-word '("t͡s" "a")  ; ṰA
                   "one/4th person (friendly)"
                   (set :everything :noun :positive))

(learn-dragon-word '("e̞" "" "ʀ" "a")  ; ERA
                   "one/4th person (pejorative)"
                   (set :everything :noun :negative))

(learn-dragon-word '("e̞")  ; E
                   "one/4th person (boring)"
                   (set :everything :noun :boring))

(learn-dragon-word '("ɯ" "" "ɬ" "a" "" "k" "i" "" "θ" "e̞")  ; ULAKIŜE
                   "stranger/outsider"
                   (set :everything :noun :interesting :positive))

(learn-dragon-word '("ɕ" "e̞" "" "x" "ɯ" "" "ʈʼ" "a")  ; ŚEXUͲ̣A
                   "angry"
                   (set :everything :adjective :interesting :negative))

(learn-dragon-word '("a" "" "ɳ" "a" "" "θ" "ɯ")  ; AṆAŜU
                   "animal"
                   (set :everything :noun))

(learn-dragon-word '("k" "e̞" "" "x" "a")  ; KEXA
                   "bad"
                   (set :everything :adjective :negative))

(learn-dragon-word '("i")  ; I
                   "be (copula)"
                   (set :everything :verb))

(learn-dragon-word '("ɯ" "" "ʈʼ" "a" "" "x" "ɯ")  ; UͲ̣AXU
                   "black"
                   (set :everything :adjective))

(learn-dragon-word '("i" "" "ɦ̪͆" "e̞" "" "x" "ɯ" "" "tʼ" "a")  ; IĦEXUṬA
                   "can"
                   (set :everything :verb :positive))

(learn-dragon-word '("i" "" "ɖ" "ɯ" "" "t" "ɯ" "" "h" "ɯ")  ; IḌUTUHU
                   "child"
                   (set :everything :noun :interesting))

(learn-dragon-word '("ʐ" "a" "" "x" "a")  ; ẒAXA
                   "eat"
                   (set :everything :verb :positive))

(learn-dragon-word '("e̞" "" "θ" "e̞" "" "x" "ɯ")  ; EŜEXU
                   "find"
                   (set :everything :verb :positive :interesting))

(learn-dragon-word '("a" "" "x" "a" "" "s" "a")  ; AXASA
                   "fish"
                   (set :everything :noun))

(learn-dragon-word '("ʈ" "e̞" "" "x" "e̞")  ; ͲEXE
                   "food"
                   (set :everything :noun :positive))

(learn-dragon-word '("tʼ" "a" "" "ʔ" "ɯ" "" "θ" "e̞")  ; ṬA·UŜE
                   "forget"
                   (set :everything :verb :negative :boring))

(learn-dragon-word '("a" "" "ɕ" "e̞")  ; AŚE
                   "good"
                   (set :everything :adjective :positive))

(learn-dragon-word '("x" "ɯ" "" "ʂ" "a")  ; ΧΥΣ̬Α
                   "see"
                   (set :everything :verb :interesting))

(learn-dragon-word '("e̞" "" "ʂ" "ɯ")  ; ΕΣ̬Υ
                   "know"
                   (set :everything :verb :interesting :positive))

(learn-dragon-word '("ɯ" "" "θ" "ɯ" "" "ʀ" "ŋ" "e̞")  ; ΥΣ̭ΥΓ̩Ν̱Ε
                   "wash"
                   (set :everything :verb :positive))

(learn-dragon-word '("ɯ" "" "ɕ" "a" "" "x" "a")  ; ΥΣ̱ΑΧΑ
                   "help"
                   (set :everything :verb :positive))

(learn-dragon-word '("ɯ" "" "ʈʼ" "a" "" "ʐ" "a")  ; ΥͲ̣ΑΖ̬Α
                   "follow"
                   (set :everything :verb))

(learn-dragon-word '("a" "" "ʂ" "i" "" "x" "a")  ; ΑΣ̬ΙΧΑ
                   "meet/encounter"
                   (set :everything :verb :interesting))

(learn-dragon-word '("a" "" "ŋ" "e̞" "" "n" "a")  ; ΑΝ̱ΕΝΑ
                   "talk"
                   (set :everything :verb :interesting))

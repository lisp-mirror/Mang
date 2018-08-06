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
  (map ("ɳ" "Ṇ")
       ("n" "N")
       ("ŋ" "Ň")
       ("t" "T")
       ("tʼ" "Ṭ")
       ("d" "D")
       ("ʈ" "Ͳ")
       ("ʈʼ" "Ͳ̣")
       ("ɖ" "Ḍ")
       ("k" "K")
       ("g" "G")
       ("ʔ" "·")
       ("θ" "Ŝ")
       ("ð" "Ẑ")
       ("s" "S")
       ("z" "Z")
       ("ʂ" "Ṣ")
       ("ʐ" "Ẓ")
       ("ɕ" "Ś")
       ("ʑ" "Ź")
       ("x" "X")
       ("ʀ" "R")
       ("h" "H")
       ("h̪͆" "Ĥ")
       ("ɦ̪͆" "Ħ")
       ("t͡s" "Ṱ")
       ("ʈʂ" "Ͳ̭")
       ("k͡x" "K̭")
       ("ɬ" "L")
       ("ɮ" "Ł")
       ("i" "I")
       ("ɯ" "U")
       ("e̞" "E")
       ("a" "A")))

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

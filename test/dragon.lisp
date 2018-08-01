(in-package #:mang)

(defparameter *dragon-phonemes*
  (glyph-system (map ('c (set "ɳ" "ȵ" "ɴ" "t" "tʼ" "d" "ʈ" "ʈʼ" "ɖ" "q" "ɢ" "ʔ"
                              "θ" "ð" "s" "z" "ʂ" "ʐ" "ɕ" "ʑ" "χ" "ʁ" "h" "h̪͆"
                              "ɦ̪͆" "t͡s" "ʈʂ" "q͡χ" "ɬ" "ɮ"))
                     ('p (set "t" "d" "ʈ" "ɖ" "q" "ɢ"))
                     ('l (set "ʁ" "ɬ" "ɮ"))
                     ('n (set "ɳ" "ȵ" "ɴ"))
                     ('f (set "θ" "ð" "s" "z" "ʂ" "ʐ" "ɕ" "ʑ" "χ" "ʁ"
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
  (let ((dist (yule-distribution '("e̞" "a" "ɯ" "i" "χ" "h̪͆" "θ" "t" "ʂ" "ɕ" "h"
                                   "q" "s" "ɴ" "ʔ" "ð" "tʼ" "t̪θ" "ʈʂ" "ʁ" "ʐ"
                                   "ʈʼ" "ȵ" "d" "ɦ̪͆" "ɖ" "ɳ" "ɢ" "ʈ" "z" "ʑ" "q͡χ"
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
       ("ȵ" "N")
       ("ɴ" "Ň")
       ("t" "T")
       ("tʼ" "Ṭ")
       ("d" "D")
       ("ʈ" "Ͳ")
       ("ʈʼ" "Ͳ̣")
       ("ɖ" "Ḍ")
       ("q" "K")
       ("ɢ" "G")
       ("ʔ" "·")
       ("θ" "Ŝ")
       ("ð" "Ẑ")
       ("s" "S")
       ("z" "Z")
       ("ʂ" "Ṣ")
       ("ʐ" "Ẓ")
       ("ɕ" "Ś")
       ("ʑ" "Ź")
       ("χ" "X")
       ("ʁ" "R")
       ("h" "H")
       ("h̪͆" "Ĥ")
       ("ɦ̪͆" "Ħ")
       ("t͡s" "Ṱ")
       ("ʈʂ" "Ͳ̭")
       ("q͡χ" "K̭")
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
(learn-dragon-word '("h̪͆" "ɯ" "" "ɖ" "a" "" "χ" "a")  ; ĤUḌAXA
                   "voice"
                   (set :everything :noun))

(learn-dragon-word '("ʈʼ" "i" "" "ɴ" "a")  ; Ͳ̣IŇA
                   "friend"
                   (set :everything :noun :positive))

(learn-dragon-word '("q" "e̞")  ; KE
                   "I"
                   (set :everything :noun :boring :positive))

(learn-dragon-word '("h" "e̞" "" "tʼ" "a")  ; HEṬA
                   "you (friendly)"
                   (set :everything :noun :interesting :positive))

(learn-dragon-word '("ʁ" "a")  ; RA
                   "you (pejorative)"
                   (set :everything :noun :interesting :negative))

(learn-dragon-word '("a" "" "h̪͆" "e̞")  ; AĤE
                   "he/she/it (friendly)"
                   (set :everything :noun :interesting :positive))

(learn-dragon-word '("e̞" "" "χ" "i")  ; EXI
                   "he/she/it (pejorative)"
                   (set :everything :noun :interesting :negative))

(learn-dragon-word '("θ" "ɯ")  ; ŜU
                   "he/she/it (boring)"
                   (set :everything :noun :boring))

(learn-dragon-word '("ɖ" "ʁ" "ɯ" "" "ɦ̪͆" "a")  ; DRUĦA
                   "dragon"
                   (set :everything :noun :boring :negative))

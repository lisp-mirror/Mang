(in-package #:mang)

(defparameter *dragon-phonemes*
  (glyph-system (map ('c (set "ɳ" "ȵ" "ɴ" "t" "tʼ" "d" "dʰ" "ʈ" "ʈʼ"
                              "ɖ" "ɖʰ" "q" "ɢ" "ʔ" "θ" "ð" "s" "z" "ʂ"
                              "ʐ" "ɕ" "ʑ" "χ" "ʁ" "h" "h̪͆" "ɦ̪͆" "t̪θ"
                              "t̪θʼ" "t͡s" "t͡sʼ" "ʈʂ" "ʈʂʼ" "t͡ɕ" "t͡ɕʼ"
                              "q͡χ" "ɬ" "ɮ"))
                     ('p (set "t" "d" "ʈ" "ɖ" "q" "ɢ"))
                     ('l (set "χ" "ʁ" "ɬ" "ɮ"))
                     ('n (set "ɳ" "ȵ" "ɴ"))
                     ('f (set "θ" "ð" "s" "z" "ʂ" "ʐ" "ɕ" "ʑ" "χ" "ʁ"
                              "h" "h̪͆" "ɦ̪͆"))
                     ('v (set "i" "ɯ" "e̞" "ʌ̝" "a")))))

(defparameter *dragon-words*
  (word-system (list 1 4 `(,(set 'c '(f n)
                                 '(p l))
                            v))
               *dragon-phonemes*))

(defparameter *dragon-store*
  (let ((dist (yule-distribution '("χ" "h" "ʌ̝" "a" "θ" "t" "ʂ" "ɕ" "h̪͆"
                                   "q" "s" "ɴ" "ʔ" "ð" "tʼ" "t̪θ" "ʈʂ"
                                   "ʁ" "ʐ" "ȵ" "d" "ʈʼ" "ɦ̪͆" "ɯ" "t̪θʼ"
                                   "ɖ" "ɳ" "ɢ" "ʈ" "z" "ʑ" "q͡χ" "e̞"
                                   "t͡ɕ" "ɖʰ" "t͡ɕʼ" "t͡s" "ʈʂʼ" "t͡sʼ"
                                   "i" "ɬ" "dʰ" "ɮ" "")
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
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:noun
                    (set (match-everything-generator)
                         (match-outro-generator 3 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:verb
                    (set (match-everything-generator)
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
       ("tʼ" "T·")
       ("d" "D")
       ("dʰ" "D·")
       ("ʈ" "Ṭ")
       ("ʈʼ" "Ṭ·")
       ("ɖ" "Ḍ")
       ("ɖʰ" "Ḍ·")
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
       ("t̪θ" "Ŧ")
       ("t̪θʼ" "Ŧ·")
       ("t͡s" "T:")
       ("t͡sʼ" "T:·")
       ("ʈʂ" "Ṭ:")
       ("ʈʂʼ" "Ṭ:·")
       ("t͡ɕ" "Ť")
       ("t͡ɕʼ" "Ť·")
       ("q͡χ" "K:")
       ("ɬ" "L")
       ("ɮ" "Ł")
       ("i" "I")
       ("ɯ" "U")
       ("e̞" "E")
       ("ʌ̝" "O")
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

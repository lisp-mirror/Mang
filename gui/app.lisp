(in-package #:mang-gui)

(defparameter *standard-alphabet*
  (alphabet "A" "a" "Aj" "aj" "Ja" "ja" "Jaj" "jaj"
            "E" "e" "Ej" "ej" "Je" "je" "Jej" "jej"
            "I" "i" "Ij" "ij" "Ji" "ji" "Jij" "jij"
            "O" "o" "Oj" "oj" "Jo" "jo" "Joj" "joj"
            "U" "u" "Uj" "uj" "Ju" "ju" "Juj" "juj"
            "Y" "y" "Yj" "yj" "Jy" "jy" "Jyj" "jyj"
            "B" "b" "Bs" "bs"
            "P" "p" "Ps" "ps"
            "D" "d" "Ds" "ds"
            "T" "t" "Ts" "ts"
            "G" "g" "Gs" "gs" "Gw" "gw"
            "K" "k" "Ks" "ks" "Kw" "kw"
            "M" "m" "M!" "m!"
            "N" "n" "Ng" "ng"
            "L" "l" "L!" "l!"
            "R" "r" "X" "x"
            "H" "h" "Ch" "ch"
            "Ĝ" "ĝ" "Sch" "sch"
            "V" "v" "F" "f"
            "Z" "z" "S" "s"))

(defapp mang "Mang" :center (1000 600)
    ((alphabet-storage :type [alphabet]
                       :accessor alphabet-storage
                       :initform (c-in *standard-alphabet*))
     ;; the following value and everything manipulating it needs to be
     ;; reworked to respect the follow-length of patterns
     (categories :type set
                 :accessor categories
                 :initform (c-in (empty-set)))
     (learn-patterns :type map
                     :accessor learn-patterns
                     :initform (c-in (empty-map (empty-map))))
     (learned-words :type map
                    :accessor learned-words
                    :initform (c-in (empty-map (empty-set))))
     (fdbs :type map
           :accessor fdbs
           :initform (c-in (empty-map <no-follow-db>))))
    ()
  (+notebook ()
    "Alphabet"
    (make-instance 'alphabet-box
                   :fm-parent *parent*)
    "Learning patterns"
    (make-instance 'pattern-box
                   :fm-parent *parent*)
    "Learn"
    (make-instance 'learn-box
                   :fm-parent *parent*)
    "Generate"
    (make-instance 'generate-box
                   :fm-parent *parent*)
    ))

(defun run-mang ()
  (start-app 'mang))

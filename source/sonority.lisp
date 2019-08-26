(in-package #:mang)

;;; FIX: You know, all of this. This is obviously a sketch that doesn't do what
;;; it is supposed to do at all.
(defun syllable-nuclei (word hierarchy)
  (labels ((_scan-to-end (phoneme word i)  ; maybe an additional parameter here?
             (if word
                 (chop next-phoneme word
                   (if (before? next-phoneme phoneme hierarchy)
                       (values nil i)
                       (bind (((:values _ end)
                               (_scan-to-end phoneme word (1+ i))))
                         (values i end))))
                 (values i i))))
    (when word
      (bind (((:values begin end)
              (_scan-to-end (first word)
                            (rest word)
                            0)))
        (if begin
            (append (loop :for n :from begin :to end
                       :collect n)
                    (syllable-nuclei (nthcdr end word)
                                     hierarchy))
            (syllable-nuclei (nthcdr end word)
                             hierarchy))))))

(defun syllable-init (word nucleus-index earliest-start hierarchy)
  (or (loop :for x :downfrom (1- nucleus-index)
         :for y :from (- nucleus-index 2)
         :downto earliest-start
         :unless (before? (nth y word)
                          (nth x word)
                          hierarchy)
         :do (return-from syllable-init
               y))
      earliest-start))

(defun syllable-coda (word nucleus-index latest-end hierarchy)
  (or (loop :for x :from (1+ nucleus-index)
         :for y :from (+ nucleus-index 2)
         :to latest-end
         :unless (before? (nth y word)
                          (nth x word)
                          hierarchy)
         :do (return-from syllable-coda
               y))
      latest-end))

(defun syllable-inits (word hierarchy)
  (bind ((nuclei (syllable-nuclei word hierarchy)))
    (cons 0 (loop :for (prev this)
               :on nuclei
               :if this
               :collect (1+ (syllable-init word this prev hierarchy))))))

(defun syllable-codas (word hierarchy)
  (bind ((nuclei (syllable-nuclei word hierarchy)))
    (loop :for (this next)
       :on nuclei
       :if next
       :collect (syllable-coda word this next hierarchy))))

(defun mark-syllables (word nuclei inits constant-init present-init absent-init
                       constant-nucleus present-nucleus absent-nucleus
                       constant-coda present-coda absent-coda)
  (declare (type list word nuclei inits)
           (type map constant-init constant-nucleus constant-coda)
           (type set present-init absent-init present-nucleus absent-nucleus
                 present-coda absent-coda))
  (loop :for i :from 0
     :for phoneme :in word
     :collect
       (cond
         ((member i nuclei)
          (augment-feature-set phoneme constant-nucleus present-nucleus
                               absent-nucleus))
         (([d]or (not (find-previous i nuclei))
                 (< it (find-previous i inits)))
          (augment-feature-set phoneme constant-init present-init
                               absent-init))
         (t
          (augment-feature-set phoneme constant-coda present-coda
                               absent-coda)))))

(defun syllabalize (word hierarchy constant-init present-init absent-init
                    constant-nucleus present-nucleus absent-nucleus
                    constant-coda present-coda absent-coda)
  (declare (type list word hierarchy)
           (type map constant-init constant-nucleus constant-coda)
           (type set present-init absent-init present-nucleus absent-nucleus
                 present-coda absent-coda))
  (mark-syllables word (syllable-nuclei word hierarchy)
                  (syllable-inits word hierarchy)
                  constant-init present-init absent-init constant-nucleus
                  present-nucleus absent-nucleus constant-coda present-coda
                  absent-coda))

(defun parse-sonority-class (glyphs categories)
  (declare (type map glyphs categories))
  (>>!
    _ (parse-whitespace)
    front (// (parse-from-map categories)
              (parse-from-map glyphs))
    back (many (>> (parse-whitespace)
                   (parse-constant ",")
                   (parse-whitespace)
                   (// (parse-from-map categories)
                       (parse-from-map glyphs)))
               (empty-set)
               (lambda (phoneme phonemes)
                 (with phonemes phoneme)))
    _ (parse-expression-end)
    (succeed (with back front))))

(defun parse-sonority-hierarchy (glyphs categories)
  (declare (type map glyphs categories))
  (>> (parse-whitespace)
      (parse-constant "#sonority-hierarchy:")
      (parse-expression-end)
      (some (parse-sonority-class glyphs categories)
            '() #'cons)))

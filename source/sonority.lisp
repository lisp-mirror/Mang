(in-package #:mang)

(defun syllable-nuclei (word hierarchy)
  (labels ((_scan-nuclei (word i)
             (when word
               (chop phoneme word
                 (if word
                     (bind ((next-phoneme (first word)))
                       (cond
                         ((before? next-phoneme phoneme hierarchy)
                          (values (list i)
                                  (1+ i)))
                         ((before? phoneme next-phoneme hierarchy)
                          (_scan-nuclei word (1+ i)))
                         (t
                          (bind (((:values next end)
                                  (_scan-nuclei word (1+ i))))
                            (if (> (first next)
                                   (1+ i))
                                (values next end)
                                (values (cons i next)
                                        end))))))
                     (values (list i)
                             (1+ i))))))
           (_scan-candidate (word i)
             ([a]when (rest word)
               (chop (phoneme next-phoneme)
                   word
                 (if (before? next-phoneme phoneme hierarchy)
                     (_scan-candidate it (1+ i))
                     (1+ i))))))
    (when word
      (bind (((:values nuclei end)
              (_scan-nuclei word 0))
             (next-candidate (_scan-candidate word (1+ end))))
        (if next-candidate
            (append nuclei
                    (mapcar (lambda (n)
                              (+ n next-candidate))
                            (syllable-nuclei (nthcdr next-candidate word)
                                             hierarchy)))
            nuclei)))))

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
  (parse-sequence (// (<$> (parse-category categories)
                           (lambda (category)
                             (convert 'set
                                      (second category))))
                      (<$> (parse-glyph glyphs)
                           (lambda (glyph)
                             (set (second glyph)))))
                  (>> (parse-whitespace)
                      (parse-constant ",")
                      (parse-whitespace))
                  (empty-set)
                  #'union))

(defun parse-sonority-hierarchy (glyphs categories)
  (declare (type map glyphs categories))
  (parse-section "sonority-hierarchy"
                 (parse-lines (parse-sonority-class glyphs categories))))

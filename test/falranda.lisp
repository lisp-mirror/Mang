(in-package #:mang)

(defun word-in-world (world population spec)
  (@ (dictionary<- (@ (populations<- (first world))
                      population))
     spec))

(defparameter *falranda*
  (list (add-population (world)
                        "urdragons"
                        (<population> *dragon-dictionary* 100))))

(defmacro apply-world-changes (place &body changes)
  (when changes
    (let ((g!new-world (gensym "new-world")))
      `(let ((,g!new-world (first ,place)))
         ,@(loop :for change :in changes
              :collect
              `(setf ,g!new-world
                     (,(first change)
                       ,g!new-world
                       ,@(rest change))))
         (push ,g!new-world ,place)))))

(apply-world-changes *falranda*
  (split-population "urdragons"
                    "western urdragons" 1
                    "eastern urdragons" 1
                    2)
  (realize-sound-change (sound-change `(,(set "ɦ̪͆" "h̪͆"))
                                      '("a")
                                      '("")
                                      '("ɯ")
                                      :ignore-syllable-boundaries? nil)
                        "western urdragons"
                        2)
  (realize-sound-change (sound-change `(,(set "ɦ̪͆" "h̪͆"))
                                      '("e̞")
                                      '("")
                                      '("i")
                                      :ignore-syllable-boundaries? nil)
                        "eastern urdragons"
                        2)
  (connect-populations "western urdragons" "eastern urdragons" 3)
  (split-population "western urdragons"
                    "upper western urdragons" 1
                    "lower western urdragons" 1
                    2))

(defun export-language-for-speak (dict file)
  (with-open-file (file file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (export-dictionary file dict " - "
                       (list (lambda (entry)
                               (string<-word (word<- entry)))
                             #'gloss<-))))

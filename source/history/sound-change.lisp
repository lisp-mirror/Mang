(in-package #:mang)

(defclass sound-change ()
  ((%source :type function
            :reader source<-
            :initarg :source
            :initform `(:sequence))
   (%target :type string
            :reader target<-
            :initarg :target
            :initform nil)
   (%pre :type function
         :reader pre<-
         :initarg :pre
         :initform `(:sequence))
   (%post :type function
          :reader post<-
          :initarg :post
          :initform `(:sequence))))

;;;; ANNOTATED-STRING<-WORD is necessary(?) to be able to use CL-PPCRE – it only
;;;; works on strings. This means that "#" has to be an illegal character in any
;;;; given glyph.
(defmethod annotated-string<-word ((word cons))
  (format nil "~{~A~}"
          (intersperse "#" word)))

(defmethod annotated-string<-word ((word word))
  (annotated-string<-word (form<- word)))

(defmethod merge-words ((word1 cons)
                        (word2 cons))
  (append word1 word2))

(defmethod merge-words ((word1 word)
                        (word2 word))
  (word (merge-words (form<- word1)
                     (form<- word2))
        :origin (list word1 word2)))

(defmethod apply-sound-change ((word string)
                               (sound-change sound-change))
  (let ((pre (pre<- sound-change))
        (source (source<- sound-change))
        (post (post<- sound-change)))
    (multiple-value-bind (begin end)
        (scan `(:sequence (:positive-lookbehind ,pre)
                          ,source
                          (:positive-lookahead ,post))
              word)
      (if begin
          (concatenate 'string
                       (subseq word 0 begin)
                       (target<- sound-change)
                       (subseq word end))
          word))))

(defmethod apply-sound-change ((word word)
                               (sound-change sound-change))
  (word
   (split-sequence #\#
                   (apply-sound-change (annotated-string<-word word)
                                       sound-change)
                   :remove-empty-subseqs nil
                   :test #'char=)
   :origin word))

(defmethod apply-sound-change ((word dictionary)
                               (sound-change sound-change))
  (dictionary (image (lambda (dictionary-entry)
                       (dictionary-entry
                        (apply-sound-change (word<- dictionary-entry)
                                            sound-change)
                        (gloss<- dictionary-entry)
                        (learn<- dictionary-entry)))
                     (content<- word))))

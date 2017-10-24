(in-package #:mang)

(defclass word ()
  ((%form :type cons
          :reader form<-
          :initarg :form
          :initform '(""))
   (%origin :type (or word null)
            :reader origin<-
            :initarg :origin
            :initform nil)
   (%transformations :type (or cons null)
                     :reader transformations<-
                     :initarg :transformations
                     :initform nil)))

(defmethod print-object ((object word)
                         stream)
  (format stream "(WORD ~S :ORIGIN ~S :TRANSFORMATIONS ~S)"
          (form<- object)
          (origin<- object)
          (transformations<- object)))

(defun word (form &key origin transformations)
  (declare (type cons form)
           (type (or word null)
                 origin)
           (type (or cons null)
                 transformations))
  (make-instance 'word
                 :form form
                 :origin origin
                 :transformations transformations))

(defmethod learn (markov (obj word))
  (learn markov (form<- obj)))

(defclass dictionary-entry ()
  ((%word :type word
          :reader word<-
          :initarg :word
          :initform (word '("")))
   (%gloss :type string
           :reader gloss<-
           :initarg :gloss
           :initform "")
   (%learn :type set
           :reader learn<-
           :initarg :learn
           :initform (set))))

(defmethod print-object ((object dictionary-entry)
                         stream)
  (format stream "(DICTIONARY-ENTRY ~S ~S ~S)"
          (word<- object)
          (gloss<- object)
          (learn<- object)))

(defun dictionary-entry (word gloss learn)
  (declare (type word word)
           (type string gloss))
  (make-instance 'dictionary-entry
                 :word word
                 :gloss gloss
                 :learn learn))

(defclass dictionary ()
  ((%content :type set
             :reader content<-
             :initarg :content
             :initform (set))))

(defmethod print-object ((object dictionary)
                         stream)
  (format stream "(DICTIONARY ~S)"
          (content<- object)))

(defun dictionary (&rest entries)
  (make-instance 'dictionary
                 :content (convert 'set entries)))

(defmethod lookup ((collection dictionary)
                   (key string))
  (filter (lambda (entry)
            (equal? (gloss<- entry)
                    key))
          (content<- collection)))

(defmethod lookup ((collection dictionary)
                   (key word))
  (filter (lambda (entry)
            (equal? (word<- entry)
                    key))
          (content<- collection)))

(defmethod lookup ((collection dictionary)
                   (key cons))
  (filter (lambda (entry)
            (equal? (form<- (word<- entry))
                    key))
          (content<- collection)))

(defmethod with ((collection dictionary)
                 (value1 dictionary-entry)
                 &optional value2)
  (let ((content (content<- collection)))
    (if value2  ; this use of value2 is probably very unclean
        (if (@ content value1)
            (values collection nil)
            (values (make-instance 'dictionary
                                   :content (with content value1))
                    t))
        (values (make-instance 'dictionary
                               :content (with content value1))
                t))))

(defmethod less ((collection dictionary)
                 (value1 dictionary-entry)
                 &optional value2)
  (declare (ignore value2))
  (make-instance 'dictionary
                 :content (with (content<- collection)
                                value1)))

(defmethod add-entry ((dictionary dictionary)
                      (word word)
                      (gloss string)
                      (learn set))
  (with dictionary (dictionary-entry word gloss learn)))

(in-package #:mang)

(defun parse-gloss ()
  (some (>>* (parse-anything)
             parsed (if (find-if (lambda (parsed-char)
                                   (or (has-property parsed-char "Whitespace")
                                       (has-property parsed-char "Control")
                                       (@ (set #\- #\[ #\])
                                          parsed-char)))
                                 parsed)
                        (fail nil)
                        (succeed parsed)))))

(defun parse-defined-entry (language parts-of-speech)
  (declare (type language)
           (type set parts-of-speech))
  (bind ((markov-spec (markov-spec<- language)))
    (>>!
      gloss (parse-gloss)
      _ (parse-whitespace)
      part-of-speech (parse-from-set parts-of-speech)
      _ (parse-whitespace)
      word (parse-word (glyphs<- language))
      _ (parse-whitespace)
      allow-homophones? (// (<$ (parse-constant "y")
                                t)
                            (<$ (parse-constant "n")
                                nil))
      _ (parse-whitespace)
      word-categories
      (<? (parse-wrapped "{" (parse-separated (parse-from-map markov-spec)
                                              "," (empty-set)
                                              (lambda (cat cats)
                                                (with cats (first cat))))
                         "}")
          (empty-set))
      (succeed (add-word language word-categories part-of-speech gloss word
                         allow-homophones?)))))

(defun parse-generated-entry (language parts-of-speech)
  (declare (type language language)
           (type set parts-of-speech))
  (bind ((markov-spec (markov-spec<- language)))
    (>>!
      gloss (parse-gloss)
      _ (parse-whitespace)
      part-of-speech (parse-from-set parts-of-speech)
      _ (parse-whitespace)
      word-categories
      (parse-wrapped "{" (parse-separated (parse-from-map markov-spec)
                                          "," (empty-set)
                                          (lambda (cat cats)
                                            (with cats (first cat))))
                     "}")
      negative-word-categories
      (>> (parse-whitespace)
          (// (parse-wrapped "{" (parse-separated (parse-from-map markov-spec)
                                                  "," (empty-set)
                                                  (lambda (cat cats)
                                                    (with cats (first cat))))
                             "}")
              (<$ (parse-constant "{}")
                  (empty-set))))
      (succeed (add-gloss language word-categories negative-word-categories
                          part-of-speech gloss)))))

(defun parse-dictionary-entry (language parts-of-speech)
  (declare (type language language)
           (type set parts-of-speech))
  (// (parse-defined-entry language parts-of-speech)
      (parse-generated-entry language parts-of-speech)))

(defun parse-dictionary-entries (language parts-of-speech)
  (declare (type language language)
           (type set parts-of-speech))
  (>>!
    language (parse-dictionary-entry language parts-of-speech)
    (<? (>> (parse-expression-end)
            (parse-whitespace)
            (parse-dictionary-entries language parts-of-speech))
        language)))

(defun parse-dictionary (language)
  (declare (type language language))
  (parse-section "dictionary"
                 (>>!
                   parts-of-speech (parse-separated (parse-identifier
                                                     *mang-reserved-symbols*)
                                                    "," (empty-set)
                                                    (lambda (pos poss)
                                                      (with poss pos)))
                   _ (parse-whitespace)
                   (parse-dictionary-entries language parts-of-speech))))

(defun parse-dictionary-file (language)
  (declare (type language language))
  (bind ((expected-name (name<- language)))
    (>>!
      name (parse-language-header)
      (if (string= expected-name name)
          (>>!
            language (parse-dictionary language)
            _ (>> (parse-whitespace)
                  (parse-eof))
            (succeed language))
          (fail `(:wrong-language ,name ,expected-name))))))

(defun load-dictionary-file (language file)
  (with-open-file (stream file)
    (parser-call (parse-dictionary-file language)
                 stream)))

(defun write-dictionary (stream language
                         &key
                           (sort-by-gloss? t)
                           (computer-readable? t)
                           (sorting-predicate (lambda (s1 s2)
                                                (string< (string-downcase s1)
                                                         (string-downcase s2)))))
  (declare (type language language)
           (type boolean sort-by-gloss? computer-readable?)
           (type (function (string string)
                           boolean)
                 sorting-predicate))
  (bind ((dictionary (dictionary<- language))
         (glyphs (glyphs<- language)))
    (format stream "# dictionary~%")
    (format stream "~{~A~^,~}~%~%"
            (convert 'list
                     (domain dictionary)))
    (bind ((dict (sort (reduce (lambda (acc pos entries)
                                 (append (reduce (lambda (acc gloss entry)
                                                   (cons (cons gloss
                                                               (cons pos entry))
                                                         acc))
                                                 entries
                                                 :initial-value '())
                                         acc))
                               dictionary
                               :initial-value '())
                       sorting-predicate
                       :key (if sort-by-gloss?
                                #'first
                                #'third))))
      (loop
        :for (gloss pos word word-categories allow-homophones?)
          :in dict
        :do
           (format stream "~A ~A ~A ~A {~{~A~^,~}}~%"
                   gloss pos (string<-word glyphs word
                                           :computer-readable?
                                           computer-readable?)
                   (if allow-homophones?
                       "y"
                       "n")
                   (convert 'list
                            word-categories))))))

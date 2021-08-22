(in-package #:mang)

(defun parse-full-existing-gloss (dictionary)
  (bind ((parts-of-speech (domain dictionary)))
    (>>!
      pos (parse-from-set parts-of-speech)
      _ (parse-constant ":")
      gloss (parse-from-set (domain (@ dictionary pos)))
      (succeed `(:gloss ,pos ,gloss)))))

(defun parse-part-of-speech (dictionary)
  (>>!
    pos (parse-wrapped (parse-constant "[")
                       (parse-from-set (domain dictionary))
                       (parse-constant "]"))
    register (parse-number)
    (succeed `(:part-of-speech ,pos ,register))))

(defun parse-semantic-shift-source (dictionary)
  (parse-sequence (// (<$> (parse-full-existing-gloss dictionary)
                           (lambda (gloss)
                             `(,gloss ,(empty-set))))
                      (<$> (parse-part-of-speech dictionary)
                           (lambda (pos)
                             `(,pos ,(set (third pos))))))
                  (>> (parse-whitespace)
                      (parse-constant "+")
                      (parse-whitespace))
                  `(()
                    ,(empty-set))
                  (lambda (spec acc)
                    (spread (cons union)
                      spec acc))))

(defun parse-semantic-shift-target (dictionary registers)
  (>>!
    pos (parse-from-set (domain dictionary))
    _ (parse-constant ":")
    target-namer (some (// (parse-wrapped (parse-constant "[")
                                          (>>!
                                            register (parse-number)
                                            (if (@ registers register)
                                                (succeed register)
                                                (fail `(:invalid-register
                                                        ,register
                                                        ,registers))))
                                          (parse-constant "]"))
                           (parse-gloss))
                       '() #'cons)
    (succeed `(,pos ,@target-namer))))

(defun parse-semantic-shift (source-language)
  (bind ((dictionary (map-union (dictionary<- source-language)
                                (unknown-dictionary<- source-language)
                                #'map-union)))
    (>>!
      (source-glosses registers)
      (parse-semantic-shift-source dictionary)
      _ (>> (parse-whitespace-no-newline)
            (parse-constant "=>")
            (parse-whitespace-no-newline))
      target (parse-semantic-shift-target dictionary registers)
      _ (parse-whitespace-no-newline)
      word-categories
      (<? (parse-w/s (>> (parse-constant "{")
                         (parse-whitespace))
                     (parse-from-set (domain (markov-spec<- source-language)))
                     (>> (parse-whitespace)
                         (parse-constant ",")
                         (parse-whitespace))
                     (>> (parse-whitespace)
                         (parse-constant "}"))
                     (empty-set)
                     (lambda (cat cats)
                       (with cats cat)))
          (empty-set))
      (succeed `(,source-glosses ,target ,word-categories)))))

(defun semantic-shift-results (semantic-shift dictionary)
  (bind (((source (target-pos &rest target)
                  word-categories)
          semantic-shift))
    (labels ((_build-gloss (acc registers todo)
               (if todo
                   (chop task todo
                     (_build-gloss (concatenate 'string
                                                acc
                                                (if (stringp task)
                                                    task
                                                    (@ registers task)))
                                   registers todo))
                   acc))
             (_rec (word registers todo)
               (if todo
                   (chop task todo
                     (bind (((mode pos gloss/register)
                             task))
                       (ecase mode
                         (:part-of-speech
                          (reduce (lambda (acc gloss continue)
                                    (map-union acc
                                               (_rec (append-words word
                                                                   (first continue))
                                                     (with registers
                                                           gloss/register gloss)
                                                     todo)))
                                  (@ dictionary pos)
                                  :initial-value (empty-map)))
                         (:gloss
                          (_rec (append-words word (first (@ (@ dictionary pos)
                                                             gloss/register)))
                                registers todo)))))
                   (map ((_build-gloss "" registers target)
                         word)))))
      (list target-pos (_rec `(,(map (:begin t))
                               ,(map (:end t)))
                             (empty-map)
                             source)
            word-categories))))

(defun parse-drop-gloss (language)
  (>>!
    gloss (parse-full-existing-gloss (map-union (dictionary<- language)
                                                (unknown-dictionary<- language)
                                                #'map-union))
    _ (>> (parse-whitespace-no-newline)
          (parse-constant "#"))
    (succeed `(:drop ,gloss))))

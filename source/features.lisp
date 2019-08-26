(in-package #:mang)

(defun parse-binary-feature-definition ()
  (>> (parse-constant "binary")
      (parse-whitespace)
      (parse-constant ":")
      (parse-whitespace)
      (parse-separated (parse-identifier *mang-reserved-symbols*)
                       "," (empty-set)
                       (lambda (feature features)
                         (with features feature)))))

(defun parse-valued-feature-definition ()
  (>>!
    _ (>> (parse-constant "valued")
          (parse-whitespace)
          (parse-constant ":")
          (parse-whitespace))
    name (parse-identifier *mang-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    values (parse-separated (parse-identifier *mang-reserved-symbols*)
                            "," (empty-set)
                            (lambda (value values)
                              (with values value)))
    (succeed `(,name ,values))))

(defun parse-privative-feature-definition ()
  (>> (parse-constant "privative")
      (parse-whitespace)
      (parse-constant ":")
      (parse-whitespace)
      (parse-separated (parse-identifier *mang-reserved-symbols*)
                       "," (empty-set)
                       (lambda (feature features)
                         (with features feature)))))

(defun parse-feature-definition ()
  (// (<$> (parse-binary-feature-definition)
           (lambda (definition)
             `(,definition
                ,(empty-map (empty-set))
                ,(empty-set))))
      (<$> (parse-valued-feature-definition)
           (lambda (definition)
             (bind (((name values)
                     definition))
               `(,(empty-set)
                  ,(map (name values))
                  ,(empty-set)))))
      (<$> (parse-privative-feature-definition)
           (lambda (definition)
             `(,(empty-set)
                ,(empty-map (empty-set))
                ,definition)))))

(defun parse-feature-section ()
  (parse-section
   "features"
   (parse-lines (parse-feature-definition)
                `(,(empty-set)
                   ,(empty-map)
                   ,(empty-set))
                (lambda (def defs)
                  (bind (((binary valued privative)
                          defs)
                         ((nbinary nvalued nprivative)
                          def))
                    `(,(union binary nbinary)
                       ,(map-union valued nvalued)
                       ,(union privative nprivative)))))))

(defun parse-binary-feature-spec (binary-features)
  (declare (type set binary-features))
  (>>!
    sign (// (<$ (parse-constant "+")
                 t)
             (<$ (parse-constant "-")
                 nil))
    _ (parse-whitespace)
    feature (parse-from-set binary-features)
    (succeed `(,feature ,sign))))

(defun parse-valued-feature-spec (valued-features)
  (declare (type map valued-features))
  (>>!
    (feature values)
    (parse-from-map valued-features)
    _ (>> (parse-whitespace)
          (parse-constant "=")
          (parse-whitespace))
    value (parse-from-set values)
    (succeed `(,feature ,value))))

(defun parse-present-feature-spec (privative-features)
  (declare (type set privative-features))
  (>>!
    present? (?? (parse-constant "~")
                 (succeed nil)
                 (succeed t))
    feature (parse-from-set privative-features)
    (succeed `(,feature ,present?))))

(defun parse-feature-spec (binary-features valued-features privative-features)
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (// (<$> (// (parse-binary-feature-spec binary-features)
               (parse-valued-feature-spec valued-features))
           (lambda (spec)
             (bind (((feature value)
                     spec))
               `(,(map (feature value)
                       :default (empty-set))
                  ,(empty-set)
                  ,(empty-set)))))
      (<$> (parse-present-feature-spec privative-features)
           (lambda (spec)
             (bind (((feature present?)
                     spec))
               (if present?
                   `(,(empty-map (empty-set))
                      ,(set feature)
                      ,(empty-set))
                   `(,(empty-map (empty-set))
                      ,(empty-set)
                      ,(set feature))))))))

(defun parse-feature-set-with-absent (binary-features valued-features
                                      privative-features)
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (parse-wrapped "["
                 (parse-separated
                  (parse-feature-spec binary-features valued-features
                                      (union (union binary-features
                                                    (domain valued-features))
                                             privative-features))
                  "," `(,(empty-map)
                         ,(empty-set)
                         ,(empty-set))
                  (lambda (feature features)
                    (bind (((constant present absent)
                            features)
                           ((nconstant npresent nabsent)
                            feature))
                      `(,(map-union constant nconstant)
                         ,(union present npresent)
                         ,(union absent nabsent)))))
                 "]"))

(defun parse-feature-set (binary-features valued-features privative-features)
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (parse-wrapped "["
                 (parse-separated (parse-feature-spec binary-features
                                                      valued-features
                                                      privative-features)
                                  "," (empty-map)
                                  (lambda (feature features)
                                    (bind (((constant present _)
                                            feature))
                                      (map-union
                                       (map-union constant
                                                  (convert 'map
                                                           present
                                                           :key-fn #'identity
                                                           :value-fn
                                                           (constantly t)))
                                       features))))
                 "]"))

(defun has-features? (phoneme features present absent)
  (declare (type map phoneme features))
  (and (not (@ (gmap :set (lambda (feature value)
                            (eql (@ phoneme feature)
                                 value))
                     (:map features))
               nil))
       (bind ((features (domain phoneme)))
         (and (empty? (set-difference present features))
              (empty? (intersection absent features))))))

(defun augment-feature-set (feature-set overwrite present absent)
  (filter (lambda (k v)
            (declare (ignore v))
            (not (@ absent k)))
          (map-union (map-union feature-set overwrite)
                     (convert 'map
                              present
                              :key-fn #'identity
                              :value-fn (constantly t)))))

(in-package #:mang)

(defun parse-glyph-definition (binary-features valued-features
                               privative-features)
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (>>!
    name (parse-identifier *mang-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    phoneme (parse-feature-set binary-features valued-features
                               privative-features)
    (succeed (map (name phoneme)
                  :default (empty-map)))))

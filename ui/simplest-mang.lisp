(defpackage #:mang.simplest
  (:use #:cl #:mang)
  (:shadowing-import-from
   #:metabang-bind
   #:bind)
  (:shadowing-import-from
   #:opts
   #:get-opts #:define-opts #:describe #:exit))

(in-package #:mang.simplest)

(defun simplest-mang-inner (feature-file language-file dictionary-file
                            &key
                              diachrony-file
                              computer-readable?)
  (bind ((language (read-mang-files feature-file language-file
                                    :dictionary-file dictionary-file
                                    :diachrony-file diachrony-file)))
    (if (listp language)
        (bind (((error-type error)
                language))
          (format *error-output* (ecase error-type
                                   (:language-file-failure
                                    "Language file failed to load: ~A")
                                   (:dictionary-file-failure
                                    "Dictionary file failed to load: ~A")
                                   (:diachrony-file-error
                                    "Diachrony file failed to load: ~A"))
                  error))
        (loop
          :while (ask-arbitrary-known-gloss! language)
          :finally
             (write-dictionary t language
                               :computer-readable? computer-readable?)))))

(define-opts
  (:name :features
   :description "Phonological feature description file"
   :short #\f
   :long "--features"
   :arg-parser #'identity
   :meta-var "path/to/features.mang"
   :required t)
  (:name :language
   :description "Language description file"
   :short #\l
   :long "--language-base"
   :arg-parser #'identity
   :meta-var "path/to/language/base.mang"
   :required t)
  (:name :dictionary
   :description "Language dictionary file"
   :short #\d
   :long "--language-dictionary"
   :arg-parser #'identity
   :meta-var "path/to/language/dictionary.mang"
   :required t)
  (:name :diachrony
   :description "Diachrony file"
   :short #\D
   :long "--diachrony"
   :arg-parser #'identity
   :meta-var "path/to/language/diachrony.mang")
  (:name :computer-readable?
   :description "Print the dictionary in a format that is uniquely parseable by
  Mang?"
   :short #\c
   :long "--computer-readable"))

(defun simplest-mang ()
  (bind (((:values args free)
          (get-opts)))
    ;; TODO: Add error handling for missing arguments
    (assert (not free))
    (simplest-mang-inner (getf args :features)
                         (getf args :language)
                         (getf args :dictionary)
                         :diachrony-file (getf args :diachrony)
                         :computer-readable? (get args :computer-readable?))))

#+sbcl
(defun compile-simplest-mang ()
  (sb-ext:save-lisp-and-die "simplest-mang"
                            :toplevel #'simplest-mang
                            :executable t
                            :purify t
                            :root-structures #'simplest-mang
                            :compression t))

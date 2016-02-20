(in-package #:mang-gui)

(defmd learn-box (hbox)
  :kids
  (kids-list?
   (+listbox :category ("Category")
       ()
       (c? (convert 'list
                    (app-value learn-patterns)
                    :pair-fn (lambda (k v)
                               (declare (ignore v))
                               k)))
     (list item))
   (+vbox (:expand t :fill t)
     (+hbox ()
       (+label "Enter word to learn:")
       (+entry :word))
     (+hbox ()
       (+label "Weight:")
       (+spin-button :weight (1 1000 1)))
     (+button
         (c?
           (let ((decomposition (widget-value :decompositions))
                 (category (widget-value :category))
                 (weight (widget-value :weight)))
             (if decomposition
                 (if category
                     (format nil "Learn '~A' in category '~A' with weight ~D"
                             (glyph-string<-rope decomposition)
                             category weight)
                     (format nil "No category selected to learn '~A' in"
                             (glyph-string<-rope decomposition)))
                 "No decomposition to learn selected")))
         (w e d)
       (let ((decomposition (widget-value :decompositions))
             (category (widget-value :category))
             (weight (widget-value :weight)))
         (when (and decomposition category)
           (let* ((learned (app-value learned-words))
                  (c-learned (lookup learned category)))
             (unless (lookup c-learned decomposition)
               ;; learn the word
               (let ((fdbs (app-value fdbs)))
                 (setf (app-value fdbs)
                       (with fdbs category
                             (reduce (lambda (fdb pattern/weight)
                                       (learn fdb (car pattern/weight)
                                              decomposition (* (cdr pattern/weight)
                                                               weight)
                                              #| preliminary |# 1))
                                     (convert 'list
                                              (lookup (app-value learn-patterns)
                                                      category))
                                     :initial-value (lookup fdbs category)))))
               (print (app-value fdbs))
               ;; register the word as learned
               (setf (app-value learned-words)
                     (with learned category
                           (with c-learned decomposition))))))))
     (+listbox :decompositions ("Decomposition")
         ()
         (c? (let ((word (widget-value :word)))
               (when word
                 (extract-ropes word (app-value alphabet-storage)))))
       (list (glyph-string<-rope item))))
   (+vbox (:expand t :fill t)
     (+label (c? (let ((category (widget-value :category)))
                   (if category
                       (format nil "Words learned in category '~A':"
                               category)
                       "No category chosen"))))
     (+listbox :learned ("Learned words")
         ()
         (c? (let ((category (widget-value :category)))
               (when category
                 (convert 'list
                          (lookup (app-value learned-words)
                                  category)))))
       (list (glyph-string<-rope item))))))

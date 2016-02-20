(in-package #:mang-gui)

(defmd generate-box (hbox)
  :kids
  (kids-list?
   (+vbox (:expand t :fill t)
     (+hbox ()
       (+label "Number of words to generate:")
       (+spin-button :number-generated (1 100 10)))
     (+listbox :gen-category ("Category")
         ()
         (c? (convert 'list (app-value categories)))
       (list item)))
   (+vbox (:expand t :fill t)
     (+listbox :generated ("Generated words")
         ()
         (c?
           (let ((category (lookup (app-value fdbs)
                                   (widget-value :gen-category))))
             (when category
               (loop for x below (or (widget-value :number-generated)
                                     0)
                  collect (complete category (<rglyph> <start-glyph>))))))
       (list (glyph-string<-rope item))))
   (+listbox :learned ("Learned words")
       ()
       (c? (let ((category (widget-value :gen-category)))
             (when category
               (convert 'list
                        (lookup (app-value learned-words)
                                category)))))
     (list (glyph-string<-rope item)))))

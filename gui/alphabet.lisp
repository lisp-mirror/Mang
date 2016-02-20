(in-package #:mang-gui)

(defmd alphabet-box (hbox)
  :kids
  (kids-list?
   (+vbox (:expand t :fill t)
     (+hbox ()
       (+entry :new-glyph)
       (+button
           (c?
             (let ((new-glyph (widget-value :new-glyph)))
               (if new-glyph
                   (format nil "Add glyph '~A'"
                           new-glyph)
                   "No glyph to add")))
           (w e d)
         (let ((new-glyph (widget-value :new-glyph)))
           (when new-glyph
             (setf (app-value alphabet-storage)
                   (with (app-value alphabet-storage)
                         (<glyph> new-glyph)))))))
     (+button
         (c?
           (let ((glyph (widget-value :alphabet)))
             (if glyph
                 (format nil "Remove glyph '~A'"
                         (rep glyph))
                 "No glyph selected")))
         (w e d)
       (let ((glyph (widget-value :alphabet)))
         (when glyph
           (setf (app-value alphabet-storage)
                 (less (app-value alphabet-storage)
                       glyph)))))
     (+listbox :alphabet ("Glyph")
         ()
         (c? (list<- (app-value alphabet-storage)))
       (list (rep item))))))

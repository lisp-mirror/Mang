(in-package #:mang)

(defun ensure-list (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun repeat (n obj &optional (intersperse nil intersperse?))
  (when (> n 0)
    (if (and intersperse? (> n 1))
        (list* obj intersperse (repeat (1- n)
                                       obj intersperse))
        (cons obj (repeat (1- n)
                          obj)))))

(defun var-repeat (min max obj &optional (intersperse nil intersperse?))
  (bind ((collected (empty-set)))
    (if intersperse?
        (loop :for n :from min :to max
           :do (setf collected
                     (with collected (repeat n obj intersperse))))
        (loop :for n :from min :to max
           :do (setf collected
                     (with collected (repeat n obj)))))
    collected))

(defun intersperse (item list)
  (if (rest list)
      (list* (first list)
             item (intersperse item (rest list)))
      list))

(defmethod tree-map (f tree)
  (@ f tree))

(defmethod tree-map (f (tree null))
  (declare (ignore f tree))
  nil)

(defmethod tree-map (f (tree cons))
  (cons (tree-map f (car tree))
        (tree-map f (cdr tree))))

(defmethod length= ((seq1 null)
                    (seq2 null))
  (declare (ignore seq1 seq2))
  t)

(defmethod length= ((seq1 null)
                    (seq2 integer))
  (declare (ignore seq1))
  (= seq2 0))

(defmethod length= ((seq1 null)
                    (seq2 cons))
  (declare (ignore seq1 seq2))
  nil)

(defmethod length= ((seq1 null)
                    (seq2 sequence))
  (declare (ignore seq1))
  (= (length seq2)
     0))

(defmethod length= ((seq1 integer)
                    (seq2 null))
  (declare (ignore seq2))
  (= seq1 0))

(defmethod length= ((seq1 integer)
                    (seq2 integer))
  (= seq1 seq2))

(defmethod length= ((seq1 integer)
                    (seq2 cons))
  (unless (= seq1 0)
    (length= (1- seq1)
             (cdr seq2))))

(defmethod length= ((seq1 integer)
                    (seq2 sequence))
  (= seq1 (length seq2)))

(defmethod length= ((seq1 cons)
                    (seq2 null))
  (declare (ignore seq1 seq2))
  nil)

(defmethod length= ((seq1 cons)
                    (seq2 integer))
  (unless (= seq2 0)
    (length= (cdr seq1)
             (1- seq2))))

(defmethod length= ((seq1 cons)
                    (seq2 cons))
  (length= (cdr seq1)
           (cdr seq2)))

(defmethod length= ((seq1 cons)
                    (seq2 sequence))
  (length= seq1 (length seq2)))

(defmethod length= ((seq1 sequence)
                    (seq2 null))
  (declare (ignore seq2))
  (= (length seq1)
     0))

(defmethod length= ((seq1 sequence)
                    (seq2 integer))
  (= (length seq1)
     seq2))

(defmethod length= ((seq1 sequence)
                    (seq2 cons))
  (length= (length seq1)
           seq2))

(defmethod length= ((seq1 sequence)
                    (seq2 sequence))
  (= (length seq1)
     (length seq2)))

(defmethod length> ((seq1 null)
                    seq2)
  (declare (ignore seq1 seq2))
  nil)

(defmethod length> ((seq1 integer)
                    (seq2 null))
  (declare (ignore seq2))
  (/= seq1 0))

(defmethod length> ((seq1 integer)
                    (seq2 integer))
  (> seq1 seq2))

(defmethod length> ((seq1 integer)
                    (seq2 cons))
  (unless (= seq1 0)
    (length> (1- seq1)
             (cdr seq2))))

(defmethod length> ((seq1 integer)
                    (seq2 sequence))
  (> seq1 (length seq2)))

(defmethod length> ((seq1 cons)
                    (seq2 null))
  (declare (ignore seq1 seq2))
  t)

(defmethod length> ((seq1 cons)
                    (seq2 integer))
  (or (= seq2 0)
      (length> (cdr seq1)
               (1- seq2))))

(defmethod length> ((seq1 cons)
                    (seq2 cons))
  (length> (cdr seq1)
           (cdr seq2)))

(defmethod length> ((seq1 cons)
                    (seq2 sequence))
  (length> seq1 (length seq2)))

(defmethod length> ((seq1 sequence)
                    (seq2 null))
  (declare (ignore seq2))
  (/= (length seq1)
      0))

(defmethod length> ((seq1 sequence)
                    (seq2 integer))
  (> (length seq1)
     seq2))

(defmethod length> ((seq1 sequence)
                    (seq2 cons))
  (length> (length seq1)
           seq2))

(defmethod length> ((seq1 sequence)
                    (seq2 sequence))
  (> (length seq1)
     (length seq2)))

(defun length>= (a b)
  (or (length= a b)
      (length> a b)))

(defun prefix? (prefix sequence &key (test #'equal?))
  (if (length= prefix 0)
      t
      (and (length> sequence 0)
           (@ test (elt prefix 0)
                   (elt sequence 0))
           (prefix? (subseq prefix 1)
                    (subseq sequence 1)
                    :test test))))

(defun postfix? (postfix sequence &key (test #'equal?))
  (prefix? (reverse postfix)
           (reverse sequence)
           :test test))

(defun intro (n sequence)
  (subseq sequence 0 (min n (length sequence))))

(defun intros (sequence)
  (loop :for n :from 0 :to (length sequence)
     :collect (intro n sequence)))

(defun outro (n sequence)
  (reverse (intro n (reverse sequence))))

(defmethod filter (fn (collection null))
  (declare (ignore fn collection))
  nil)

(defmethod filter ((fn function)
                   (collection cons))
  (remove-if-not fn collection))

(defmethod filter ((fn collection)
                   (collection cons))
  (bind (((curr &rest rest) collection))
    (if (@ fn curr)
        (cons curr (filter fn rest))
        (filter fn rest))))

(defmethod without ((collection null)
                    items)
  (declare (ignore collection items))
  nil)

(defmethod without ((collection cons)
                    (items set))
  (bind (((curr &rest rest) collection))
    (if (@ items curr)
        (without rest items)
        (cons curr (without rest items)))))

(defmethod before? (a b (sequence cons))
  (labels ((_find (x xs)
             (when xs
               (bind (((f &rest r)
                       xs))
                 (if (typecase f
                       (set
                        (@ f x))
                       (function
                        (funcall f x))
                       (t
                        (equal? f x)))
                     (values r t)
                     (_find x r))))))
    (nth-value 1 (_find b (_find a sequence)))))

(defmethod find-previous ((number real)
                          (numbers null))
  nil)

(defmethod find-previous ((number real)
                          (numbers cons))
  (bind (((current &rest numbers)
          numbers))
    (if (> current number)
        nil
        (or (find-previous number numbers)
            current))))

(defmethod find-previous ((number real)
                          (numbers vector))
  (loop :for i :across numbers
     :do (when (<= i number)
           (return-from find-previous
             i))
     :finally (return nil)))

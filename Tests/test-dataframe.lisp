(in-package #:eu.turtleware.polyclot/tests)

(def-suite* polyclot.data-frame
  :in polyclot
  :description "Data frame protocol.")

(defconstant +max-cols+ 16)
(defconstant +max-rows+ 32)

(defun gen-valid-cols (length &key (prefix "col"))
  (let ((cols (loop for i from 0 below length
                    collect (format nil "~a-~a" prefix i))))
    (lambda ()
      ;; we may use any sequence to specify cols
      (coerce cols (whichever 'vector 'list)))))

(defun gen-valid-row (length)
  (let ((gen-elt (gen-one-element 1 "a" '(1) #\d #(1) #*101 3.14))
        (gen-str (gen-string :length (constantly length))))
    (lambda ()
      (case (random 4)
        ;; rows specified with a list
        (0 (loop for i from 0 below length
                 collect (funcall gen-elt)))
        ;; rows specified with a vector
        (1 (coerce (loop for i from 0 below length
                         collect (funcall gen-elt))
                   'vector))
        ;; rows specified with a string
        (2 (funcall gen-str))
        ;; rows specified with a bit vector
        (3 (make-array length :element-type 'bit :initial-element 0))))))

(defun gen-valid-rows (how-many length)
  (let ((gen-row (gen-valid-row length)))
    (lambda ()
      (loop repeat how-many
            collect (funcall gen-row)))))

(defun gen-invalid-length (length)
  (cond ((zerop length)
         (gen-integer :min 1 :max +max-cols+))
        ((= length +max-cols+)
         (gen-integer :min 0 :max (1- length)))
        (t
         (lambda ()
           (let ((int (funcall (gen-integer :min 0 :max (1- length)))))
             (if (< int length)
                 int
                 (1+ int)))))))

(defun gen-invalid-cols (length &key (prefix "col"))
  (let ((gen-invalid-length (gen-invalid-length length))
        (gen-invalid-value (gen-one-element 1 'column #(1 2) #(#\a #\b))))
    (lambda ()
      (case (cond ((zerop length) (random 1))
                  ((< length 2)   (random 2))
                  (t              (random 3)))
        ;; too many or to little columns.
        (0 (funcall (gen-valid-cols (funcall gen-invalid-length))))
        ;; non-string column name
        (1 (loop with invalid-index = (random length)
                 for i from 0 below length
                 if (= i invalid-index)
                   collect (funcall gen-invalid-value)
                 else
                   collect (format nil "~a-~a" prefix i)))
        ;; ;; duplicated column name
        (2 (loop with dup-src = (random length)
                 with dup-dst = (let ((dup (random length)))
                                  (if (/= dup dup-src)
                                      dup
                                      (if (zerop dup)
                                          (1+ dup)
                                          (1- dup))))
                 for i from 0 below length
                 if (= i dup-dst)
                   collect (format nil "~a-~a" prefix dup-src)
                 else
                   collect (format nil "~a-~a" prefix i)))))))

(defun gen-invalid-rows (how-many length)
  (let ((gen-invalid-length (gen-invalid-length length)))
    (gen-valid-rows how-many (funcall gen-invalid-length))))

(defun gen-data-frame (how-many length)
  (let ((gen-cols (gen-valid-cols length))
        (gen-rows (gen-valid-rows how-many length)))
    (lambda ()
      (apply #'make-data-frame
             (funcall gen-cols)
             (funcall gen-rows)))))

(defun df-equal (df1 df2)
  (and (equal (multiple-value-list (dims df1))
              (multiple-value-list (dims df2)))
       (equalp (cols df1) (cols df2))
       (prog1 t
         (map nil (lambda (row1 row2)
                    (unless (every #'equalp row1 row2)
                      (return-from df-equal nil)))
              (rows df1) (rows df2)))))

(defun gen-valid-row-index (data-frame)
  (lambda ()
    (random (dims data-frame))))

(defun gen-valid-row-spec (data-frame)
  (let ((gen-index (gen-valid-row-index data-frame))
        (gen-row (apply #'gen-one-element
                        (coerce (rows data-frame) 'list))))
    (lambda ()
      (case (random 2)
        (0 (funcall gen-index))
        (1 (funcall gen-row))))))

(defun gen-valid-col-index (data-frame)
  (lambda ()
    (let ((index (random (nth-value 1 (dims data-frame)))))
      (case (random 2)
        (0 index)
        (1 (elt (cols data-frame) index))))))

(defun gen-valid-row-slice (data-frame)
  (let ((gen-index (gen-valid-row-index data-frame)))
    (lambda ()
      (case (random 5)
        (0 t)
        (1 (cons t (funcall gen-index)))
        (2 (cons (funcall gen-index) t))
        (3 (let ((i1 (funcall gen-index))
                 (i2 (funcall gen-index)))
             (cons (min i1 i2) (max i1 i2))))
        (4 (let* ((length (random (dims data-frame)))
                  (array (make-array length)))
             (loop for i from 0 below length
                   do (setf (elt array i)
                            (funcall gen-index))
                   finally (return array))))))))

(defun gen-valid-col-slice (data-frame)
  (let ((gen-index (gen-valid-col-index data-frame))
        (cols (cols data-frame)))
    (lambda ()
      (case (random 5)
        (0 t)
        (1 (cons t (funcall gen-index)))
        (2 (cons (funcall gen-index) t))
        (3 (let ((i1 (funcall gen-index))
                 (i2 (funcall gen-index)))
             (cond ((and (integerp i1)
                         (integerp i2))
                    (cons (min i1 i2) (max i1 i2)))
                   ((integerp i1)
                    (if (< i1 (position i2 cols :test #'string=))
                        (cons i1 i2)
                        (cons i2 i1)))
                   ((integerp i2)
                    (if (> i2 (position i1 cols :test #'string=))
                        (cons i1 i2)
                        (cons i2 i1)))
                   (t
                    (if (< (position i1 cols :test #'string=)
                           (position i2 cols :test #'string=))
                        (cons i1 i2)
                        (cons i2 i1))))))
        (4 (let* ((length (random (dims data-frame)))
                  (array (make-array length)))
             (loop for i from 0 below length
                   do (setf (elt array i)
                            (funcall gen-index))
                   finally (return array))))))))

(defun gen-invalid-row-index (data-frame)
  (let ((how-many (dims data-frame)))
    (lambda ()
      (case (random 3)
        (0 (funcall (gen-integer :max -1)))
        (1 (funcall (gen-integer :min how-many)))
        (2 (funcall (gen-string)))))))

(defun gen-invalid-row-spec (data-frame)
  (let ((how-many (dims data-frame)))
    (lambda ()
      (case (random 3)
        (0 (funcall (gen-integer :max -1)))
        (1 (funcall (gen-integer :min how-many)))
        (2 :bam)))))

(defun gen-invalid-col-index (data-frame)
  (let ((length (nth-value 1 (dims data-frame))))
    (lambda ()
      (case (random 2)
        (0 (funcall (gen-integer :max -1)))
        (1 (funcall (gen-integer :min length)))))))

(defun gen-invalid-row-slice (data-frame)
  (let ((gen-index (gen-valid-row-index data-frame))
        (gen-index* (gen-invalid-row-index data-frame))
        (rows (rows data-frame)))
    (lambda ()
      (case (random 7)
        (0 nil)
        (1 (cons t (funcall gen-index*)))
        (2 (cons (funcall gen-index*) t))
        (3 (let ((i1 (funcall gen-index))
                 (i2 (funcall gen-index)))
             (if (< i1 i2)
                 (cons i2 i1)
                 :invalid)))
        (4 (let ((i1 (funcall gen-index))
                 (i2 (funcall gen-index*))
                 (i3 (funcall gen-index*)))
             (case (random 3)
               (0 (cons i1 i2))
               (1 (cons i2 i1))
               (2 (cons i2 i3)))))
        (5 (let* ((length (funcall (gen-integer :min 1 :max (length rows))))
                  (wrong (random length))
                  (array (make-array length)))
             (loop for i from 1 below length
                   do (setf (elt array i)
                            (funcall gen-index))
                   finally (setf (elt array wrong)
                                 (funcall gen-index*))
                           (return array))))))))

(defun gen-invalid-col-slice (data-frame)
  (let ((gen-index (gen-valid-col-index data-frame))
        (gen-index* (gen-invalid-col-index data-frame))
        (cols (cols data-frame)))
    (lambda ()
      (case (random 6)
        (0 nil)
        (1 (cons t (funcall gen-index*)))
        (2 (cons (funcall gen-index*) t))
        (3 (let ((i1 (funcall gen-index))
                 (i2 (funcall gen-index)))
             (cond ((and (integerp i1)
                         (integerp i2))
                    (if (< i1 i2)
                        (cons i2 i1)
                        :invalid))
                   ((integerp i1)
                    (if (< i1 (position i2 cols :test #'string=))
                        (cons i2 i1)
                        :invalid))
                   ((integerp i2)
                    (if (> i2 (position i1 cols :test #'string=))
                        (cons i2 i1)
                        :invalid))
                   (t
                    (if (< (position i1 cols :test #'string=)
                           (position i2 cols :test #'string=))
                        (cons i2 i1)
                        :invalid)))))
        (4 (let ((i1 (funcall gen-index))
                 (i2 (funcall gen-index*))
                 (i3 (funcall gen-index*)))
             (case (random 3)
               (0 (cons i1 i2))
               (1 (cons i2 i1))
               (2 (cons i2 i3)))))
        (5 (let* ((length (funcall (gen-integer :min 1 :max (length cols))))
                  (wrong (random length))
                  (array (make-array length)))
             (loop for i from 0 below length
                   do (setf (elt array i)
                            (funcall gen-index))
                   finally (setf (elt array wrong)
                                 (funcall gen-index*))
                           (return array))))))))

(defun gen-valid-function (which-function)
  ;; which-function is set to a symbol matching the function name we are
  ;; testing
  (lambda ()
    ;; combination of inputs
    (ecase which-function
      ((map-data-frame-rows add-cols!)
       (lambda (row-index row)
         (declare (ignore row-index))
         (if (> (length row) 2)
             (cons (elt row 0) (elt row 1))
             42)))
      (map-data-frame-cols
       (lambda (col-index col-name value)
         (declare (ignore col-index))
         (cons col-name value)))
      (map-data-frame
       (lambda (row-index row col-index col-name value)
         (declare (ignore row-index col-name))
         (eq (elt row col-index) value))))))

(defun gen-invalid-function (which-function)
  ;; which-function is set to a symbol matching the function name we are
  ;; testing
  (lambda ()
    (case (random 3)
      ;; not a function
      (0 42)
      ;; wrong parameter definition
      (1 (lambda (only-one-arg)
           only-one-arg))
      ;; invalid use of arguments
      (2 (ecase which-function
           ((map-data-frame-rows add-cols!)
            (lambda (row-index row) ; wrong index
              (declare (ignore row-index))
              (elt row (1+ (length row))))) ; 1+ for empty row
           (map-data-frame-cols
            (lambda (col-index col-name value) ; very bad function
              (declare (ignore col-name value))
              (elt col-index (1+ (length col-index)))))
           (map-data-frame
            (lambda (row-index row col-index col-name value)
              (declare (ignore row-index col-index col-name value))
              (elt row (1+ (length row)))))))))) ; 1+ for empty row

(defun gen-valid-name-fun-pairs (how-many)
  (let ((gen-fun (gen-valid-function 'add-cols!))
        (cols (funcall (gen-valid-cols how-many :prefix "new"))))
    (lambda ()
      (loop for i from 0 below how-many
            append (list (elt cols i)
                         (funcall gen-fun))))))

(defun gen-invalid-name-fun-pairs (how-many)
  (let ((gen-valid-fun (gen-invalid-function 'add-cols!))
        (gen-invalid-fun (gen-invalid-function 'add-cols!))
        (cols-valid (funcall (gen-valid-cols how-many :prefix "new")))
        (cols-invalid (funcall (gen-invalid-cols how-many :prefix "new"))))
    (flet ((gen-pair (ind)
             (case (if (< ind (length cols-invalid))
                       (random 3)
                       0)
               (0 (list (elt cols-valid ind) (funcall gen-invalid-fun)))
               (1 (list (elt cols-invalid ind) (funcall gen-valid-fun)))
               (2 (list (elt cols-invalid ind) (funcall gen-invalid-fun))))))
      (lambda ()
        (loop for i from 0 below how-many
              append (gen-pair i))))))


(def-test make-data-frame ()
  (for-all ((width  (gen-integer :min 0 :max +max-cols+))
            (height (gen-integer :min 0 :max +max-rows+)))
    (let ((col-spec (funcall (gen-valid-cols width)))
          (row-spec (funcall (gen-valid-rows height width))))
      (is (typep (apply #'make-data-frame col-spec row-spec) <data-frame>)))))

(def-test make-data-frame.invalid ()
  (for-all ((width  (gen-integer :min 0 :max +max-cols+))
            (height (gen-integer :min 1 :max +max-rows+)))
    (let ((invalid-col-spec (funcall (gen-invalid-cols width)))
          (invalid-row-spec (funcall (gen-invalid-rows height width)))
          (valid-col-spec   (funcall (gen-valid-cols width)))
          (valid-row-spec   (funcall (gen-valid-rows height width))))
      (signals error
        (apply #'make-data-frame invalid-col-spec valid-row-spec))
      (signals error
        (apply #'make-data-frame valid-col-spec invalid-row-spec)))))

(def-test copy-data-frame ()
  (for-all ((width (gen-integer :min 0 :max +max-cols+))
            (height (gen-integer :min 0 :max +max-rows+)))
    (let ((df (funcall (gen-data-frame height width))))
      (is (df-equal df (copy-data-frame df))))))

(def-test join-data-frame ()
  (fail "Not implemented yet."))

(def-test sel ()
  (for-all (((df row-slice col-slice)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-slice df))
                       (funcall (gen-valid-col-slice df)))))))
    (is (typep (sel df row-slice col-slice) <data-frame>))))

(def-test sel.invalid ()
  (for-all (((df row-slice col-slice row-slice* col-slice*)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-slice df))
                       (funcall (gen-valid-col-slice df))
                       (funcall (gen-invalid-row-slice df))
                       (funcall (gen-invalid-col-slice df)))))))
    (signals <invalid-slice> (sel df row-slice col-slice*))
    (signals <invalid-slice> (sel df row-slice* col-slice))
    (signals <invalid-slice> (sel df row-slice* col-slice*))))

(def-test ref ()
  (for-all (((df row-spec col-index)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-spec df))
                       (funcall (gen-valid-col-index df)))))))
    (finishes (ref df row-spec col-index))))

(def-test ref.invalid ()
  (for-all (((df row-spec col-index row-spec* col-index*)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-spec df))
                       (funcall (gen-valid-col-index df))
                       (funcall (gen-invalid-row-spec df))
                       (funcall (gen-invalid-col-index df)))))))
    (signals <invalid-index> (ref df row-spec col-index*))
    (signals <invalid-index> (ref df row-spec* col-index))
    (signals <invalid-index> (ref df row-spec* col-index*))))

(def-test map-data-frame ()
  (for-all (((df row-slice col-slice function)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-slice df))
                       (funcall (gen-valid-col-slice df))
                       (funcall (gen-valid-function 'map-data-frame)))))))
    (finishes (map-data-frame df row-slice col-slice function))))

(def-test map-data-frame.invalid ()
  (for-all (((df row-slice col-slice row-slice* col-slice*)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-slice df))
                       (funcall (gen-valid-col-slice df))
                       (funcall (gen-invalid-row-slice df))
                       (funcall (gen-invalid-col-slice df))))))
            (function (gen-valid-function 'map-data-frame))
            (function* (gen-invalid-function 'map-data-frame)
                       (not (or (and (arrayp row-slice)
                                     (zerop (array-total-size row-slice)))
                                (and (arrayp col-slice)
                                     (zerop (array-total-size col-slice)))))))
    (signals error (map-data-frame df row-slice col-slice function*))
    (signals <invalid-slice> (map-data-frame df row-slice col-slice* function))
    (signals <invalid-slice> (map-data-frame df row-slice* col-slice function))
    (signals <invalid-slice> (map-data-frame df row-slice* col-slice* function))
    (signals <invalid-slice> (map-data-frame df row-slice* col-slice* function*))))

(def-test map-data-frame-rows ()
  (for-all (((df row-slice function)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-slice df))
                       (funcall (gen-valid-function 'map-data-frame-rows)))))))
    (finishes (map-data-frame-rows df row-slice function))))

(def-test map-data-frame-rows.invalid ()
  (for-all (((df row-slice row-slice*)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-slice df))
                       (funcall (gen-invalid-row-slice df))))))
            (function (gen-valid-function 'map-data-frame-rows))
            (function* (gen-invalid-function 'map-data-frame-rows) 
                       (not (and (arrayp row-slice)
                                 (zerop (array-total-size row-slice))))))
    (signals error (map-data-frame-rows df row-slice function*))
    (signals <invalid-slice> (map-data-frame-rows df row-slice* function))
    (signals <invalid-slice> (map-data-frame-rows df row-slice* function*))))

(def-test map-data-frame-cols ()
  (for-all (((df row-spec col-slice function)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-spec df))
                       (funcall (gen-valid-col-slice df))
                       (funcall (gen-valid-function 'map-data-frame-cols)))))))
    (finishes (map-data-frame-cols df row-spec col-slice function))))

(def-test map-data-frame-cols.invalid ()
  (for-all (((df row-spec col-slice row-spec* col-slice*)
             (lambda ()
               (let ((df (funcall
                          (gen-data-frame (funcall (gen-integer :min 1 :max +max-rows+))
                                          (funcall (gen-integer :min 1 :max +max-cols+))))))
                 (list df
                       (funcall (gen-valid-row-spec df))
                       (funcall (gen-valid-col-slice df))
                       (funcall (gen-invalid-row-spec df))
                       (funcall (gen-invalid-col-slice df))))))
            (function (gen-valid-function 'map-data-frame-cols))
            (function* (gen-invalid-function 'map-data-frame-cols)
                       (not (and (arrayp col-slice)
                                 (zerop (array-total-size col-slice))))))
    (signals error (map-data-frame-cols df row-spec col-slice function*))
    (signals <invalid-slice> (map-data-frame-cols df row-spec* col-slice function))
    (signals <invalid-slice> (map-data-frame-cols df row-spec col-slice* function))
    (signals <invalid-slice> (map-data-frame-cols df row-spec* col-slice* function))
    (signals <invalid-slice> (map-data-frame-cols df row-spec* col-slice* function*))))

(def-test add-rows! ()
  (for-all ((width  (gen-integer :min 1 :max +max-cols+))
            (height (gen-integer :min 1 :max +max-rows+))
            (new-height (gen-integer :min 1 :max +max-rows+)))
    (let ((df (funcall (gen-data-frame height width)))
          (new-row-spec (funcall (gen-valid-rows new-height width))))
      (finishes (apply #'add-rows! df new-row-spec))
      (is (= (dims df) (+ height new-height))))))

(def-test add-rows!.invalid ()
  (for-all ((width  (gen-integer :min 1 :max +max-cols+))
            (height (gen-integer :min 1 :max +max-rows+))
            (new-height (gen-integer :min 1 :max +max-rows+)))
    (let ((df (funcall (gen-data-frame height width)))
          (new-row-spec (funcall (gen-invalid-rows new-height width))))
      (signals <insert-error> (apply #'add-rows! df new-row-spec)))))

(def-test add-cols! ()
  (for-all ((width  (gen-integer :min 1 :max +max-cols+))
            (height (gen-integer :min 1 :max +max-rows+))
            (new-width (gen-integer :min 1 :max +max-cols+)))
    (let* ((df (funcall (gen-data-frame height width)))
           (name-fun-pairs (funcall (gen-valid-name-fun-pairs new-width))))
      (finishes (apply #'add-cols! df name-fun-pairs))
      (is (= (nth-value 1 (dims df)) (+ width new-width))))))

(def-test add-cols!.invalid ()
  (for-all ((width  (gen-integer :min 1 :max +max-cols+))
            (height (gen-integer :min 1 :max +max-rows+))
            (new-width (gen-integer :min 1 :max +max-cols+)))
    (let* ((df (funcall (gen-data-frame height width)))
           (name-fun-pairs (funcall (gen-invalid-name-fun-pairs new-width))))
      (signals error (apply #'add-cols! df name-fun-pairs)))))

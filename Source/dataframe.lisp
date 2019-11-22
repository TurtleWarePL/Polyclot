(in-package #:eu.turtleware.polyclot.impl)

(define-class <data-frame> () ())

(define-class <raw-data-frame> (<data-frame>)
  ((rows :initarg :rows :reader rows)
   (cols :initarg :cols :reader cols)))

(deftype %col-index% () `(or string   (integer 0)))
(deftype %row-index% () `(integer 0))
(deftype %row-spec%  () `(or %row-index% sequence))

(deftype %col-slice% () `(or (eql t)
                             (cons %col-index% %col-index%)
                             (cons (eql t) %col-index%)
                             (cons %col-index% (eql t))
                             (vector %col-index%)))

(deftype %row-slice% () `(or (eql t)
                             (cons %row-index% %row-index%)
                             (cons (eql t) %row-index%)
                             (cons %row-index% (eql t))
                             (vector %row-index%)))

(define-condition <invalid-slice> (error) ())
(define-condition <invalid-row-slice> (<invalid-slice>) ())
(define-condition <invalid-col-slice> (<invalid-slice>) ())

(define-condition <invalid-index> (<invalid-slice>) ())
(define-condition <invalid-row-index> (<invalid-index>) ())
(define-condition <invalid-col-index> (<invalid-index>) ())
(define-condition <row-does-not-exist> (<invalid-row-index>) ())
(define-condition <col-does-not-exist> (<invalid-col-index>) ())

(define-condition <insert-error> (error) ())
(define-condition <col-name-not-unique> (<insert-error>) ())
(define-condition <row-length-mismatch> (<insert-error>) ())

(defun normalize-row-index (df row-index)
  (typecase row-index
    ((integer 0)
     (if (array-in-bounds-p (rows df) row-index)
         row-index
         (error '<row-does-not-exist>)))
    (sequence
     (or (position row-index (rows df))
         (error '<row-does-not-exist>)))
    (otherwise
     (error '<invalid-row-index>))))

(defun normalize-row-spec (df row-spec)
  "Returns the row (not its index)."
  (typecase row-spec
    ;; Undocumented hack to map over col names.
    (null
     (cols df))
    (sequence
     ;; It is left undefined whether we error when the row spec is a
     ;; sequence which is not part of a data frame. -- jd 2019-11-11
     #+ (or)
     (unless (find row-spec (rows df))
       (error '<row-does-not-exist>))
     row-spec)
    ((integer 0)
     (if (array-in-bounds-p (rows df) row-spec)
         (elt (rows df) row-spec)
         (error '<row-does-not-exist>)))
    (otherwise
     (error '<invalid-row-index>))))

(defun normalize-col-index (df col-index)
  (typecase col-index
    (string
     (or (position col-index (cols df) :test #'string=)
         (error '<col-does-not-exist>)))
    ((integer 0)
     (if (array-in-bounds-p (cols df) col-index)
         col-index
         (error '<col-does-not-exist>)))
    (otherwise
     (error '<invalid-col-index>))))

(defun normalize-row-slice (df row-slice)
  (typecase row-slice
    ((eql t)
     (cons 0 (1- (length (rows df)))))
    (cons
     (let ((beg (car row-slice))
           (end (cdr row-slice)))
       (if (eql beg t)
           (setf beg 0)
           (setf beg (normalize-row-index df beg)))
       (if (eql end t)
           (setf end (1- (length (rows df))))
           (setf end (normalize-row-index df end)))
       (if (and (<= beg end))
           (cons beg end)
           (error '<invalid-row-slice>))))
    (vector
     (map 'vector (curry #'normalize-row-index df) row-slice))
    (atom
     (let ((index (normalize-row-index df row-slice)))
       (cons index index)))))

(defun normalize-col-slice (df col-slice)
  (typecase col-slice
    ((eql t)
     (cons 0 (1- (length (cols df)))))
    (cons
     (let ((beg (car col-slice))
           (end (cdr col-slice)))
       (if (eql beg t)
           (setf beg 0)
           (setf beg (normalize-col-index df beg)))
       (if (eql end t)
           (setf end (1- (length (cols df))))
           (setf end (normalize-col-index df end)))
       (if (<= beg end)
           (cons beg end)
           (error '<invalid-col-slice>))))
    (vector
     (map 'vector (curry #'normalize-col-index df) col-slice))
    (atom
     (let ((index (normalize-col-index df col-slice)))
       (cons index index)))))

(defgeneric dims (<data-frame>)
  (:method ((df <raw-data-frame>))
    (values (length (rows df))
            (length (cols df)))))

;;; Methods ROWS and COLS for <RAW-DATA-FRAME> are defined as readers
;;; in the class definition.
(defgeneric cols (<data-frame>))
(defgeneric rows (<data-frame>))

(defgeneric ref (<data-frame> %row-spec% %col-index%)
  (:method ((df <raw-data-frame>) row col)
    (elt (normalize-row-spec df row)
         (normalize-col-index df col))))

(defgeneric sel (<data-frame> %row-slice% %col-slice%)
  (:method ((df <raw-data-frame>) row-slice col-slice)
    (setf row-slice (normalize-row-slice df row-slice)
          col-slice (normalize-col-slice df col-slice))
    (let* ((row-num (if (consp row-slice)
                        (1+ (- (cdr row-slice) (car row-slice)))
                        ;; vector
                        (length row-slice)))
           (col-num (if (consp col-slice)
                        (1+ (- (cdr col-slice) (car col-slice)))
                        ;; vector
                        (length col-slice)))
           (new-rows (make-array row-num :adjustable t :fill-pointer t))
           (new-cols (make-array col-num :adjustable t :fill-pointer t))
           (index 0))
      (map-data-frame-cols df nil col-slice
                           (lambda (col-index col-name value)
                             (declare (ignore col-index value))
                             (setf (elt new-cols index) col-name)
                             (incf index)))
      (setf index 0)
      (flet ((copy-row (src dst &aux (index 0))
               (map-data-frame-cols df src col-slice
                                    (lambda (col-index col-name value)
                                      (declare (ignore col-index col-name))
                                      (setf (elt dst index) value)
                                      (incf index)))))
        (map-data-frame-rows df row-slice
                             (lambda (row-index row)
                               (declare (ignore row-index))
                               (let ((new-row (make-array col-num :adjustable t
                                                                  :fill-pointer t)))
                                 (copy-row row new-row)
                                 (setf (elt new-rows index) new-row)
                                 (incf index)))))
      (<raw-data-frame> :cols new-cols :rows new-rows))))

(defgeneric map-data-frame (<data-frame> %row-slice% %col-slice% function)
  (:method ((df <raw-data-frame>) row-slice col-slice fun)
    (map-data-frame-rows
     df row-slice
     (lambda (row-index row)
       (map-data-frame-cols
        df row col-slice
        (lambda (col-index col-name value)
          (funcall fun row-index row col-index col-name value)))))))

(defgeneric map-data-frame-rows (<data-frame> %row-slice% function)
  (:method ((df <raw-data-frame>) row-slice fun)
    (setf row-slice (normalize-row-slice df row-slice))
    (let ((rows (rows df)))
      (if (consp row-slice)
          (loop for index from (car row-slice) upto (cdr row-slice)
                for row = (elt rows index)
                do (funcall fun index row))
          (loop for index across row-slice
                for row = (elt rows index)
                do (funcall fun index row))))))

(defgeneric map-data-frame-cols (<data-frame> %row-spec% %col-slice% function)
  (:method ((df <raw-data-frame>) row col-slice fun)
    (setf col-slice (normalize-col-slice df col-slice)
          row (normalize-row-spec df row))
    (let ((cols (cols df)))
      (if (consp col-slice)
          (loop with i1 = (car col-slice)
                with i2 = (cdr col-slice)
                for index from i1 upto i2
                for name = (elt cols index)
                for value = (elt row index)
                do (funcall fun index name value))
          (loop for col-index across col-slice
                for name = (elt cols col-index)
                for value = (elt row col-index)
                do (funcall fun col-index name value))))))

(defgeneric add-rows! (<data-frame> &rest rows)
  (:method ((df <raw-data-frame>) &rest rows)
    (loop with len = (length (cols df))
          for row in rows
          for new-row = (copy-array (coerce row 'vector)
                                    :adjustable t
                                    :fill-pointer t)
          do (assert (sequence-of-length-p new-row len)
                     nil
                     '<row-length-mismatch>)
             (vector-push-extend new-row (rows df)))))

(defgeneric add-cols! (<data-frame> &rest name-fun-pairs)
  (:method ((df <raw-data-frame>) &rest name-fun-pairs)
    (loop for (name fun) on name-fun-pairs by #'cddr
          do (assert (not (find name (cols df) :test #'string=))
                     nil
                     '<col-name-not-unique>)
             (vector-push-extend name (cols df))
             (loop for ind from 0
                   for row across (rows df)
                   for val = (funcall fun ind row)
                   do (vector-push-extend val row)))))

(defun make-data-frame (cols &rest rows)
  (let* ((new-cols (copy-array (coerce cols 'vector)
                               :adjustable t
                               :fill-pointer t))
         (new-rows (make-array (length rows)
                               :adjustable t
                               :fill-pointer t))
         (col-width (length new-cols))
         (index 0))
    (assert (every #'stringp cols) nil '<invalid-col-index>)
    (assert (= col-width (length (remove-duplicates cols :test #'string=)))
            nil
            '<col-name-not-unique>)
    (map nil (lambda (row)
               (setf row (copy-array (coerce row 'vector)
                                     :element-type t
                                     :adjustable t
                                     :fill-pointer t))
               (assert (sequence-of-length-p row col-width)
                       nil
                       '<row-length-mismatch>)
               (setf (elt new-rows index) row)
               (incf index))
         rows)
    (<raw-data-frame> :cols new-cols :rows new-rows)))

(defgeneric copy-data-frame (data-frame)
  (:method ((df <data-frame>))
    (let* ((src (rows df))
           (rows (make-array (length src)
                             :adjustable t
                             :fill-pointer t))
           (cols (copy-array (cols df)))
           (index 0))
      (map nil (lambda (row)
                 (setf (aref rows index) (copy-array row))
                 (incf index))
           src)
      (<raw-data-frame> :cols cols :rows rows))))

(defgeneric join-data-frame (df1 df2 &rest args)
  (:method ((df1 <data-frame>) (df2 <data-frame>) &rest args)
    (declare (ignore df1 df2 args))
    (error "Not specified yet.")))

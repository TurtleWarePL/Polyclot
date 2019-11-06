
(in-package #:eu.turtleware.polyclot)

;;; This is quite barebone implementation of "data frames". Many
;;; functions which would be expected from such abstraction are simply
;;; not implemented unless we've needed them.

(define-class <data-frame> ()
  ((cols :initarg :cols :type (vector symbol) :accessor cols :documentation "Column names.")
   (rows :initarg :rows :type (vector symbol) :accessor rows :documentation "Row names.")
   (data :initarg :data :type vector :accessor data :documentation "Records."))
  (:documentation "Raw data frame."))

(defun map-data-frame (df fun)
  (loop
     for row across (rows df)
     for elt across (data df)
     do (loop
           for col across (cols df)
           for val across elt
           do (funcall fun df row elt col val))))

(defmacro do-data-frame ((df row col value) &body body)
  `(flet ((thunk (,row ,col ,value)
            ,@body))
     (map-data-frame ,df #'thunk)))

(defun ref (data-frame row col)
  (etypecase row
    (string (setf row (position row (rows data-frame))))
    (integer #| do nothing |#))
  (etypecase col
    (string (setf col (position col (cols data-frame))))
    (integer #| do nothing |#))
  (svref (aref (data data-frame) row) col))

(defun sel (data-frame rows cols)
  (when (and (eql rows t)
             (eql cols t))
    (return-from sel data-frame))
  (map-rows data-frame rows
            (lambda (row-name row-data)
              (map-cols row-data cols
                        (lambda (col-name value)
                          )))))


(defun add-row (data-frame name row)
  (unless (length= row (cols data-frame))
    (error "Row arity must be ~s." (length (cols data-frame))))
  (prog1 data-frame
    (vector-push-extend name (rows data-frame))
    (vector-push-extend row  (data data-frame))))

(defun add-col (data-frame name fun)
  (vector-push-extend name (vcols data-frame))
  (vector-push-extend col  (vdata data-frame)))

(defun map-rows (data-frame filter function)
  (let ((filter-spec (slot-value df 'row-filter)))
    (etypecase filter
      ;; everything
      ((eql t)
       (lambda (fun)
         (loop
            for row across (rows data-frame)
            for elt across (data data-frame)
            do (funcall function row elt))))
      ;; single element
      ((or integer string)
       (lambda (fun)
         (let* ((raw-df (raw-data-frame df))
                (raw-index (funcall (row-deref df) filter))
                (row (aref (rows raw-df) raw-index))
                (elt (aref (data raw-df) raw-index)))
           (funcall fun row elt))))
      ;; subsequences (a1 a2 b1 b2 …)
      (list
       (lambda (fun)
         (loop
            with raw-df = (raw-data-frame df)
            with row-deref = (row-deref df)
            for (x1 x2) on filter by #'cddr
            do (loop
                  for index from x1 to x2
                  for raw-index = (funcall row-deref index)
                  for row = (aref (rows raw-df) raw-index)
                  for elt = (aref (data raw-df) raw-index)
                  do (funcall fun row elt)))))
      ;; indexes
      (vector
       (lambda (fun)
         (loop
            with raw-df = (raw-data-frame df)
            with row-deref = (row-deref df)
            for i across filter
            for raw-index = (funcall row-deref i)
            for row = (aref (rows raw-df) raw-index)
            for elt = (aref (data raw-df) raw-index)
            do (funcall fun row elt))))
      ;; custom predicate
      (function
       (lambda (fun)
        (loop
           with raw-df = (raw-data-frame df)
           for row across (rows raw-df)
           for elt across (data raw-df)
           when (funcall filter df row elt)
           do (funcall fun row elt)))))))

(define-unbound ((df <filtered-data-frame>) col-mapper)
  (let ((filter (slot-value df 'col-filter)))
    (etypecase filter
      ;; everything
      ((eql t)
       (lambda (row elt fun)
         (loop
            with raw-df = (raw-data-frame df)
            for col across (cols raw-df)
            for val across elt
            do (funcall fun col val))
         (loop
            for col across (vcols df)
            for vfn across (vdata df)
            do (funcall fun col (funcall vfn df row elt)))))
      ;; single element
      ((or integer string)
       (lambda (row elt fun)
         (let* ((raw-df (raw-data-frame df))
                (raw-index (funcall (col-deref df) filter))
                (max-index (length (cols raw-df))))
           (if (< raw-index max-index)
               (let ((col (aref (cols raw-df) raw-index))
                     (val (aref elt raw-index)))
                 (funcall fun col val))
               (let* ((virt-index (- raw-index max-index))
                      (col (aref (vcols df) virt-index))
                      (vfn (aref (vdata df) virt-index)))
                 (funcall fun col (funcall vfn df row elt)))))))
      ;; subsequences (a1 a2 b1 b2 …)
      (list
       (lambda (row elt fun)
         (loop
            with raw-df = (raw-data-frame df)
            with col-deref = (col-deref df)
            with max-index = (length (cols raw-df))
            for (x1 x2) on filter by #'cddr
            do
              (loop
                 for index from x1 to x2
                 for raw-index = (funcall col-deref index)
                 do (if (< raw-index max-index)
                        (let ((col (aref (cols raw-df) raw-index))
                              (val (aref elt raw-index)))
                          (funcall fun col val))
                        (let* ((virt-index (- raw-index max-index))
                               (col (aref (vcols df) virt-index))
                               (vfn (aref (vdata df) virt-index)))
                          (funcall fun col (funcall vfn df row elt))))))))
      ;; indexes
      (vector
       (lambda (row elt fun)
         (loop
            with raw-df = (raw-data-frame df)
            with col-deref = (col-deref df)
            with max-index = (length (cols raw-df))
            for index across filter
            for raw-index = (funcall col-deref index)
            do (if (< raw-index max-index)
                   (let ((col (aref (cols raw-df) raw-index))
                         (val (aref elt raw-index)))
                     (funcall fun col val))
                   (let* ((virt-index (- raw-index max-index))
                          (col (aref (vcols df) virt-index))
                          (vfn (aref (vdata df) virt-index)))
                     (funcall fun col (funcall vfn df row elt)))))))
      ;; custom predicate
      (function
       (lambda (row elt fun)
        (loop
           with raw-df = (raw-data-frame df)
           for col across (cols raw-df)
           for val across elt
           when (funcall filter row elt col val)
           do (funcall fun col val))
        (loop
           for col across (vcols df)
           for vfn across (vdata df)
           for val = (funcall vfn df row elt)
           when (funcall filter row elt col val)
           do (funcall fun col val)))))))

(define-unbound ((df <filtered-data-frame>) row-deref)
  (let ((filter (slot-value df 'row-filter))
        (parent-deref (if (eql (raw-data-frame df)
                               (parent-data-frame df))
                          #'identity
                          (slot-value (parent-data-frame df) 'row-deref))))
    ;; index is always integer
    (etypecase filter
      ((eql t)
       parent-deref)
      ((or integer string)
       (lambda (index)
         (etypecase index
           (integer
            (if (zerop index)
                (funcall parent-deref filter)
                'not-available))
           (string
            (let ((zero-index (funcall parent-deref filter)))
              (if (= (funcall parent-deref index) zero-index)
                  zero-index
                  'not-available))))))
      (list
       (lambda (index)
         (etypecase index
           (integer
            (loop
               for (i j) on filter by #'cddr
               for first-index = 0 then (1+ last-index)
               for last-index = (+ first-index (- j i))
               do (cond ((< index first-index)
                         (return 'not-available))
                        ((= index first-index)
                         (return (funcall parent-deref i)))
                        ((= index last-index)
                         (return (funcall parent-deref j)))
                        ((<= index last-index)
                         (return (funcall parent-deref
                                          (+ i (- index first-index)))))
                        ;; (> index last-index), continue search
                        (T NIL))))
           (string
            (let ((parent-index (funcall parent-deref index)))
              (loop
                 for (i j) on filter by #'cddr
                 for first-index = (funcall parent-deref i)
                 for last-index = (funcall parent-deref j)
                 when (<= first-index parent-index last-index)
                 return parent-index
                 finally (return 'not-available)))))))
      (vector
       (lambda (index)
         (etypecase index
           (integer
            (if (< index (length filter))
                (funcall parent-deref (aref filter index))
                'not-available))
           (string
            (let ((parent-index (funcall parent-deref index)))
              (loop
                 for i across filter
                 when (= (funcall parent-deref i) parent-index)
                 return parent-index
                 finally (return 'not-available)))))))
      (function
       (lambda (index)
        )))))

(define-unbound ((df <filtered-data-frame>) col-deref)
  (let ((filter (slot-value df 'col-filter)))
    (etypecase filter
      ((eql t)
       #'identity)
      ((or integer string)
       (lambda (index)))
      (list
       (lambda (index)))
      (vector
       (lambda (index)))
      (function
       (lambda (index))))))

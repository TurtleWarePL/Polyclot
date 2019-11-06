
(in-package #:eu.turtleware.polyclot)

;;; This is quite barebone implementation of "data frames". Many
;;; functions which would be expected from such abstraction are simply
;;; not implemented unless we've needed them.

(define-class <data-frame> () ())
(defmethod initialize-instance :before ((df <data-frame>) &rest args)
  (declare (ignore args))
  (when (eql (class-of df) <data-frame>)
    (error "Trying to make an instance of a protocol class <DATA-FRAME>.")))

(define-class <raw-data-frame> (<data-frame>)
  ((cols :initarg :cols :type (vector string) :accessor cols :documentation "Column names.")
   (rows :initarg :rows :type (vector string) :accessor rows :documentation "Row names.")
   (data :initarg :data :type vector :accessor data :documentation "Records."))
  (:documentation "Raw data frame."))

(define-class <filtered-data-frame> (<data-frame>)
  ((rdf :initarg :data-frame :type <raw-data-frame> :reader raw-data-frame)
   (pdf :initarg :data-frame :type <data-frame> :reader parent-data-frame)
   (vcols :type (vector string)   :accessor vcols)
   (vdata :type (vector function) :accessor vdata)
   (row-filter :initarg :row-filter :reader row-filter)
   (col-filter :initarg :col-filter :reader col-filter)
   (row-mapper :type function :reader row-mapper)
   (col-mapper :type function :reader col-mapper)
   (row-deref  :type function :reader row-deref)
   (col-deref  :type function :reader col-deref))
  (:documentation "Filtered data frame. Doesn't cons."))

(defgeneric map-data-frame (df fun)
  (:method ((df <raw-data-frame>) fun)
    (loop
       for row across (rows df)
       for elt across (data df)
       do (loop
             for col across (cols df)
             for val across elt
             do (funcall fun df row elt col val))))
  (:method ((df <filtered-data-frame>) fun)
    (funcall (row-mapper df)
             (lambda (row elt)
               (funcall (col-mapper df)
                        row elt
                        (lambda (col val)
                          (funcall fun df row elt col val)))))))

(defmacro do-data-frame ((df row col value) &body body)
  `(flet ((thunk (,row ,col ,value)
            ,@body))
     (map-data-frame ,df #'thunk)))

(defgeneric ref (<data-frame> row col)
  (:documentation "Selects a single element indexed by row and col.")
  (:method ((data-frame <raw-data-frame>) row col)
    (etypecase row
      (string (setf row (position row (rows data-frame) :test #'string=)))
      (integer #| do nothing |#))
    (etypecase col
      (string (setf col (position col (cols data-frame) :test #'string=)))
      (integer #| do nothing |#))
    (svref (aref (data data-frame) row) col))
  (:method ((df <filtered-data-frame>) row col)
    (ref (parent-data-frame df)
         (funcall (row-deref df) row)
         (funcall (col-deref df) col))))

(defgeneric sel (<data-frame> rows cols)
  (:documentation "Creates a data-frame subset by selected by rows and cols.")
  (:method ((df <raw-data-frame>) rows cols)
    (check-type rows (or (eql t) list vector function))
    (check-type cols (or (eql t) list vector function))
    (<filtered-data-frame> :data-frame df :row-filer rows :col-filter cols))
  (:method ((df <filtered-data-frame>) rows cols)
    (declare (ignore df rows cols))
    (error "Nested filters are not implemented (yet?)!")))

#+ (or)
(defgeneric flatten (<data-frame>)
  (:method ((df <raw-data-frame)) df)
  (:method ((df <filtered-data-frame>))
    (<raw-data-frame> :cols (cols df)
                      :rows (rows df)
                      :data (data df))))
#+ (or)
(defgeneric sort-df (<data-frame> &rest cols))

(defgeneric add-row (<data-frame> name row)
  (:method ((data-frame <raw-data-frame>) (name string) (row vector))
    (unless (length= row (cols data-frame))
      (error "Row arity must be ~s." (length (cols data-frame))))
    (prog1 data-frame
      (vector-push-extend name (rows data-frame))
      (vector-push-extend row  (data data-frame))))
  (:method ((data-frame <filtered-data-frame>) (name string) (row vector))
    (let ((raw-df (raw-data-frame data-frame)))
      (unless (length= row (cols raw-df))
        (error "Row arity must be ~s." (length (cols raw-df))))
      (prog1 data-frame
        (vector-push-extend name (rows raw-df))
        (vector-push-extend row  (data raw-df))))))

(defgeneric add-col (<data-frame> name fun)
  (:method ((data-frame <raw-data-frame>) (name string) (col function))
    (let ((fdf (<filtered-data-frame> :df data-frame :row-filter t :col-filter t)))
      (add-col fdf name col)))
  (:method ((data-frame <filtered-data-frame>) (name string) (col function))
    (vector-push-extend name (vcols data-frame))
    (vector-push-extend col  (vdata data-frame))))

(defmacro define-unbound (((var class) slot-name) &body body)
  (with-gensyms (class-var slot-name-var)
    `(defmethod slot-unbound ((,class-var ,class) ,var (,slot-name-var (eql ',slot-name)))
       (setf (slot-value ,var ,slot-name-var)
             (progn ,@body)))))

(define-unbound ((df <filtered-data-frame>) vcols)
  (make-array '(4)
              :element-type 'string
              :adjustable t
              :initial-element ""))

(define-unbound ((df <filtered-data-frame>) vdata)
  (make-array '(4)
              :element-type 'function
              :adjustable t
              :initial-element (constantly 42)))

(define-unbound ((df <filtered-data-frame>) row-mapper)
  (let ((filter-spec (slot-value df 'row-filter)))
    (etypecase filter-spec
      ;; everything
      ((eql t)
       (lambda (fun)
         (loop
            with raw-df = (raw-data-frame df)
            for row across (rows raw-df)
            for elt across (data raw-df)
            do (funcall fun row elt))))
      ;; single element
      ((or integer string)
       (lambda (fun)
         (let* ((raw-df (raw-data-frame df))
                (raw-index (funcall (row-deref df) filter-spec))
                (row (aref (rows raw-df) raw-index))
                (elt (aref (data raw-df) raw-index)))
           (funcall fun row elt))))
      ;; subsequences (a1 a2 b1 b2 …)
      (list
       (lambda (fun)
         (loop
            with raw-df = (raw-data-frame df)
            with row-deref = (row-deref df)
            for (x1 x2) on filter-spec by #'cddr
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
            for i across filter-spec
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
           when (funcall filter-spec df row elt)
           do (funcall fun row elt)))))))

(define-unbound ((df <filtered-data-frame>) col-mapper)
  (let ((filter-spec (slot-value df 'col-filter)))
    (etypecase filter-spec
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
                (raw-index (funcall (col-deref df) filter-spec))
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
            for (x1 x2) on filter-spec by #'cddr
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
            for index across filter-spec
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
           when (funcall filter-spec row elt col val)
           do (funcall fun col val))
        (loop
           for col across (vcols df)
           for vfn across (vdata df)
           for val = (funcall vfn df row elt)
           when (funcall filter-spec row elt col val)
           do (funcall fun col val)))))))

(define-unbound ((df <filtered-data-frame>) row-deref)
  (let ((filter-spec (slot-value df 'row-filter))
        (parent-deref (if (eql (raw-data-frame df)
                               (parent-data-frame df))
                          #'identity
                          (slot-value (parent-data-frame df) 'row-deref))))
    ;; index is always integer
    (etypecase filter-spec
      ((eql t)
       parent-deref)
      ((or integer string)
       (lambda (index)
         (etypecase index
           (integer
            (if (zerop index)
                (funcall parent-deref filter-spec)
                'not-available))
           (string
            (let ((zero-index (funcall parent-deref filter-spec)))
              (if (= (funcall parent-deref index) zero-index)
                  zero-index
                  'not-available))))))
      (list
       (lambda (index)
         (etypecase index
           (integer
            (loop
               for (i j) on filter-spec by #'cddr
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
                 for (i j) on filter-spec by #'cddr
                 for first-index = (funcall parent-deref i)
                 for last-index = (funcall parent-deref j)
                 when (<= first-index parent-index last-index)
                 return parent-index
                 finally (return 'not-available)))))))
      (vector
       (lambda (index)
         (etypecase index
           (integer
            (if (< index (length filter-spec))
                (funcall parent-deref (aref filter-spec index))
                'not-available))
           (string
            (let ((parent-index (funcall parent-deref index)))
              (loop
                 for i across filter-spec
                 when (= (funcall parent-deref i) parent-index)
                 return parent-index
                 finally (return 'not-available)))))))
      (function
       (lambda (index)
        )))))

(define-unbound ((df <filtered-data-frame>) col-deref)
  (let ((filter-spec (slot-value df 'col-filter)))
    (etypecase filter-spec
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

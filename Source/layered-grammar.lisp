(in-package #:eu.turtleware.polyclot.impl)

(define-class <aest> ()
  (;; mapping
   (aest :initarg :aest)
   (vars :initarg :vars)
   ;; slots used for caching
   (last-df :initform nil)
   (indexes :initform nil)
   ;; handling of (enum variable)
   (enums :initform nil)))

(define-class <layered-grammar-component> ()
  ((aest :initarg :aest)
   (data :initarg :data)))

(define-class <stat> (<layered-grammar-component>) ())
(define-class <mods> (<layered-grammar-component>) ())
(define-class <geom> (<layered-grammar-component>) ())

(define-class <stat-identity> (<stat>) ())
(define-class <mods-identity> (<mods>) ())
(define-constant <identity> '<identity>)

;;; FIXME stubs for now
(define-class <scale> () ())
(define-class <coord> () ())
(define-class <facet> () ())

(define-class <layer> (<layered-grammar-component>)
  ((stat :initarg :stat)
   (geom :initarg :geom)
   (mods :initarg :mods)))

(define-class <chart> (<layer>)
  ((coord  :initarg :coord)
   (scale  :initarg :scale)
   (facet  :initarg :facet)
   (layers :initarg :layers)))

(defmethod initialize-instance :after ((object <chart>) &key layers)
  (unless layers
    (setf (slot-value object 'layers)
          (list object))))

(defun map-aesthetics (aest df row)
  (with-slots (aest vars last-df indexes enums) aest
    (unless (eql last-df df)
      (setf last-df df
            indexes nil
            enums nil)
      (do* ((vars vars (cdr vars))
            (var #1=(car vars) #1#))
           ((null vars)
            (setf indexes (nreverse indexes)
                  enums   (nreverse enums)))
        (etypecase var
          (string
           (push nil enums)
           (push (position var (cols df) :test #'string=) indexes))
          ((cons (eql :enum)
                 (cons string null))
           (push (make-hash-table :test #'equal) enums)
           (push (position (second var) (cols df) :test #'string=) indexes)))))
    (loop for aes in aest
          for enu in enums
          for ind in indexes
          collect aes
          if (not enu)
            collect (ref df row ind)
          else
            collect (ensure-gethash (ref df row ind)
                                    enu
                                    (hash-table-count enu)))))

(defgeneric collision-modifier (<mods> last vals)
  (:method ((mods <mods-identity>) last vals)
    (declare (ignore last))
    vals))

(defgeneric statistical-transformation (<stat> <data-frame>)
  (:method ((stat <stat-identity>) (frame <data-frame>))
    (declare (ignore stat))
    frame))

(defgeneric geometric-object-start  (<geom> <coord> <mods>))
(defgeneric geometric-object-add    (<geom> &rest args))
(defgeneric geometric-object-finish (<geom>))

(defmacro aest (&rest args)
  (if (sequence-of-length-p args 1)
      `(slot-value ,(first args) 'aest)
      `(loop for (i j) on ',args by #'cddr
             collect i into aes
             collect j into var
             finally (return (<aest> :aest aes :vars var)))))

(defmacro stat (class &rest args)
  (case class
    (<identity> `(<stat-identity> ,@args))
    (otherwise  `(,class ,@args))))

(defmacro mods (class &rest args)
  (case class
    (<identity> `(<mods-identity> ,@args))
    (otherwise  `(,class ,@args))))

(defmacro geom (class &rest args)
  (case class
    (otherwise `(,class ,@args))))

;;; FIXME macro stub
(defmacro define-chart (name options &rest layers)
  (declare (ignore name options layers)))

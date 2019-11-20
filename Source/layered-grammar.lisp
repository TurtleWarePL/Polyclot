(in-package #:eu.turtleware.polyclot.impl)

(define-class <aest> ()
  (;; mapping
   (aest :initarg :aest)
   (vars :initarg :vars)
   ;; slots used for caching
   (last-df :initform nil)
   (indexes :initform nil)))

(define-class <layered-grammar-component> ()
  ((aest :initarg :aest)))

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

(define-class <layer> ()
  ((data :initarg :data)
   (aest :initarg :aest)
   (stat :initarg :stat)
   (geom :initarg :geom)
   (mods :initarg :mods)))

(define-class <chart> (<layer>)
  ((coord :initarg :coord)
   (scale :initarg :scale)
   (facet :initarg :facet)
   (layer :initarg :layer)))

(defun map-aesthetics (aest df row)
  (with-slots (aest vars last-df indexes) aest
    (unless (eql last-df df)
      (setf last-df df
            indexes (map 'list
                         (lambda (col)
                           (position col (cols df) :test #'string=))
                         vars)))
    (loop for aes in aest
          for ind in indexes
          collect aes
          collect (ref df row ind))))

;;; CAUTION modifies a dataframe row
(defun unmap-aesthetics! (aest df row vals)
  (with-slots (aest vars last-df indexes) aest
    (unless (eql last-df df)
      (setf last-df df
            indexes (map 'list
                         (lambda (col)
                           (position col (cols df) :test #'string=))
                         vars)))
    (loop for aes in aest
          for ind in indexes
          for val = (getf vals aes)
          do (setf (elt row ind) val))))

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

(defun aest (&rest plist)
  (if (alexandria:sequence-of-length-p plist 1)
      (slot-value (pop plist) 'aest)
      (loop for (i j) on plist by #'cddr
            collect i into aes
            collect j into var
            finally (return (<aest> :aest aes :vars var)))))

(defun stat (class &rest args)
  (case class
    (<identity> (apply <stat-identity> args))
    (apply class args)))

(defun mods (class &rest args)
  (case class
    (<identity> (apply <mods-identity> args))
    (apply class args)))

(defun geom (class &rest args)
  (apply class args))

;;; FIXME macro stub
(defmacro define-chart (name options &rest layers)
  (declare (ignore name options layers)))

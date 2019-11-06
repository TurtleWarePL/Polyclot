(in-package #:eu.turtleware.polyclot)

(define-class <axis> ()
  ((label :initarg :label :documentation "Axis label")
   (min   :initarg :min   :documentation "Axis minimum value")
   (max   :initarg :max   :documentation "Axis maximum value")
   (major :initarg :major :documentation "Major ticks")
   (minor :initarg :minor :documentation "Minor ticks")
   (step  :initarg :step  :documentation "Step (for <fun-serie>)")
   (scale :initarg :scale :documentation "Scaling function"))
  (:documentation "Description of the axis (serie domain)."))

;;; Who doesn't love good old hackery?
(define-class <boring-serie-plot> ()
  ;; We may have different axis for the same dimension across multiple
  ;; series. Otherwise this would be a slot in <chart>.
  ((axes #|:type (sequence <axis>)|# :accessor axes))
  (:documentation "Mixed in for visual representation.")
  (:stealth-mixin <boring-serie>))

(define-class <chart> ()
  ((series :initarg :series :accessor series)))

(defmethod initialize-instance :after ((chart <chart>) &key series axes)
  (cond ((null series)
         (error "<chart> with no series can't be created.")
         #+ (or) (return-from initialize-instance))
        (axes
         (mapc (lambda (s) (setf (axes s) axes)) series)
         (return-from initialize-instance)))
  ;; If axes were not supplied we take a minimal area which can hold
  ;; all series (in linear scale). This code is general enough to
  ;; compute N-dimensional series.
  ;;
  ;; Initial values from the first serie.
  (let* ((serie-1 (first series))
         (value-1 (first (ensure-list (vals serie-1)))))
    (setf axes (mapcar (lambda (val)
                         (<axis> :label "axis-label" :min val :max val))
                       (ensure-list value-1))))
  (loop
     for serie in series
     do (setf (axes serie) axes)
     do (loop
           for v in (ensure-list (vals serie))
           do (mapc (lambda (axis value)
                      (minf (slot-value axis 'min) value)
                      (maxf (slot-value axis 'max) value))
                    (axes serie)
                    (ensure-list v)))))

;;; 0d series. Presents a single pie chart with all the values. If
;;; initarg :total is not supplied then all values sum up to
;;; 2pi. Otherwise TOTAL is 2pi and all values must sum up to this
;;; number (or below).
(define-class <pie-chart> (<chart>)
  ((total :initarg :total :type (real (0)))))

;;; 0d series. Presents a single bar chart.
(define-class <bar-chart> (<chart>)
  ((orientation :initarg :orientation :initform :vertical
                :type (member :vertical :horizontal)
                :reader orientation)))

;;; 0-dimensional series. It may be used to draw a line on a chart to
;;; signify some particular values.
(define-class <guide> (<chart>)
  ((orientation :initarg :orientation :initform :vertical
                :type (member :vertical :horizontal))))

;;; 1d series. Initarg :bins is a list of numbers [-inf,1st],
;;; [1st,2nd], ..., [nth,infty]. Defaults to an equal-width sectioning
;;; from min to max among all series with 8 slices.
(define-class <histogram> (<chart>)
  ((bins :initarg :bins :initform :auto
         :type (or keyword sequence))))

;;; 1d series. Initarg :interpolation is one of (:none :line :curve).
(define-class <time-series> (<chart>)
  ((interpolation :initarg :interpolate :initform :line
                  :type (member :none :line :curve))))

;;; Series associated with this chart must be 2-dimensional.
(define-class <scatterplot> (<chart>) ())

;;; Series associated with this chart must be 3-dimensional. This will
;;; make a nice demo but is inpractical given CLIM drawing primitives.
#+ (or) (define-class <scatterplot-3d> (<chart>) ())

;;; Series associated with this chart must be 3-dimensional. XY denote
;;; position and Z denotes the result color.
(define-class <heat-map> (<chart>) ())

;;; For each serie a different chart kind is used (that means that
;;; multi-chart may plot data with different dimensionality or place
;;; multiple charts side-by-side).
(define-class <mixed-chart> (<chart>)
  ((charts :initarg :charts)))

;;; Series associated with this chart must be 1-dimensional. Each
;;; result for a serie is put in a separate bar chart. Initarg names
;;; allows labelling separate bar groups.
(define-class <multi-bar-chart> (<chart>)
  ((group-names :initarg :names)
   (orientation :initarg :orientation :initform :vertical
                :type (member :vertical :horizontal))))

(define-class <multi-bar-chart-2> (<multi-bar-chart>)
  ())

;;; Series associated with this chart must be 2-dimensional.
(define-class <2d-bar-chart> (<chart>)
  ((group-names :initarg :group-names)
   (major-names :initarg :major-names)))

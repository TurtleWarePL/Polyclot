(in-package #:eu.turtleware.polyclot)

;;; Function draws a grid, axes, decorations and prepares graphical
;;; context for drawing series. It should call DRAW-SERIES.
(defgeneric draw-chart (pane chart))

;;; Function draws series. It may either draw them in one go or call
;;; DRAW-SERIE for each. Drawing area is normalized to range [0;1].
(defgeneric draw-series (pane chart))

;;; Function is responsible for plotting a single serie of
;;; data. Drawing area is normalized to range [0;1], function should
;;; do any necessary math to scale values accordingly to data axes.
(defgeneric draw-serie (pane chart serie axes))

;;; Class is responsible for recording chart output. Once created it
;;; may be replayed on any pane which wants to display the chart.
(define-class <chart-record> (clim:standard-sequence-output-record)
  ((chart :initarg :chart :type <chart>)))

(defmethod initialize-instance :after ((record <chart-record>) &key chart
                                       &aux (stream nil))
  (clim:with-new-output-record
      (stream 'clim:standard-sequence-output-record rec :parent record)
    (setf (clim:stream-cursor-position stream) (values 25 25))
    (draw-chart stream chart)))

(defmethod draw-chart (pane (chart <chart>))
  (let ((width    360 #+ (or) (clim:space-requirement-width pane))
        (height   600 #+ (or) (clim:space-requirement-height pane))
        (padding-x 10 #+ (or) (clime:space-requirement-padding-x pane))
        (padding-y 10 #+ (or) (clime:space-requirement-padding-y pane)))
    (clim:with-room-for-graphics (pane :height height)
      ;(clim:draw-rectangle* pane 0 0 width height :filled nil :ink clim:+gray+)
      (clim:with-translation (pane padding-x padding-y)
        (let ((effective-width (- width (* 2 padding-x)))
              (effective-height (- height (* 2 padding-y))))
          (clim:with-scaling (pane effective-width effective-height)
            (draw-series pane chart)))))))

(defmethod draw-series (pane (chart <chart>))
  (let ((inks (coerce (clim:make-contrasting-inks (length (series chart))) 'list)))
    (dolist (serie (series chart))
      (let* ((axes (axes serie))
             (pres (clim:presentation-type-of serie)))
        (nest (clim:with-drawing-options (pane :ink (pop inks) :line-thickness 5))
              (clim:with-output-as-presentation (pane serie pres)
                (draw-serie pane chart serie axes)))))))


;;; 0d charts (I use draw-series because each serie position is
;;; dependent on the previous serie).

(defmethod draw-series (pane (chart <bar-chart>))
  (let* ((axis (first (axes (first (series chart)))))
         (bars (length (series chart)))
         (max (slot-value axis 'max))
         (scale-pos (/ 1 bars))
         (scale-val (/ 1 max)))
    (loop
       for ink across (clim:make-contrasting-inks bars)
       ;with ink = clim:+dark-gray+
       for bar-x from 0
       for serie in (series chart)
       do (ecase (orientation chart)
            (:vertical
             (clim:with-scaling (pane scale-pos scale-val)
                                (let ((x1 (+ bar-x .15))
                                      (x2 (+ bar-x .85))
                                      (y1 0)
                                      (y2 (vals serie)))
                                  (clim:draw-rectangle* pane x1 y1 x2 y2 :ink ink))))
            (:horizontal
             (clim:with-scaling (pane scale-val scale-pos)
                                (let ((y1 (+ bar-x .15))
                                      (y2 (+ bar-x .85))
                                      (x1 0)
                                      (x2 (vals serie)))
                                  (clim:draw-rectangle* pane x1 y1 x2 y2 :ink ink))))))))

(defmethod draw-series (pane (chart <pie-chart>))
  (let ((slices-no 0)
        (slice-total 0))
    (loop
       for slice in (series chart)
       do
         (incf slices-no)
         (incf slice-total (vals slice)))
    (loop
       for ink across (clim:make-contrasting-inks slices-no)
       for slice in (series chart)
       for start = 0 then (+ start range)
       for range = (* 2 pi (/ (vals slice) slice-total))
       do (clim:draw-circle* pane 0.5 0.5 0.4
                             :ink ink
                             :start-angle start
                             :end-angle (+ start range)))))


;;; 1d charts
(defmethod draw-serie (pane (chart <time-series>) serie axes)
  (assert (length= 1 axes))
  (let* ((axis (first axes))
         (min (slot-value axis 'min))
         (max (slot-value axis 'max))
         (scale-x (/ 1 (1- (length (vals serie)))))
         (scale-y (/ 1 (- max min)))
         (trans-x 0)
         (trans-y (- min)))
    (nest
     (clim:with-scaling (pane scale-x scale-y))
     (clim:with-translation (pane trans-x trans-y))
     (let ((points nil))
       (mapc (let ((x 0))
               (lambda (coords element)
                 (destructuring-bind (y) (ensure-list coords)
                   (clim:with-output-as-presentation
                       (pane element (clim:presentation-type-of element))
                     (clim:draw-point* pane x y))
                   (setf points (list* x y points))
                   (incf x))))
             (vals serie)
             (elts serie))
       (clim:draw-polygon* pane points :filled nil :closed nil :line-thickness 1)))))

(defmethod draw-series (pane (chart <multi-bar-chart>))
  (loop
     with scale = (/ 1 (length (series chart)))
     with padding = (/ 0.08 (length (series chart)))
     for serie in (series chart)
     for start = 0 then (+ start scale)
     do (ecase (orientation chart)
          (:vertical
           (nest
            (clim:with-translation (pane (+ start padding) 0))
            (clim:with-scaling (pane (- scale (* 2 padding)) 1))
            (let ((bar-chart (<bar-chart> :series (make-series (vals serie))
                                          :orientation (orientation chart))))
              (clim:draw-text* pane (name serie) 0 0 :align-x :right :align-y :top)
              (draw-series pane bar-chart))))
          (:horizontal
           (nest
            (clim:with-translation (pane 0 (+ start padding)))
            (clim:with-scaling (pane 1 (- scale (* 2 padding))))
            (let ((bar-chart (<bar-chart> :series (make-series (vals serie))
                                          :orientation (orientation chart))))
              (clim:draw-point* pane 0 0)
              (clim:draw-text* pane (name serie) 0 .5 :align-x :left :align-y :center
                               :text-size :small)
              (clim:with-translation (pane 1/2 0)
                                     (draw-series pane bar-chart))))))))

(defmethod draw-series (pane (chart <multi-bar-chart-2>))
  (loop
     with scale = (/ 1 (length (series chart)))
     with padding = (/ 0.08 (length (series chart)))
     for serie in (series chart)
     for start = 0 then (+ start scale)
     do (ecase (orientation chart)
          (:horizontal
           (nest
            (clim:with-translation (pane 0 (+ start padding)))
            (clim:with-scaling (pane 1 (- scale (* 2 padding))))
            (let ((bar-chart (<bar-chart> :series (make-series (vals serie))
                                          :orientation (orientation chart))))
              (draw-series pane bar-chart)))))))


;;; 2d charts
(defmethod draw-serie (pane (chart <scatterplot>) serie axes)
  (assert (length= 2 axes))
  (let* ((min-x (slot-value (nth 0 axes) 'min))
         (max-x (slot-value (nth 0 axes) 'max))
         (min-y (slot-value (nth 1 axes) 'min))
         (max-y (slot-value (nth 1 axes) 'max))
         (scale-x (/ 1 (- max-x min-x)))
         (scale-y (/ 1 (- max-y min-y)))
         (trans-x (- min-x))
         (trans-y (- min-y)))
    (nest
     (clim:with-scaling (pane scale-x scale-y))
     (clim:with-translation (pane trans-x trans-y))
     (mapc (lambda (coords element)
             (destructuring-bind (x y) coords
               (clim:with-output-as-presentation
                   (pane element (clim:presentation-type-of element))
                 (clim:draw-point* pane x y))))
           (vals serie)
           (elts serie)))))

(defmethod draw-series (pane (chart <2d-bar-chart>))
  (loop
     with scale = (/ 1 (length (series chart)))
     with padding = (/ 0.08 (length (series chart)))
     for serie in (series chart)
     for start = 0 then (+ start scale)
     do (nest
         (clim:with-translation (pane (+ start padding) 0))
         (clim:with-scaling (pane (- scale (* 2 padding)) 1))
         (let ((bar-chart (<multi-bar-chart> :series (make-series serie)
                                             :orientation :vertical)))
           (clim:draw-text* pane (name serie) 0.5 0 :align-x :center :align-y :top)
           (draw-series pane bar-chart)))))

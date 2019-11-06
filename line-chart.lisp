(in-package #:eu.turtleware.polyclot)

(defclass line-chart (clim:standard-sequence-output-record)
  ((series :initarg :series :type (sequence 2d-serie))))

;;; line chart


(defmethod initialize-instance :after ((chart line-chart) &key series
                                       &aux (stream nil))
  (clim:with-new-output-record
      (stream  'clim:standard-sequence-output-record rec :parent chart)
    (setf (clim:stream-cursor-position stream) (values 25 25))
    (clim:with-room-for-graphics (stream)
      (clim:draw-rectangle* stream 0 0 500 340 :filled nil :ink clim:+gray+) ; grid
      (clim:with-translation (stream 10 10)
        ;; first pass (min/max)
        (let ((min-x most-positive-long-float)
              (max-x most-negative-long-float)
              (min-y most-positive-long-float)
              (max-y most-negative-long-float))
          (do-series ((x y) elt serie series)
            :point (progn (minf min-x x)
                          (maxf max-x x)
                          (minf min-y y)
                          (maxf max-y y)))
          (clim:with-scaling (stream (/ 480 (- max-x min-x))
                                     (/ 320 (- max-y min-y)))
            (clim:with-translation (stream (- min-x) (- min-y))
              (clim:draw-line* stream min-x 0 max-x 0 :ink clim:+gray+ :line-dashes t)
              (clim:draw-line* stream 0 min-y 0 max-y :ink clim:+gray+ :line-dashes t)
              (let ((inks (coerce (clim:make-contrasting-inks (length series)) 'list)))
                (do-series ((x y) elt serie series)
                  :serie (clim:with-output-as-presentation (stream serie (clim:presentation-type-of serie))
                           (clim:with-drawing-options (stream :ink (pop inks))
                             (clim:draw-polygon* stream (vals serie) :filled nil :closed nil)
                             (call-point-method)))
                  :point (clim:with-output-as-presentation (stream elt (clim:presentation-type-of elt))
                           (clim:draw-point* stream x y :line-thickness 5)))))))))))

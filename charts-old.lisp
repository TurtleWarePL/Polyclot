
(defgeneric draw-chart (frame pane)
  (:documentation "Display function which draws a chart. Function
should specialize on a pane (i.e xy-chart-pane).")
  (:method :around (frame pane)
           (clim:with-room-for-graphics (pane :height 500)
             (clim:with-translation (pane 25 25)
               (clim:draw-rectangle* pane 0 0 450 450 :filled nil :ink clim:+gray+)
               (clim:with-translation (pane 25 25)
                 (call-next-method))))))


(defclass table-chart ()
  ((data :initarg :data :reader data)))

(defclass legend-mixin () ())

(defclass line-chart (table-chart legend-mixin clim:application-pane)
  ()
  (:default-initargs :display-function #'draw-chart :min-width 500 :min-height 500))

(defmethod draw-chart (frame (pane line-chart))
  (loop
     with n = (length (data pane))
     for serie in (data pane)
     for i from 0
     for ink = (clim:make-contrasting-inks n i)
     do
       (loop
          for (x y) on serie by #'cddr
          for min-x = x then (min min-x x)
          for max-x = x then (max max-x x)
          for min-y = y then (min min-y y)
          for max-y = y then (max max-y y)
          finally (let ((x-range (- max-x min-x))
                        (y-range (- max-y min-y)))
                    (clim:with-scaling (pane (/ 400 x-range)
                                             (/ 400 y-range))
                      (clim:with-translation (pane (- min-x) (- min-y))
                        (clim:draw-points* pane serie :ink ink :line-thickness 5)
                        (clim:draw-polygon* pane serie :filled nil :closed nil :ink ink)))))))


(defclass xy-chart-pane (clim:application-pane)
  ((x-data :initarg :x-data :reader x-data)
   (y-data :initarg :y-data :reader y-data))
  (:default-initargs :display-function #'draw-chart :min-width 500 :min-height 500))

;;; https://stackoverflow.com/questions/14398178/plotting-graphs-using-bezier-curves
(defmethod draw-chart (frame (pane xy-chart-pane))
  (let ((min-x (first (x-data pane)))
        (min-y (first (y-data pane)))
        (max-x (first (x-data pane)))
        (max-y (first (y-data pane))))
    (climi::collect (points)
                    ;; first pass
                    (mapc (lambda (x y)
                            (maxf max-x x)
                            (maxf max-y y)
                            (minf min-x x)
                            (minf min-y y)
                            (points x y))
                          (x-data pane)
                          (y-data pane))
                    ;; drawing
                    (clim:with-scaling (pane (/ 400 (- max-x min-x)) (/ 400 (- max-y min-y)))
                      (clim:with-translation (pane (- min-x) (- min-y))
                        (clim:draw-points* pane (points) :ink clim:+dark-red+ :line-thickness 5)
                                        ;#+ (or) ;; interpolate or not to interpolate?
                        (clim:draw-polygon* pane (points) :filled nil :closed nil))))))


(defclass bar-chart-pane (clim:application-pane)
  ((category-values-alist :initarg :data :reader data))
  (:default-initargs :display-function #'draw-chart :width 500 :height 500))

(defmethod draw-chart (frame (pane bar-chart-pane))
  (let ((max-y (first (cdar (data pane))))
        (max-b 1)
        (all-cat-len (length (data pane))))
    ;; first pass
    (mapc (lambda (category)
            (maxf max-b (1+ (length (rest category))))
            (maxf max-y (reduce #'max (rest category))))
          (data pane))
    ;; drawing
    (clim:with-scaling (pane (/ 400 (* max-b all-cat-len)) (/ 400 max-y))
      (let ((start 0))
        (dolist (category (data pane))
          (clim:draw-text* pane (car category) (+ start (/ max-b 2)) 0
                           :align-x :center
                           :align-y :top
                           :text-size :small)
          (let ((x (+ start 0.5))
                (i 0))
            (dolist (y (rest category))
              (clim:draw-rectangle* pane x 0 (1+ x) y :ink (clim:make-contrasting-inks max-b i))
              (clim:draw-text* pane (format nil "~a" y) (+ x 0.5) y
                               :align-x :center
                               :align-y :bottom
                               :text-family :fix
                               :text-size :very-small)
              (incf x 1.0)
              (incf i)))
          (incf start max-b))))))


(defclass horizontal-bar-chart-pane (bar-chart-pane)
  ((category-values-alist :initarg :data :reader data))
  (:default-initargs :display-function #'draw-chart :width 500 :height 500))

(defmethod draw-chart (frame (pane horizontal-bar-chart-pane))
  (let ((max-y (first (cdar (data pane))))
        (max-b 1)
        (all-cat-len (length (data pane))))
    ;; first pass
    (mapc (lambda (category)
            (maxf max-b (1+ (length (rest category))))
            (maxf max-y (reduce #'max (rest category))))
          (data pane))
    ;; drawing
    (clim:with-scaling (pane (/ 300 (* max-b all-cat-len)) (/ 300 max-y))
      (let ((start 0))
        (dolist (category (data pane))
          (clim:draw-text* pane (car category) 0 (+ start (/ max-b 2))
                           :align-x :right
                           :align-y :center
                           :text-size :small)
          (let ((y (+ start 0.5))
                (i 0))
            (dolist (x (rest category))
              (clim:draw-rectangle* pane 0 y x (1+ y) :ink (clim:make-contrasting-inks max-b i))
              ;; (clim:draw-text* pane (format nil "~a" y) (+ x 0.5) y
              ;;                  :align-x :center
              ;;                  :align-y :bottom
              ;;                  :text-family :fix
              ;;                  :text-size :very-small)
              (incf y 1.0)
              (incf i)))
          (incf start max-b))))))


(defclass pie-chart-pane (clim:application-pane)
  ((category-value-alist :initarg :data :reader data))
  (:default-initargs :display-function #'draw-chart :width 500 :height 500))

(defmethod draw-chart (frame (pane pie-chart-pane))
  (when (null (data pane))
    (clim:draw-circle* pane 200 200 200 :ink clim:+grey+)
    (return-from draw-chart))
  (let ((sum 0.0)
        (max-pie 0)
        (i 0)
        (rad 0))
    ;; first pass
    (mapc (lambda (category)
            (incf max-pie)
            (incf sum (cdr category)))
          (data pane))
    ;; second pass
    (dolist (category (data pane))
      (clim:draw-ellipse* pane 200 200 200 0 0 200
                          :ink (clim:make-contrasting-inks max-pie i)
                          :start-angle rad
                          :end-angle (+ rad (* 2 pi (/ (cdr category) sum))))
      (incf i)
      (incf rad (* 2 pi (/ (cdr category) sum))))))


(defclass histogram-chart-pane (clim:application-pane)
  ((number-of-bins :initarg :bins :reader number-of-bins)
   (data :initarg :data :reader data))
  (:default-initargs :bins 7 :display-function #'draw-chart))

(defmethod draw-chart (frame (pane histogram-chart-pane))
  (let ((min-x (first (data pane)))
        (max-x (first (data pane))))
    ;; first pass
    (mapc (lambda (x)
            (minf min-x x)
            (maxf max-x x))
          (data pane))
    (let* ((bins (make-list (number-of-bins pane) :initial-element 0))
           (slice-size (/ (- max-x min-x) (1- (number-of-bins pane)))))
      ;; second pass
      (mapc (lambda (x)
              (let ((slice-no (truncate (- x min-x) slice-size)))
                (incf (elt bins slice-no))))
            (data pane))
      (clim:with-scaling (pane (/ 400 (number-of-bins pane) )
                               (/ 400 (reduce #'max bins)))
        (let ((slice-no 0))
          (dolist (slice bins)
            (clim:draw-rectangle* pane slice-no 0 (1+ slice-no) slice
                                  :ink (clim:make-contrasting-inks (number-of-bins pane)
                                                                   slice-no))
            (incf slice-no)))))))

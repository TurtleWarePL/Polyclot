(in-package #:eu.turtleware.polyclot.impl)

(define-class <count> (<stat>) ())

(define-class <bin> (<stat>)
  ;; :width and :count are mutually exclusive.
  ((width :initarg :width :initform nil)
   (count :initarg :count :initform 29)
   (left-edge :initarg :left-edge :initform t)
   (bin-edges :initarg :bin-edges :initform nil)))

(define-condition <stat-error> (error) ())
(define-condition <bins-not-monotonic> (<stat-error>) ())

(defmethod statistical-transformation ((stat <count>) (frame <data-frame>))
  (let ((df (copy-data-frame frame))
        (aest (aest stat))
        (fr (make-hash-table :test #'equal)))
    (prog1 df
      (flet ((add-it (index row)
               (declare (ignore index))
               (let* ((vals (map-aesthetics aest df row))
                      (xval (getf vals :x)))
                 (incf (gethash xval fr 0))))
             (put-it (index row)
               (declare (ignore index))
               (let* ((vals (map-aesthetics aest df row))
                      (xval (getf vals :x)))
                 (gethash xval fr 0))))
        (map-data-frame-rows df t #'add-it)
        (add-cols! df "..count.." #'put-it)))))

(defun uniform-bins (count min max)
  "Return uniform bin centers given `count` and range `min` and `max`."
  (let* ((bin-centers (make-array count))
         ;; (bin-edges (make-array (1+ count)))
         (width (- max min))
         (dw (/ width count))
         (dw/2 (/ dw 2)))
    ;; (setf (aref bin-edges 0) min)
    (loop for index from 1 to count and i from 0
          for factor = (/ index count)
          for edge = (+ (* (- 1 factor) min) (* factor max))
          for center = (- edge dw/2)
          ;; do (setf (aref bin-edges index) edge
          ;;          (aref bin-centers i) center))
          do (setf (aref bin-centers i) center))
    ;; (values bin-edges bin-centers)))
    bin-centers))

(defun nonuniform-bins (bin-edges)
  "Return non-uniform bin centers given the `bin-edges`."
  (let* ((count (1- (length bin-edges)))
         (bin-centers (make-array count)))
    (loop for index from 0 below count and i from 1
          for xf0 = (elt bin-edges 0) then xf1
          for xf1 = (elt bin-edges i)
          for xc = (/ (+ xf0 xf1) 2)
          do (assert (> xf1 xf0) nil '<bins-not-monotonic>)
             (setf (aref bin-centers index) xc))
    bin-centers))

(defmethod statistical-transformation ((stat <bin>) (frame <data-frame>))
  (let* ((col-name "..count..")
         (aest (aest stat))
         (df (make-empty-data-frame aest frame :new-cols col-name))
         (new-aest (copy-aest aest :y col-name))
         (ranges (get-data-range aest frame))
         (xmin (getf (first ranges) :x))
         (xmax (getf (second ranges) :x))
         (xrange (- xmax xmin))
         (jit-factor 1.0d-8)
         (n-bins) (w-bins))
    (with-slots (aest count width left-edge bin-edges) stat
      (if width
          (setf w-bins width
                n-bins (max 1 (round (/ xrange width))))
          (setf n-bins count
                w-bins (/ xrange count)))
      ;; bin it
      (let ((bin-centers (if bin-edges 
                             (nonuniform-bins bin-edges)
                             (uniform-bins n-bins xmin xmax)))
            (bin-counts (make-array n-bins :initial-element 0))
            (jit (if left-edge jit-factor (- jit-factor))))
        (if bin-edges
            ;; non-uniform-bins requires search
            (flet ((count-into-bins (row-index row)
                     (declare (ignore row-index))
                     (let* ((vals (map-aesthetics aest frame row)) ; old aest
                            (xval (+ (getf vals :x) jit)))
                       (cond 
                         ((< xval xmin) (incf (aref bin-counts 0)))
                         ((>= xval xmax) (incf (aref bin-counts (1- n-bins))))
                         (t (loop with index = 0 ;; avoid (non)conforming issue with i in finally clause
                                  for i from 1 to count
                                  until (< xval (elt bin-edges i))
                                  do (incf index)
                                  finally (incf (aref bin-counts index))))))))
              (map-data-frame-rows frame t #'count-into-bins))
            ;; uniform-bins can take a shortcut calculating the correct bin index
            (flet ((count-into-bins (row-index row)
                     (declare (ignore row-index))
                     (let* ((vals (map-aesthetics aest frame row)) ; old aest
                            (xval (+ (getf vals :x) jit)))
                       (cond 
                         ((< xval xmin) (incf (aref bin-counts 0)))
                         ((>= xval xmax) (incf (aref bin-counts (1- n-bins))))
                         (t (let ((index (floor (* n-bins (/ (- xval xmin) xrange)))))
                              (incf (aref bin-counts index))))))))
              (map-data-frame-rows frame t #'count-into-bins)))
        (loop for center across bin-centers ; lots of consing to make df
              for freq across bin-counts
              collect (list center freq) into rows
              finally (apply #'add-rows! df rows))
      (setf aest new-aest))
      df)))

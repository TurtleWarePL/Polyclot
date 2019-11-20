(in-package #:eu.turtleware.polyclot.impl)

(define-class <count> (<stat>) ())

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

(define-class <bin> (<stat>)
  ;; :width and :count are mutually exclusive.
  ((width :initarg :width)
   (count :initarg :count)))

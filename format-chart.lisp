(in-package #:eu.turtleware.polyclot)

(defun format-chart (series &rest args &key (output nil) chart-type &allow-other-keys)
  (remf args :output)
  (remf args :chart-type)
  ;; ensure the most reasonable interpretation in the current context
  (when (null output)
    (if (null clim:*application-frame*)
        (setf output :window)
        (setf output *standard-output*)))
  (etypecase output
    ((or stream (eql t))  #| simply render it on the stream starting at the cursor position |#
     (when (eq t output) (setq output *standard-output*))
     (let ((record (<chart-record> :chart (apply #'make-instance chart-type :series series args))))
       (multiple-value-bind (cx cy) (clim:stream-cursor-position output)
         (clim:add-output-record record (clim:stream-output-history output))
         (setf (clim:output-record-position record) (values cx cy))
         (clim:with-output-recording-options (output :draw t :record nil)
           (clim:replay record output))
         (setf (clim:stream-cursor-position output)
               (values (clim:bounding-rectangle-max-x record)
                       (clim:bounding-rectangle-max-y record)))
         record)))
    ((eql :window) #| open a separate window and display the chart in it |#
     (let* ((window (clim:open-window-stream))
            (chart (apply #'make-instance chart-type :series series args))
            (record (<chart-record> :chart chart)))
       (clim:add-output-record record (clim:stream-output-history window))
       (clim:replay-output-record record window)
       (finish-output window)))
    ((eql :application) #| create a frame with a display function drawing the chart |#
     (let* ((chart (apply #'make-instance chart-type :series series args))
            (frame (clim:make-application-frame 'chart-frame :chart chart)))
       (clim:run-frame-top-level frame)))
    ((or string pathname) #| recognize pathname-type and try to render there (png, ps, pdf) |#)
    ((eql :stream) #| creates a clim-stream which may be used as a pane in the frame |#)
    ((eql :record) #| create an output-record with the chart displayed on it |#
     (<chart-record> :chart (apply #'make-instance chart-type :series series args)))))

(clim:define-application-frame chart-frame ()
  ((chart :reader chart :initarg :chart))
  (:pane :application-pane :display-function (lambda (frame pane)
                                               (draw-chart pane (chart frame)))
         :width 480 :height 320))

(define-chart-frame-command (com-refresh :keystroke #\space) ())

(in-package #:eu.turtleware.polyclot)

;;; We can't simply use a standard-output-recording-stream. We may
;;; create it, but no primary methods are defined for the appropriate
;;; methods. Report the issue and fix it (easy enough, replace :around
;;; methods with primary ones which eventually call the next method).
#+ (or)
(defparameter +null-stream+
  (make-instance 'clim:standard-output-recording-stream))

(defparameter +null-stream+
  (let* ((port (clim:find-port :server-path :null))
         (fm (clim:find-frame-manager :port port))
         (frame (make-instance 'clim:standard-application-frame :port port)))
    (clim:with-look-and-feel-realization (fm frame)
      (clim:make-pane 'clim:clim-stream-pane))))

(defmethod clim:invoke-with-output-to-output-record
    ((stream null) continuation record-type &rest initargs)
  (let ((stream +null-stream+)
        (new-record (apply #'make-instance record-type initargs)))
    (clim:with-output-recording-options (stream :record t :draw nil)
      (climi::letf (((clim:stream-current-output-record stream) new-record)
                    ((clim:stream-cursor-position stream) (values 0 0)))
        (funcall continuation stream new-record)
        (finish-output stream)))
    new-record))

(defmethod clim:invoke-with-new-output-record
    ((stream null) continuation record-type &rest initargs &key parent)
  (check-type parent clim:output-record)
  (climi::with-keywords-removed (initargs (:parent))
    (let ((stream +null-stream+)
          (new-record (apply #'make-instance record-type initargs)))
      (clim:with-output-recording-options (stream :record t :draw nil)
        (climi::letf (((clim:stream-current-output-record stream) new-record)
                      ((clim:stream-cursor-position stream) (values 0 0)))
          (funcall continuation stream new-record)
          (finish-output stream)))
      (clim:add-output-record new-record parent)
      new-record)))

(defmacro define-class (name superclass slots &rest options)
  `(progn
     ,(if-let ((victim (second (find :stealth-mixin options :key #'car))))
        `(stealth-mixin:define-stealth-mixin
          ,name ,superclass ,victim ,slots
          ,@(remove :stealth-mixin options :key #'car))
        `(defclass ,name ,superclass ,slots ,@options))
     (defun ,name (&rest args) (apply #'make-instance ',name args))
     (defvar ,name (find-class ',name))))

(defmacro define-function (name lambda-list &body body)
  `(defun ,name ,lambda-list ,@body))

(defmacro nest (&rest things)           ; curbed from uiop
  (reduce #'(lambda (outer inner)
              `(,@outer ,inner))
          things :from-end t))

(defun scaling-ratio (pane)
  (multiple-value-bind (dx dy)
      (clim:transform-distance (clim:medium-transformation pane) 1 1)
    (format *debug-io* "ratio is ~s~%" (abs (/ dx dy)))))

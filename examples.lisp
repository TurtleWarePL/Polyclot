
(in-package #:eu.turtleware.polyclot)

;; (defmacro formatting-chart ((&optional stream &rest args) &body body)
;;   `(progn ,@body))

;; (defmacro formatting-serie ((&optional stream &rest args) &body data)
;;   `(progn ,@data))

;; (out stream (:chart (:serie (:label "fixnum" :color +dark-red+) 1 2 3 4)
;;                     (:serie (:label "bignum" :color +dark-red+) 1 2 3 4)))

;; (formatting-chart (stream :type :bar :x-spacing 50 :y-spacing 50)
;;   (formatting-serie (stream :label "Fixnum" :color +dark-red+)
;;     (out 3 11 5 2 4 8))
;;   (formatting-serie (stream :label "Bignum" :color +dark-red+)
;;     (out 14 12 8 18 8 19)))

;; (defclass bar-chart-output-record (clim:output-record)
;;   ((bars :initarg :bars :reader bars)
;;    (data :initarg :data :reader data)))

;; (make-instance 'bar-chart-output-record
;;                :bars '("ECL (new)" "ECL" "Clozure CL" "SBCL" "Clisp")
;;                :data '(("Fixnum" 3 11 5 2 4 8)
;;                        ("Bignum" 14 12 8 18 8 19)
;;                        ("Ratio"  8 12 10 10 9 9)
;;                        ("Float"  20 20 20 3 14 3)
;;                        ("Complex" 4 3 30 12 22 18)
;;                        ("CFloat" 12 32 19 10 30 15)))

;; (defmethod initialize-instance :after ((instance bar-chart-output-record))
;;   (clim:with-output-to-output-record (nil)
;;     ))

(clim:define-application-frame foo ()
  ()
  (:pane (clim:make-pane 'line-chart :data '((1 1 2 2 3 3 4 4 5 5)
                                             (1 8 2 16 3 32 4 64))
                          :width 500 :height 500)))

(clim:run-frame-top-level
 (clim:make-application-frame 'foo))

(defun format-chart (series &rest args &key stream chart-type &allow-other-keys)
  (etypecase stream
    ((or stream (eql t))  #| simply replay it on the stream starting at the cursor position |#)
    ((or string pathname) #| recognize pathname-type and try to render there (png, ps, pdf) |#)
    (null                 #| open a window stream displaying the chart |#)))

(format-chart '(("xyz" (1 3) (2 4) (3 5))
                ("abc" (1 8) (2 16) (3 32) (4 64)))
              :chart-type :line
              :stream *standard-output*)

(format-chart '(("xyz" 3 3 4 2)
                ("abc" 1 2 2 1))
              :stream *standard-output*
              :chart-type :bar
              :orientation :horizontal)

(format-chart :bar '(("serie-1" 1 2 3 4)
                     ("serie-2" 4 5 6 7))
              '("cat-1" "cat-2" "cat-3"))

(format-chart :line '(("serie-1" (1 2) (3 4) (4 4))
                      ("serie-2" (2 2) (3 4) (5 8) (9 10))))

(clim:formatting-table (stream :record-type 'bar-chart)
  (clim:formatting-row (stream :record-type 'labels)
    (clim:format-items '("Serie" "Fixnum" "Bignum" "Ratio" "Complex" "CFloat")
                       :stream stream
                       :presentation-type 'label))
  (clim:formatting-row (stream :record-type 'bar-chart-serie)
    (clim:present "serie-1" 'serie-label)
    (clim:format-items '(1 2 3 4) :stream stream :presentation-type 'value))
  (clim:formatting-row (stream :record-type 'bar-chart-serie)
    (clim:present "serie-1" 'serie-label)
    (clim:format-items '(1 2 3 4) :stream stream :presentation-type 'value)))

(show-bar-chart
 '(("Serie" "Fixnum" "Bignum" "Ratio" "Complex" "CFloat")
   ("serie-1" 1 2 3 4 5)
   ("serie-2" 1 2 3 4 5)
   ("serie-3" 1 2 3 4 5)))

(show-bar-chart '("Impl" "Fixnum" "Bignum" "Ratio" "Complex" "CFloat")
                '(("ECL" 1 2 3 4)
                  ("ECL (new)" 1 2 3 4)
                  ("SBCL" 1 2 3 4)))

#-(or) ;; If you like what you don't have you don't have what you like.
(progn
  (defparameter *x-data* (iota 100 :start -300 :step 0.2))
  (defparameter *y-data* (mapcar (lambda (x) (+ (cos x) (sin (/ x 3)) (sin (/ x 2.5)))) *x-data*))
  (defparameter *bar-data* '(("Fixnum" 3 11 5 2 4 8)
                             ("Bignum" 14 12 8 18 8 19)
                             ("Ratio"  8 12 10 10 9 9)
                             ("Float"  20 20 20 3 14 3)
                             ("Complex" 4 3 30 12 22 18)
                             ("CFloat" 12 32 19 10 30 15)))
  (defparameter *pie-data* '(("Fixnum" . 30)
                             ("Bignum" . 60)
                             ("Ratio"  . 40)
                             ("Float"  . 20)
                             ("Complex" . 15)
                             ("CFloat" . 33)))
  (clim:define-application-frame polyclot ()
    ()
    (:pointer-documentation t)
    (:panes (xy  (clim:make-pane 'xy-chart-pane :x-data *x-data* :y-data *y-data*))
            (bar (clim:make-pane 'bar-chart-pane :data *bar-data*))
            (pie (clim:make-pane 'pie-chart-pane :data *pie-data*))
            (his (clim:make-pane 'histogram-chart-pane :data *y-data*))
            (int :interactor :scroll-bars nil))
    (:layouts (default
                  (clim:horizontally ()
                    (500 int)
                    (clim:tabling (:grid t :width 500 :height 500)
                      (list xy bar)
                      (list his pie)
                      #+ (or) ;; grid should be capable of having empty entries
                      (list nil pie)
                      #+ (or) ;; I want to be able to display same sheet in two places
                      (list pie pie)))))))

(defun plot (&optional (x *x-data*) (y *y-data*))
  #+(or) ;; That's how I'd like to open it.
  (clim:open-window-stream :stream-class 'xy-chart-pane :x-data x :y-data y)
  #-(or)
  (let ((*x-data* x)
        (*y-data* y))
    (clim:run-frame-top-level (clim:make-application-frame 'polyclot))))

(plot)

#+(or) ;; Also I'd like to make this possible.
(clim:with-look-and-feel-realization ((clim:find-frame-manager) nil)
  (clim:make-pane 'xy-chart-pane :x-data '(1 2 3) :y-data '(3 4 5)))

#+ (or)
(progn (defvar *bar-data-1*)
       (defvar *bar-data-2*)
       (defvar *columns*)
       (defvar *bar-data-4*)
       (clim:define-application-frame xxx ()
         ()
         (:pointer-documentation t)
         (:name "FOO")
         (:panes (real (clim:make-pane 'bar-chart-pane :data *bar-data-1*))
                 (user (clim:make-pane 'bar-chart-pane :data *bar-data-2*))
                 (appl :application :display-function
                       (lambda (frame pane)
                         (let ((len (length *columns*)))
                           (loop
                              for i from 0
                              for c in *columns*
                              do (clim:draw-rectangle* pane
                                                       50 (+ (* i 80) 50)
                                                       100 (+ (* i 80) 100)
                                                       :ink (clim:make-contrasting-inks len i))
                                (clim:draw-text* pane c 120 (+ (* i 80) 75))))))
                 (cons (clim:make-pane 'bar-chart-pane :data *bar-data-4*))
                 (int :interactor))
         (:layouts (default
                       (clim:vertically ()
                         (clim:tabling (:grid t :width 500 :height 500)
                           (list real user)
                           (list cons appl))
                         int))))

       (let* ((tries '("real" "user" "sys" "cons"))
              (results (make-hash-table :test #'equalp))
              (all-results (cl-bench::read-benchmarks))
              (columns (mapcar #'car all-results)))
         (dolist (v all-results)
           (mapcar (lambda (probe)
                     (let ((boxes (ensure-gethash (car probe)
                                                  results
                                                  (list nil nil nil nil))))
                       (dotimes (v 4)
                         (push (or (elt (cdr probe) v) 1) (elt boxes v)))))
                   (cdr v)))
         (maphash (lambda (key val)
                    (let ((*bar-data-1* (list (list* (elt tries 0) (nreverse (elt val 0)))))
                          (*bar-data-2* (list (list* (elt tries 1) (nreverse (elt val 1)))))
                                        ;(*bar-data-3* (list (list* (elt tries 2) (elt val 2))))
                          (*bar-data-4* (list (list* (elt tries 3) (nreverse (elt val 3)))))
                          (*columns* columns))
                      (clim:run-frame-top-level (clim:make-application-frame 'xxx :pretty-name key))))
                  results)))


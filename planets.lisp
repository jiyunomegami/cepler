;; Tanstaafl - A free space flight simulator
;; Copyright (C) 2009  Neil Forrester
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY;; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cepler)

(defvar *all-objs*)
(defvar *G* 6.673d-11)
(defvar *m-per-au* 149598000000d0)

(defun magnitude (vec)
  (v3:length vec))

(defun mult (s vec)
  (v3:*s vec (coerce s 'single-float)))

(defun qmult (q1 q2)
  (q:* q1 q2))

(defun add (v1 v2)
  (v3:+ v1 v2))

(defun sub (v1 v2)
  (v3:- v1 v2))

(defun make-vector-3 (&optional (x 0) (y 0) (z 0))
  (v! (coerce x 'single-float)
      (coerce y 'single-float)
      (coerce z 'single-float)))

(defun make-quaternion (&optional (w 0) (x 0) (y 0) (z 0))
  (q! (coerce w 'single-float)
      (coerce x 'single-float)
      (coerce y 'single-float)
      (coerce z 'single-float)))

(defun make-matrix-3-3 (&rest rest)
  (apply #'m3:make (loop for x in rest collect (coerce x 'single-float))))

(defun rotate (vec quat)
  (let ((x (v:x vec))
        (y (v:y vec))
        (z (v:z vec)))
    (let ((res (qmult quat (qmult (make-quaternion 0 x y z) (q:inverse quat)))))
      (let ((x (v:x res))
            (y (v:y res))
            (z (v:z res)))
        (make-vector-3 x y z)))))

(defun compute-inertia-tensor (x y z)
  (make-matrix-3-3
   x 0 0
   0 y 0
   0 0 z))

(defclass space-object ()
  ((name
    :initarg :name
    :initform ""
    :documentation "The name of the object.")
   (mass
    :initarg :mass
    :initform 1.0
    :documentation "Mass measured in kg.")

   (pos
    :initarg :pos
    :initform (make-vector-3)
    :documentation "position measured in m, cartesian, as a vector-3")
   (vel
    :initarg :vel
    :initform (make-vector-3)
    :documentation "velocity measured in m/s, cartesian, as a vector-3")
   (acc
    :initarg :acc
    :initform (make-vector-3)
    :documentation "acceleration measured in m/s/s, cartesian, as a vector-3")

   (inertia-tensor
    :initarg :inertia-tensor
    :initform (compute-inertia-tensor 1 1 1)
    :documentation "inertia tensor, as a matrix-3-3")
   (ang-pos
    :initarg :ang-pos
    :initform (make-quaternion)
    :documentation "angular position, as a quaternion")
   (ang-vel
    :initarg :ang-vel
    :initform (make-vector-3)
    :documentation "angular velocity in radians/s, as a vector-3")
   (ang-acc
    :initarg :ang-acc
    :initform (make-vector-3)
    :documentation "angular acceleration in radians/s/s, as a vector-3")
   (radius
    :initarg :radius
    :initform 1
    :documentation "The radius of the space object, used for determining whether objects overlap in depth, and thus need to be in the same depth layer for drawing. spherical-bodies also use this as their literal radius.")))

(defmethod distance ((o1 space-object) (o2 space-object))
  (v3:length
   (v3:- (slot-value o1 'pos)
         (slot-value o2 'pos))))

(defmethod relative-position ((o1 space-object) (o2 space-object))
  (v3:- (slot-value o1 'pos)
        (slot-value o2 'pos)))

(defgeneric add-force (obj force &key)
  (:documentation "apply a force to obj, and update acc"))

(defmethod add-force ((obj space-object) (force t) &key (frame :local))
  (with-slots (mass acc) obj
    (setf acc (add acc (mult (/ 1 mass)
			     (if (eq frame :global)
                                 force
                                 (rotate force (slot-value obj 'ang-pos))))))))

(defgeneric compute-gravity (obj)
  (:documentation "compute gravity on obj due to *all-objs*."))

(defmethod compute-gravity ((obj space-object))
  (loop for other-obj in *all-objs* do
       (unless (eq obj other-obj)
         (let*
             ((rel-pos (sub
			(slot-value other-obj 'pos)
			(slot-value obj 'pos)))
              (distance (magnitude rel-pos)))
           (unless (= 0.0 distance)
             (add-force obj
                        (mult ; gravitational force vector
                         (/ ; G * m / r^2
                          (* *G* (slot-value obj 'mass) (slot-value other-obj 'mass))
                          (expt distance 2))
                         (mult ; r hat
                          (/ 1 distance)
                          rel-pos)) :frame :global))))))

(defgeneric add-torque (obj torque &key)
  (:documentation "apply a torque to obj, and update acc"))

(defmethod add-torque ((obj space-object) (torque t) &key (frame :local))
  (with-slots (inertia-tensor ang-acc) obj
    (setf ang-acc (add ang-acc (mult (m3:affine-inverse inertia-tensor) ;; XXX not true inverse
				     (if (eq frame :global)
                                         torque
                                         (rotate torque (slot-value obj 'ang-pos))))))))

(defgeneric add-force-off-center (obj force pos &key)
  (:documentation "apply a force to obj, compute the associated torque, and update acc and ang-acc accordingly"))

(defmethod add-force-off-center ((obj space-object) (force t) (pos t) &key (frame :local))
  (add-force obj force :frame frame)
  (add-torque obj (v3:cross pos force) :frame frame))

(defgeneric compute-forces (obj dt)
  (:documentation "compute the forces on obj, and update acc and ang-acc"))

(defmethod compute-forces :before ((obj space-object) dt)
  (setf (slot-value obj     'acc) (make-vector-3 0 0 0))
  (setf (slot-value obj 'ang-acc) (make-vector-3 0 0 0)))

(defmethod compute-forces ((obj space-object) dt)
  (compute-gravity obj))

(defgeneric integrate-acc-to-vel (obj dt)
  (:documentation "integrate acceleration to get velocity."))

(defmethod integrate-acc-to-vel ((obj space-object) dt)
  (with-slots (vel acc) obj
    (setf vel (add vel (mult dt acc)))))

(defgeneric integrate-ang-acc-to-ang-vel (obj dt)
  (:documentation "integrate angular acceleration to get angular velocity."))

(defmethod integrate-ang-acc-to-ang-vel ((obj space-object) dt)
  (with-slots (ang-vel ang-acc) obj
    (setf ang-vel (add ang-vel (mult dt ang-acc)))))

(defgeneric integrate-vel-to-pos (obj dt)
  (:documentation "integrate velocity to get position."))

(defmethod integrate-vel-to-pos ((obj space-object) dt)
  (with-slots (pos vel) obj
    (setf pos (add pos (mult dt vel)))))

(defgeneric integrate-ang-vel-to-ang-pos (obj dt)
  (:documentation "integrate angular velocity to get angular position."))

(defmethod integrate-ang-vel-to-ang-pos ((obj space-object) dt)
  (with-slots (ang-pos ang-vel) obj
    (let ((x (v:x ang-vel))
          (y (v:y ang-vel))
          (z (v:z ang-vel)))
      (setf ang-pos (q:+ ang-pos (q:*s (q:*s (q:* (make-quaternion 0 x y z) ang-pos) 0.5) dt)))
      (setf ang-pos (q:normalize ang-pos)))))

(defvar *epoch-time* 0d0)

(defun timestep (dt)
  (setf *epoch-time* (+ dt *epoch-time*))
  (loop for obj in *all-objs* do
       (compute-forces obj dt))
  (loop for obj in *all-objs* do
       (integrate-acc-to-vel obj dt)
       (integrate-ang-acc-to-ang-vel obj dt)
       (integrate-vel-to-pos obj dt)
       (integrate-ang-vel-to-ang-pos obj dt)))

(defvar *state-output-stream* t)

(defparameter *gl-scale* 0.0000000001)
(defvar *gl-planets*)

(defun find-gl-planet (name)
  (find-if #'(lambda (glp) (equalp name (gl-planet-name glp))) *gl-planets*))

(defvar *sun*)

(defun update-positions ()
  (dolist (obj (cdr *all-objs*))
    (let ((pos (relative-position obj *sun*)))
      (let ((x (* (v:x pos) *gl-scale*))
            (y (* (v:y pos) *gl-scale*))
            (z (* (v:z pos) *gl-scale*)))
        (let ((glp (find-gl-planet (slot-value obj 'name))))
          (when glp
            (setf (v:x (pos glp)) x
                  (v:y (pos glp)) y
                  (v:z (pos glp)) z)))))))

(defun print-positions ()
  (dolist (obj (cdr *all-objs*))
    (let ((pos (relative-position obj *sun*)))
      (let ((x (* (v:x pos) *gl-scale*))
            (y (* (v:y pos) *gl-scale*))
            (z (* (v:z pos) *gl-scale*)))
        (let ((glp (find-gl-planet (slot-value obj 'name))))
          (setf (v:x (pos glp)) x
                (v:y (pos glp)) y
                (v:z (pos glp)) z))
        (format t "~A: ~f  ~f  ~f~%"
                (slot-value obj 'name)
                x y z)))))

(defun print-timestep (&optional (objs *all-objs*))
  (format *state-output-stream* "begin-timestep ~a~%" *epoch-time*)
  (dolist (obj objs)
    (format *state-output-stream* "begin-object ~A~%" (slot-value obj 'name))
    (let* ((pos (slot-value obj 'pos))
           (x (v:x pos))
           (y (v:y pos))
           (z (v:z pos)))
      (format *state-output-stream* "  pos ~a ~a ~a~%" x y z))
    (let* ((ang-pos (slot-value obj 'ang-pos))
           (w (q:w ang-pos))
           (x (q:x ang-pos))
           (y (q:y ang-pos))
           (z (q:z ang-pos)))
      (let ((len (magnitude (make-vector-3 x y z))))
	;;(print (list w x y z len))
	(if (/= 0 len) ; print in angle-axis form (in degrees, because that's what OpenGL uses *shudder*)
            (format *state-output-stream* "  ang-pos ~a ~a ~a ~a~%"
                    (/ (* (* 2 (acos w)) 180) pi)
                    (/ x len)
                    (/ y len)
                    (/ z len))
            (format *state-output-stream* "  ang-pos ~a ~a ~a ~a~%"
                    0
                    1
                    0
                    0))))
    (format *state-output-stream* "  radius 1~%")
    (format *state-output-stream* "end-object~%"))
  (format *state-output-stream* "end-timestep~%"))

(let ((current-time (get-internal-real-time)) prev-time)
  (defun tanstaafl-main-loop-step (&optional dt)
    (setf prev-time current-time)
    (setf current-time (get-internal-real-time))
    (if dt
        (when (not (= dt 0))
          (timestep dt))
        (let ((dt (/ (- current-time prev-time) (/ internal-time-units-per-second *time-acceleration*))))
          (when (or nil (> dt 0) (< dt 0))
            ;;(format t "dt: ~S~%" dt)
            (timestep dt))))))


(defclass spherical-body (space-object) ())

;;; VSOP does things a little backwards (compared to normal numerical methods),
;;; computing the position, and letting you derive everything else from that by
;;; taking derivitives. In order to make things work out right with the rest of
;;; the physics engine, here's a breif explanation of how the normal functions
;;; map to what needs to happen with VSOP (all are hijacked by :around methods):
;;;
;;; compute-forces - evaluates the VSOP series sets, computes the position of the
;;;                  object, and stores it in vsop-pos. Then compares with the
;;;                  previous position (conveniently still found in pos), to find
;;;                  the velocity, and updates vsop87-vel appropriately. Then
;;;                  compares this result with the previous velocity (vel), and
;;;                  updates acc.
;;;
;;; integrate-acc-to-vel - Doesn't actually integrate anything, just copies
;;;                        vsop-vel to vel
;;;
;;; integrate-vel-to-pos - Doesn't actually integrate anything, just copies
;;;                        vsop-pos to pos

(defclass vsop-body (space-object)
  ((vsop-pos
    :initform (make-vector-3)
    :initarg :vsop-pos
    :documentation "see above comment")
   (vsop-vel
    :initform (make-vector-3)
    :initarg :vsop-vel
    :documentation "see above comment")
   (vsop-base-interval
    :initform 10d0
    :initarg :vsop-base-interval
    :documentation "The interval for interpolation between vsop-reference-points, scaled linearly with time acceleration")
   (vsop-ref-1
    :initform (make-instance 'vsop-reference-point :epoch 0)
    :initarg :vsop-ref-1
    :documentation "A vsop-reference-point")
   (vsop-ref-2
    :initform (make-instance 'vsop-reference-point :epoch -1)
    :initarg :vsop-ref-2
    :documentation "A vsop-reference-point")
   (x-series-set
    :initform ()
    :initarg :x-series-set)
   (y-series-set
    :initform ()
    :initarg :y-series-set)
   (z-series-set
    :initform ()
    :initarg :z-series-set)))

(defvar *vsop-available* nil)
(when *vsop-series-set-sun-x*
  (setq *vsop-available* t))
(defvar *use-vsop* *vsop-available*)

;;; This function hijacks the inherited behavior completely. See above.
(defmethod compute-forces :around ((obj vsop-body) dt)
  (if *use-vsop*
      (with-slots (vsop-pos
                   vsop-vel
                   vsop-base-interval
                   vsop-ref-1
                   vsop-ref-2
                   pos
                   vel
                   acc
                   x-series-set
                   y-series-set
                   z-series-set) obj
        (multiple-value-setq (vsop-pos vsop-ref-1 vsop-ref-2) (vsop-compute-position
                                                               *epoch-time*
                                                               x-series-set
                                                               y-series-set
                                                               z-series-set vsop-base-interval vsop-ref-1 vsop-ref-2))
        (setf vsop-vel (mult (/ 1 dt) (sub vsop-pos pos)))
        (setf acc      (mult (/ 1 dt) (sub vsop-vel vel))))
      (call-next-method)))

;;; This function hijacks the inherited behavior completely. See above.
(defmethod integrate-acc-to-vel :around ((obj vsop-body) dt)
  (if *use-vsop*
      (with-slots (vel vsop-vel) obj
        (setf vel vsop-vel))
      (call-next-method)))

;;; This function hijacks the inherited behavior completely. See above.
(defmethod integrate-vel-to-pos :around ((obj vsop-body) dt)
  (if *use-vsop*
      (with-slots (pos vsop-pos) obj
        (setf pos vsop-pos))
      (call-next-method)))

(defclass planet (spherical-body) ())
(defclass vsop-planet (planet vsop-body) ())

(defclass star (spherical-body) ())
(defclass vsop-star (star vsop-body) ())

(defvar *sun* (make-instance 'vsop-star
                             :name "Sun"
                             :mass 1.9891d30
                             :radius 695500000d0
                             :vsop-base-interval 1049
                             :x-series-set *vsop-series-set-sun-x*
                             :y-series-set *vsop-series-set-sun-y*
                             :z-series-set *vsop-series-set-sun-z*))

(defvar *mercury* (make-instance 'vsop-planet
                                 :name "Mercury"
                                 :mass 3.3022d23
                                 :radius 2439700d0
                                 :vsop-base-interval 1000
                                 :x-series-set *vsop-series-set-mercury-x*
                                 :y-series-set *vsop-series-set-mercury-y*
                                 :z-series-set *vsop-series-set-mercury-z*))

(defvar *venus* (make-instance 'vsop-planet
                               :name "Venus"
                               :mass 4.8685d24
                               :radius 6051800d0
                               :vsop-base-interval 1001
                               :x-series-set *vsop-series-set-venus-x*
                               :y-series-set *vsop-series-set-venus-y*
                               :z-series-set *vsop-series-set-venus-z*))

(defvar *earth* (make-instance 'vsop-planet
                               :name "Earth"
                               :mass 5.9742d24
                               :radius 6.3781d6
                               :vsop-base-interval 1002
                               :x-series-set *vsop-series-set-earth-x*
                               :y-series-set *vsop-series-set-earth-y*
                               :z-series-set *vsop-series-set-earth-z*))

(defvar *mars* (make-instance 'vsop-planet
                              :name "Mars"
                              :mass 6.4185d23
                              :radius 3386200d0
                              :vsop-base-interval 1003
                              :x-series-set *vsop-series-set-mars-x*
                              :y-series-set *vsop-series-set-mars-y*
                              :z-series-set *vsop-series-set-mars-z*))

(defvar *jupiter* (make-instance 'vsop-planet
                                 :name "Jupiter"
                                 :mass 1.8986d27
                                 :radius 69000000d0
                                 :vsop-base-interval 1004
                                 :x-series-set *vsop-series-set-jupiter-x*
                                 :y-series-set *vsop-series-set-jupiter-y*
                                 :z-series-set *vsop-series-set-jupiter-z*))

(defvar *saturn* (make-instance 'vsop-planet
                                :name "Saturn"
                                :mass 5.6846d26
                                :radius 58000000d0
                                :vsop-base-interval 1005
                                :x-series-set *vsop-series-set-saturn-x*
                                :y-series-set *vsop-series-set-saturn-y*
                                :z-series-set *vsop-series-set-saturn-z*))

(defvar *uranus* (make-instance 'vsop-planet
                                :name "Uranus"
                                :mass 8.6810d25
                                :radius 25200000d0
                                :vsop-base-interval 1006
                                :x-series-set *vsop-series-set-uranus-x*
                                :y-series-set *vsop-series-set-uranus-y*
                                :z-series-set *vsop-series-set-uranus-z*))

(defvar *neptune* (make-instance 'vsop-planet
                                 :name "Neptune"
                                 :mass 1.0243d26
                                 :radius 24500000d0
                                 :vsop-base-interval 1007
                                 :x-series-set *vsop-series-set-neptune-x*
                                 :y-series-set *vsop-series-set-neptune-y*
                                 :z-series-set *vsop-series-set-neptune-z*))

(defvar *pluto* (make-instance 'space-object
                                 :name "Pluto"
                                 :mass 1.46d22
                                 :radius 1185000d0))

(defun output-initial-conditions ()
  (dolist (obj *all-objs*)
    (format t "(setf (slot-value *~A* 'pos) (make-array 3 :element-type 'single-float :initial-contents '~A))~%"
            (string-downcase (slot-value obj 'name)) (subseq (princ-to-string (slot-value obj 'pos)) 1))
    (format t "(setf (slot-value *~A* 'vel) (make-array 3 :element-type 'single-float :initial-contents '~A))~%"
            (string-downcase (slot-value obj 'name)) (subseq (princ-to-string (slot-value obj 'vel)) 1))
    (format t "(setf (slot-value *~A* 'acc) (make-array 3 :element-type 'single-float :initial-contents '~A))~%"
            (string-downcase (slot-value obj 'name)) (subseq (princ-to-string (slot-value obj 'acc)) 1))))

(unless *vsop-available*
  (setf (slot-value *sun* 'pos) (make-array 3 :element-type 'single-float :initial-contents '(5.613398e8 3.441153e8 -2.4341866e7)))
  (setf (slot-value *sun* 'vel) (make-array 3 :element-type 'single-float :initial-contents '(-0.77037036 12.192593 -0.01111111)))
  (setf (slot-value *sun* 'acc) (make-array 3 :element-type 'single-float :initial-contents '(-0.0033476225 0.052139346 -6.769404e-6)))
  (setf (slot-value *mercury* 'pos) (make-array 3 :element-type 'single-float :initial-contents '(-4.6073074e10 -4.8134705e10 2.9274704e8)))
  (setf (slot-value *mercury* 'vel) (make-array 3 :element-type 'single-float :initial-contents '(25329.777 -31793.303 -4924.311)))
  (setf (slot-value *mercury* 'acc) (make-array 3 :element-type 'single-float :initial-contents '(111.86081 -132.21169 -21.070786)))
  (setf (slot-value *venus* 'pos) (make-array 3 :element-type 'single-float :initial-contents '(9.495007e10 5.336011e10 -4.7442555e9)))
  (setf (slot-value *venus* 'vel) (make-array 3 :element-type 'single-float :initial-contents '(-17360.592 30616.65 1422.2222)))
  (setf (slot-value *venus* 'acc) (make-array 3 :element-type 'single-float :initial-contents '(-75.97586 129.9587 6.1657295)))
  (setf (slot-value *earth* 'pos) (make-array 3 :element-type 'single-float :initial-contents '(-1.0015065e11 -1.12187924e11 -1.9332464e7)))
  (setf (slot-value *earth* 'vel) (make-array 3 :element-type 'single-float :initial-contents '(21864.297 -20112.117 4.0939813)))
  (setf (slot-value *earth* 'acc) (make-array 3 :element-type 'single-float :initial-contents '(94.20174 -85.26537 0.0039261295)))
  (setf (slot-value *mars* 'pos) (make-array 3 :element-type 'single-float :initial-contents '(-1.3356941e11 -1.86917e11 -6.566353e8)))
  (setf (slot-value *mars* 'vel) (make-array 3 :element-type 'single-float :initial-contents '(20753.066 -12109.748 -760.7704)))
  (setf (slot-value *mars* 'acc) (make-array 3 :element-type 'single-float :initial-contents '(88.994896 -51.426502 -3.2615151)))
  (setf (slot-value *jupiter* 'pos) (make-array 3 :element-type 'single-float :initial-contents '(-8.060767e11 1.0323881e11 1.7597757e10)))
  (setf (slot-value *jupiter* 'vel) (make-array 3 :element-type 'single-float :initial-contents '(-1835.6147 -12379.022 94.81481)))
  (setf (slot-value *jupiter* 'acc) (make-array 3 :element-type 'single-float :initial-contents '(-7.7701325 -53.143703 0.3970736)))
  (setf (slot-value *saturn* 'pos) (make-array 3 :element-type 'single-float :initial-contents '(-4.6005017e11 -1.4264121e12 4.3110883e10)))
  (setf (slot-value *saturn* 'vel) (make-array 3 :element-type 'single-float :initial-contents '(8722.963 -3034.074 -285.39258)))
  (setf (slot-value *saturn* 'acc) (make-array 3 :element-type 'single-float :initial-contents '(37.31285 -12.887792 -1.2576497)))
  (setf (slot-value *uranus* 'pos) (make-array 3 :element-type 'single-float :initial-contents '(2.796991e12 1.04821463e12 -3.2343153e10)))
  (setf (slot-value *uranus* 'vel) (make-array 3 :element-type 'single-float :initial-contents '(-2487.9407 6068.148 1.8962963)))
  (setf (slot-value *uranus* 'acc) (make-array 3 :element-type 'single-float :initial-contents '(-10.525615 26.08578 0.21406493)))
  (setf (slot-value *neptune* 'pos) (make-array 3 :element-type 'single-float :initial-contents '(4.202885e12 -1.5552007e12 -6.4832774e10)))
  (setf (slot-value *neptune* 'vel) (make-array 3 :element-type 'single-float :initial-contents '(1820.4445 5157.926 -247.46666)))
  (setf (slot-value *neptune* 'acc) (make-array 3 :element-type 'single-float :initial-contents '(7.9480567 22.100046 -0.67529947)))
  )

(defvar *vessel* nil)
(defvar *colliding* nil)

(defun init-planets ()
  (setq *vessel* nil
        *colliding* nil)
  #+nil
  (let* ((rel-pos (relative-position *earth* *sun*))
         (dir (v3:*s (v3:normalize rel-pos) -1.0))
         (off (v3:*s dir (coerce (* 3000 6.3781d6) 'single-float)))
         (pos (v3:+ (slot-value *sun* 'pos) (v3:+ rel-pos off))))
    (setq *pluto* (make-instance 'space-object
                                 :name "Pluto"
                                 :mass 1.46d22
                                 :pos pos
                                 :vel (let ((earth-vel (slot-value *earth* 'vel)))
                                        earth-vel)
                                 :radius 1185000d0)))
  (setq *all-objs* (list *sun* *mercury* *venus* *earth* *mars* *jupiter* *saturn* *uranus* *neptune*
                         #+nil
                         *pluto*)))

(init-planets)

(defclass vessel (space-object)
  ((thrust
    :initarg :thrust
    :accessor thrust
    :initform (make-vector-3))
   (reverse-thrust
    :initarg :reverse-thrust
    :accessor reverse-thrust
    :initform nil)
   (fuel
    :initarg :fuel
    :accessor fuel
    :initform 0)
   (fuel-remaining
    :initarg :fuel-remaining
    :accessor fuel-remaining
    :initform 200)))

(defmethod compute-gravity ((obj vessel))
  (loop for other-obj in *all-objs* do
       (unless (or (eq obj other-obj)
                   #+nil
                   (eq other-obj *earth*))
         (let*
             ((rel-pos (sub
			(slot-value other-obj 'pos)
			(slot-value obj 'pos)))
              (distance (magnitude rel-pos)))
           (unless (= 0.0 distance)
             (add-force obj
                        (mult ; gravitational force vector
                         (/ ; G * m / r^2
                          (* *G* (slot-value obj 'mass) (slot-value other-obj 'mass))
                          (expt distance 2))
                         (mult ; r hat
                          (/ 1 distance)
                          rel-pos)) :frame :global))))))

(defvar *prev-fuel* 0)
(defvar *hit-sun* nil)
(defvar *hit-other* nil)
(defvar *fuel-remaining* nil)
(defvar *cheated* nil)

(defun square (x)
  (* x x))

;; detect collisions using the on-screen sizes
(defvar *sun-actual-size* nil)
(defparameter *radius-factor* 1000)
(defparameter *radius-factor-sun* 39)

(defun detect-collisions (obj)
  (setq *colliding* nil)
  (let* ((radius-factor (if *sun-actual-size*
                            (* *radius-factor* #.(/ 5.0 109.0))
                            *radius-factor*))
         (obj-pos (slot-value obj 'pos))
         (obj-radius (* radius-factor (slot-value obj 'radius)))
         (obj-radius-nonsuncmp (* radius-factor (slot-value obj 'radius)))
         (obj-radius-suncmp (* *radius-factor-sun* (slot-value obj 'radius))))
    (loop for other-obj in *all-objs* do
         (unless (eq obj other-obj)
           (setq obj-radius (if (eq *sun* other-obj)
                                obj-radius-suncmp
                                obj-radius-nonsuncmp))
           (let* ((other-pos (slot-value other-obj 'pos))
                  (radius-factor (if (eq *sun* other-obj)
                                     *radius-factor-sun*
                                     radius-factor))
                  (other-radius (* radius-factor (slot-value other-obj 'radius))))
             (let ((dxsq (square (- (v:x other-pos) (v:x obj-pos))))
                   (dysq (square (- (v:y other-pos) (v:y obj-pos))))
                   (dzsq
                     #+nil 0
                     (square (- (v:z other-pos) (v:z obj-pos))))
                   (sumradsq (square (+ obj-radius other-radius))))
               #+nil
               (when (eq *earth* other-obj)
                 (format t "test earth: ~A  ~A~%" (+ dxsq dysq dzsq) sumradsq))
               (when (<= (+ dxsq dysq dzsq) sumradsq)
                 ;;(format t "collision ~A - ~A~%" (slot-value obj 'name) (slot-value other-obj 'name))
                 ;;(setf (v:z obj-pos) (v:z other-pos))
                 (setq *hit-other* other-obj)
                 (when (eq other-obj *sun*)
                   (setq *hit-sun* t))
                 (setq *fuel-remaining* (fuel-remaining *vessel*))
                 (setq *colliding* other-obj))))))))

(defmethod compute-forces ((obj vessel) dt)
  (setq *prev-fuel* (fuel obj))
  (when (> (fuel obj) 0)
    (add-force obj
               (if (reverse-thrust obj)
                   (v3:*s (thrust obj) -1.0)
                   (thrust obj))
               :frame :global)
    (decf (fuel obj)))
  (detect-collisions obj)
  (call-next-method))

(defvar *gl-pluto* nil)
(defvar *pluto-copies* 0)

(defun add-planet (&optional position relative-p)
  (let* ((sun-pos (slot-value *sun* 'pos))
         (earth-pos (slot-value *earth* 'pos))
         (earth-vel (slot-value *earth* 'vel))
         (thrust-factor 1)
         (mass 1.46d22) ;; pluto
         ;;(mass 7.30d22) ;; moon
         ;;(mass 5.9742d24) ;; earth
         (rel-pos (relative-position *earth* *sun*))
         (dir (v3:*s (v3:normalize rel-pos) -1.0))
         (off (v3:*s dir (coerce (* 1 2 1000 6.3781d6) 'single-float)))
         (pos (v3:+ sun-pos (v3:+ rel-pos off))))
    (let ((thrust (if position
                      (v3:*s (v3:normalize (v3:- position earth-pos))
                             (coerce (* thrust-factor (* 0.5 mass)) 'single-float))
                      (v! 0 0 0))))
      (when *vessel*
        (let* ((vessel-pos (slot-value *vessel* 'pos))
               (thrust (if position
                           (v3:*s (if relative-p
                                      position
                                      (v3:normalize (v3:- position vessel-pos)))
                                 (coerce (* thrust-factor (* 0.5 mass)) 'single-float))
                          (v! 0 0 0))))
          (setf (thrust *vessel*) thrust))
        (return-from add-planet))
      (stop-sound :intro)
      (play-sound :add-planet)
      (let* ((name (format nil "Pluto ~D" (incf *pluto-copies*)))
             (vel earth-vel)
             (radius 1185000d0) ;; pluto
             ;;(radius 1737500d0) ;; moon
             ;;(radius 6.3781d6) ;; earth
             (new-pluto (make-instance 'vessel
                                       :name name
                                       :mass mass
                                       :pos pos
                                       :vel vel
                                       :thrust thrust
                                       :fuel 1
                                       :radius radius
                                       )))
        (setq *gl-pluto* (add-gl-planet :name name :radius (* 1520 *gl-scale* radius) :pos (v! 0 0 0) :texture "plu0rss1.jpg" :day (/ 24 -153.3)))
        (setq *vessel* new-pluto)
        (setq *cheated* nil)
        ;;(describe new-pluto)
        ;;(format t "from earth: ~A~%" (v3:- pos earth-pos))
        (setq *all-objs* (append *all-objs* (list new-pluto)))))))


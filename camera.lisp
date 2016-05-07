;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;;
;; ======================================================
;; This is an example camera class.
;; It doesnt so much special and isnt very optimized but
;; it exists to show that cepl doesnt require you to use
;; the cepl.camera library to make a camera.
;;
;; This camera also doesnt use the 'space type from cepl
;; so this shows what code looks like without that
;; feature
;; ======================================================

(in-package :cepler)

;;;--------------------------------------------------------------

(defclass camera ()
  ((cam->clip :type (simple-array single-float (16)) :reader cam->clip)
   (cam->clip-func :initform nil :initarg :cam->clip-func )
   (frame-size :reader frame-size :initarg :frame-size
               :initform (jungl:viewport-resolution (jungl:current-viewport)))
   (near :type single-float :reader near :initarg :near)
   (far :type single-float :reader far :initarg :far)
   (fov :type single-float :reader fov :initarg :fov)))

(defmethod update-cam->clip ((camera camera))
  (setf (slot-value camera 'cam->clip)
        (funcall (slot-value camera 'cam->clip-func)
                 (first (frame-size camera))
                 (second (frame-size camera))
                 (near camera)
                 (far camera)
                 (fov camera))))

(defmethod (setf near) (distance (camera camera))
  (setf (slot-value camera 'near) distance)
  (update-cam->clip camera))

(defmethod (setf far) (distance (camera camera))
  (setf (slot-value camera 'far) distance)
  (update-cam->clip camera))

(defmethod (setf fov) (angle (camera camera))
  (setf (slot-value camera 'fov) angle)
  (update-cam->clip camera))

(defmethod (setf frame-size) (frame (camera camera))
  (let ((frame
         (etypecase frame
            (simple-array (list (aref frame 0)
				(aref frame 1)))

            (jungl:viewport (jungl:viewport-resolution frame))
            (list frame))))
    (setf (slot-value camera 'frame-size) frame))
  (update-cam->clip camera))

(defgeneric world->cam (camera))

(defclass pos-dir-cam (camera)
  ((world-up :type (simple-array single-float (3))
             :initform (v3:make 0.0 1.0 0.0)
             :initarg :world-up
             :accessor world-up)
   (position :type (simple-array single-float (3))
             :initform (v3:make 0.0 0.0 0.0)
             :initarg :pos
             :accessor cepl-generics:pos)
   (direction :type (simple-array single-float (3))
              :initform (v3:make 0.0 0.0 -1.0)
              :initarg :dir
              :accessor cepl-generics:dir)))

;; (defmethod look-at ((camera pos-dir-cam) point-vec3)
;;   (with-slots (world-up position direction) camera
;;     (setf direction (v3:normalize (v3:- point-vec3 position)))))

(defmethod world->cam ((camera pos-dir-cam))
  (with-slots (world-up position direction) camera
    (let* ((up (v3:normalize
                (v3:- world-up
                        (v3:*s direction (v3:dot world-up direction)))))
           (side (v3:cross direction up))
           (rotate (m3:from-rows side up (v3:negate direction)))
           (eye-inv (v3:negate (m3:*v rotate position)))
           (result (m4:rotation-from-mat3 rotate)))
      (setf (m4:melm result 0 3) (aref eye-inv 0)
            (m4:melm result 1 3) (aref eye-inv 1)
            (m4:melm result 2 3) (aref eye-inv 2))
      result)))

(defun make-camera (&optional (frame (jungl:current-viewport))
                      (near 1.0) (far 1000.0) (fov #.(coerce (/ pi 3) 'single-float))
                      (cam->clip-function #'rtg-math.projection:perspective))
  (let* ((frame
          (etypecase frame
            ((simple-array single-float (2)) (list (aref frame 0)
                                                   (aref frame 1)))

            (jungl:viewport (jungl:viewport-resolution frame))
            (list frame)))
         (camera (make-instance 'pos-dir-cam
                                :cam->clip-func cam->clip-function
                                :near near :far far :fov fov
                                :frame-size frame)))
    (update-cam->clip camera)
    camera))

;;;--------------------------------------------------------------

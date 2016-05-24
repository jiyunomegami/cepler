; Tanstaafl - A free space flight simulator
; Copyright (C) 2009  Neil Forrester
; 
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; A VSOP series will be an object of the class vsop-series
;;;; A VSOP term will be a list of 3 floats.
;;;; A VSOP series set will be a list of VSOP series.

(in-package :cepler)

;; "Gravitational constant"
(defvar *G* 6.673d-11)
(declaim (type double-float *G*))
(defvar *m-per-au* 149598000000d0)
(declaim (type double-float *m-per-au*))
(defvar *time-acceleration* 1.0d0)

(defclass vsop-series ()
  ((alpha
    :initform 0
    :type (signed-byte 32)
    :initarg :alpha)
   (nterms
    :initform 0
    :type fixnum
    :initarg :nterms)
   (terms
    :initform ()
    :type (simple-array double-float (* 3))
    :initarg :terms)))

;;;; All the VSOP87 data is contained in the subdirectory vsop87

(defun read-int-from-string (str start end)
  (read-from-string (subseq str start end)))

(defun read-fixed-point-as-double-from-string (str start end)
  (read-from-string (concatenate 'string (subseq str start end) "d0")))

;;; This function reads the VSOP87 data files.
;;; This function does not do robust text processing.
;;; It relies heavily on the fact that the VSOP87 data files
;;; have a well defined structure, laid out in vsop87.doc.
(defun vsop87-file-reader (filename)
  (format t "reading from ~A~%" filename)
  (when (not (probe-file filename))
    (format t "  file ~A not found~%" filename)
    (return-from vsop87-file-reader nil))
  (with-open-file (file filename)        
    (let ((variable-to-series-set-plist ()))
      (loop for line = (read-line file nil) while line do
           (let ((variable (read-from-string (concatenate 'string ":"
                                                          (let ((coord-index (read-int-from-string line 41 42)))
                                                            (subseq (subseq line (position #\( line) (position #\) line)) coord-index (1+ coord-index)))))))
             ;;(format t "   variable: ~A~%" variable)
             ;;(format t "   alpha: ~A~%" (read-int-from-string line 59 60))
             (setf (getf variable-to-series-set-plist variable)
                   (append (getf variable-to-series-set-plist variable)
                           (list
                            (let* ((terms-list
                                    (let (term-line)
                                      (loop for i from 1 upto (read-int-from-string line 60 67) collecting
                                           (progn
                                             (setf term-line (read-line file))
                                             (list
                                              (read-fixed-point-as-double-from-string term-line  79  97)
                                              (read-fixed-point-as-double-from-string term-line  97 111)
                                              (read-fixed-point-as-double-from-string term-line 111 131))))))
                                   (terms
                                    (let (array-terms)
                                      (dolist (term terms-list
                                               (make-array (list (length array-terms) 3)
                                                           :element-type 'double-float
                                                           :initial-contents (nreverse array-terms)))
                                        (push (make-array 3
                                                          :element-type 'double-float
                                                          :initial-contents term)
                                              array-terms)))))
                              (make-instance 'vsop-series
                                             :alpha (read-int-from-string line 59 60)
                                             :terms terms
                                             :nterms (car (array-dimensions terms)))))))))
      variable-to-series-set-plist)))

(defmacro defvar-vsop87-for-body (body)
  (let ((varname-x (read-from-string (format nil "*vsop-series-set-~a-x*" body)))
        (varname-y (read-from-string (format nil "*vsop-series-set-~a-y*" body)))
        (varname-z (read-from-string (format nil "*vsop-series-set-~a-z*" body))))
    `(progn
       (defvar ,varname-x)
       (defvar ,varname-y)
       (defvar ,varname-z))))

(defmacro build-vsop87-for-body (body &optional (version "A"))
  (let ((varname-x (read-from-string (format nil "*vsop-series-set-~a-x*" body)))
        (varname-y (read-from-string (format nil "*vsop-series-set-~a-y*" body)))
        (varname-z (read-from-string (format nil "*vsop-series-set-~a-z*" body))))
    `(let ((vsop87-plist (vsop87-file-reader ,(concatenate 'string "vsop87/VSOP87" version "." (subseq body 0 3)))))
       (setq ,varname-x (getf vsop87-plist :x))
       (setq ,varname-y (getf vsop87-plist :y))
       (setq ,varname-z (getf vsop87-plist :z)))))

(defvar-vsop87-for-body "sun")
(defvar-vsop87-for-body "mercury")
(defvar-vsop87-for-body "venus")
(defvar-vsop87-for-body "earth")
(defvar-vsop87-for-body "mars")
(defvar-vsop87-for-body "jupiter")
(defvar-vsop87-for-body "saturn")
(defvar-vsop87-for-body "uranus")
(defvar-vsop87-for-body "neptune")
;; Earth-Moon Barycenter
(defvar-vsop87-for-body "emb")

(defun build-vsop87 ()
  (build-vsop87-for-body "sun")
  (build-vsop87-for-body "mercury")
  (build-vsop87-for-body "venus")
  (build-vsop87-for-body "earth")
  (build-vsop87-for-body "mars")
  (build-vsop87-for-body "jupiter")
  (build-vsop87-for-body "saturn")
  (build-vsop87-for-body "uranus")
  (build-vsop87-for-body "neptune")
  ;; Earth-Moon Barycenter
  (build-vsop87-for-body "emb"))

(build-vsop87)

(declaim (inline evaluate-series))
(declaim (ftype (function (vsop-series (double-float 0.0d0)) double-float) evaluate-series))
(defun evaluate-series (series julian-millenium)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type vsop-series series)
           (type (double-float 0.0d0) julian-millenium))
  (let* ((sum 0.0d0)
         (terms (slot-value series 'terms))
         (nterms (slot-value series 'nterms)))
    (declare (type double-float sum)
             (type fixnum nterms)
             (type (simple-array double-float (* 3)) terms))
    (dotimes (i nterms (* (expt julian-millenium
                                (the (signed-byte 32) (slot-value series 'alpha)))
                          sum))
      (declare (type fixnum i))
      (incf sum (* (aref terms i 0)
                   (cos (+ (aref terms i 1)
                           (* julian-millenium (aref terms i 2)))))))))

(declaim (inline evaluate-series-set))
(declaim (ftype (function (list (double-float 0.0d0)) double-float) evaluate-series-set))
(defun evaluate-series-set (series-set julian-millenium)
  (declare (optimize (speed 3))
           (type double-float julian-millenium))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dolist (series series-set sum)
      (declare (type vsop-series series))
      (incf sum (the double-float (evaluate-series series julian-millenium))))))

(defclass vsop-reference-point ()
  ((pos
     :initarg :pos
     :initform (v! 0 0 0))
   (epoch
    :initarg :epoch)))

(defun sign(x)
  (cond
    ((> x 0)  1)
    ((= x 0)  0)
    ((< x 0) -1)))

(defun atan2 (x y)
  (let ((angle (if (not (= x 0)) (atan (abs (/ y x))))))
    (cond
      ((> x 0) (* (sign y) angle))
      ((< x 0) (* (sign y) (- pi angle)))
      ((= x 0) (* (sign y) (/ pi 2))))))

(declaim (inline convert-from-spherical))
(declaim (ftype (function (double-float double-float double-float) (simple-array single-float (3))) convert-from-spherical))
(defun convert-from-spherical (longitude latitude radius)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type double-float longitude latitude radius))
  (v!
   (* radius (cos longitude) (cos latitude))
   (* radius (cos longitude) (sin latitude))
   (* radius (sin longitude))))

(declaim (inline convert-to-spherical))
(declaim (ftype (function (double-float double-float double-float) (simple-array single-float (3))) convert-to-spherical))
(defun convert-to-spherical (x y z)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type double-float x y z))
  (v!
   (if (= x y z 0.0)
       0.0
       (* -1.0 (- (acos (/ z (expt (+ (expt x 2) (expt y 2) (expt z 2)) 0.5))) (/ pi 2))))
   (atan2 x y)
   (expt (+ (expt x 2) (expt y 2) (expt z 2)) 0.5)))

;;; Takes the time in seconds since the epoch, and VSOP series sets for
;;; latitude, longitude, and radius (heliocentric spherical coordinates),
;;; and returns a vsop-reference-point
(defun vsop-compute-reference-point (epoch vsop-x vsop-y vsop-z)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (double-float 0.0d0) epoch))
  (let ((julian-millenium (* epoch #.(/ 1.0 (* 60 60 24 365.25 1000.0d0)))))
    (declare (type (double-float 0.0d0) julian-millenium))
    (declare (type double-float julian-millenium))
    (make-instance 'vsop-reference-point
                   :epoch epoch
                   :pos (convert-to-spherical
                         (* *m-per-au* (evaluate-series-set vsop-x julian-millenium))
                         (* *m-per-au* (evaluate-series-set vsop-y julian-millenium))
                         (* *m-per-au* (evaluate-series-set vsop-z julian-millenium))))))

;;; Takes the time in seconds since the epoch, VSOP series sets for
;;; latitude, longitude, and radius (heliocentric spherical coordinates),
;;; an interval in seconds, and 2 vsop-reference-points. If the time is
;;; between the first and second reference points, it interpolates a
;;; position between them. Otherwise, it computes new reference points an
;;; interval apart. The function returns a vector-3, with the HELIOCENTRIC
;;; position, and the two reference points that were actually used.
;;; Careful with that heliocentric position. Wouldn't want to forget to add
;;; it to the position of Sol.
(let ((time-of-last-vsop-ref-recomputation 0.0d0) (first-time-step t))
  (declare (type double-float time-of-last-vsop-ref-recomputation))
  (defun vsop-compute-position (epoch vsop-x vsop-y vsop-z interval vsop-ref-1 vsop-ref-2)
    (declare (optimize (speed 3) (debug 0))
             (type double-float interval))
    (when (or t ;; interpolation didn't work
              #+nil
              (and (or (not (<= (slot-value vsop-ref-1 'epoch) epoch (slot-value vsop-ref-2 'epoch)))
                       (< (* 1.1 interval *time-acceleration*) (- (slot-value vsop-ref-2 'epoch) (slot-value vsop-ref-1 'epoch))))
                   (or (not (= time-of-last-vsop-ref-recomputation epoch))
                       first-time-step)))
      (if (and first-time-step
               (not (= time-of-last-vsop-ref-recomputation epoch))
               (not (= 0.0d0 time-of-last-vsop-ref-recomputation)))
          (setf first-time-step nil)) ; to ensure that we do all the vsop87 computations before the simulation gets started.
      (setf time-of-last-vsop-ref-recomputation epoch) ; to ensure that lots of vsop87 computations all at once don't slow down the simulation too much. they'll keep after all.
      (setf vsop-ref-1 (vsop-compute-reference-point                                     epoch  vsop-x vsop-y vsop-z))
      (setf vsop-ref-2 (vsop-compute-reference-point (+ (* interval (coerce *time-acceleration* 'double-float)) epoch) vsop-x vsop-y vsop-z)))
    (let* ((epoch-1 (slot-value vsop-ref-1 'epoch))
           (epoch-2 (slot-value vsop-ref-2 'epoch))
           (interval (- epoch-2 epoch-1))
           (short-interval (- epoch epoch-1)))
      (declare (type double-float epoch-1 epoch-2 interval short-interval))
      (values
       (with-slots ((vsop-ref-1-pos pos)) vsop-ref-1
         (declare (type (simple-array single-float (3)) vsop-ref-1-pos))
         (with-slots ((vsop-ref-2-pos pos)) vsop-ref-2
           (declare (type (simple-array single-float (3)) vsop-ref-2-pos))
           (convert-from-spherical
            (+ (coerce (v:x vsop-ref-1-pos) 'double-float) ;; longitude
               (* short-interval (/ (coerce (- (v:x vsop-ref-2-pos) (v:x vsop-ref-1-pos)) 'double-float)
                                    interval)))
             (+ (coerce (v:y vsop-ref-1-pos) 'double-float) ;; latitude
                (* short-interval (/ (coerce (- (v:y vsop-ref-2-pos) (v:y vsop-ref-1-pos)) 'double-float)
                                     interval)))
             (+ (coerce (v:z vsop-ref-1-pos) 'double-float) ;; radius
                (* short-interval (/ (coerce (- (v:z vsop-ref-2-pos) (v:z vsop-ref-1-pos)) 'double-float)
                                     interval))))))
       vsop-ref-1
       vsop-ref-2))))

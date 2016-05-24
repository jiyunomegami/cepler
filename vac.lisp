;; load this to use ELP2000-82
;; Modifications to Vacietis are required

(in-package :cepler)

(asdf:operate 'asdf:load-op :vacietis)

(declaim (optimize (speed 3) (debug 0) (safety 1)))
(declaim (inline ln_range_radians ln_range_radians2))

(defun compile-stuff (&optional elp)
  (when elp
    (loop for n from 1 to 36 do
         (vacietis:load-c-file (format nil "elp/elp~D.c" n))))
  (vacietis:load-c-file "elp/lunar.c"))

(defvar _arg (make-array 5 :element-type 'double-float :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)))
(defun test-posn ()
  (time
   (sum_series_elp10 _arg))
  (time
   (let ((jd 6002.8223d0))
     (dotimes (i 40)
       (format t "~S~%" (ln_get_lunar_geo_posn jd))
       (incf jd 1.0)))))

(compile-stuff t)
(setq *use-elp* t)

;; load this to use ELP2000-82
;; Modifications to Vacietis are required

(defpackage #:cepler.elp
  (:use #:cl)
  (:export 
   :ln_get_lunar_geo_posn))

(in-package :cepler.elp)

(asdf:operate 'asdf:load-op :vacietis)

(setq vacietis::*use-alien-types* t)
(setq vacietis::*debug* nil)
(setq vacietis::*verbose* t)

(defun compile-stuff (&optional elp)
  (when elp
    (loop for n from 1 to 36 do
         (vacietis:load-c-file (format nil "elp/elp~D.c" n))))
  (vacietis:load-c-file "elp/lunar.c"))

(compile-stuff :all)

(in-package :cepler)

(setq *use-elp* t)

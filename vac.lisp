;; load this to use ELP2000-82
;; Modifications to Vacietis are required

(in-package :cepler)

(asdf:operate 'asdf:load-op :vacietis)

(defun struct-to-list (vl)
  (loop for x in (slot-value vl 'vacietis::elements)
     nconc (typecase x
             (vacietis::vector-literal
              (list (struct-to-vector x)))
             (t
              (list (eval x))))))

(defun struct-to-vector (vl)
  (apply #'vector (struct-to-list vl)))

(defun vectorize-struct-array (array)
  (typecase array
    (vacietis::memptr
     (vectorize-struct-array (slot-value array 'vacietis::mem)))
    (t
     (apply #'vector
            (loop for i from 0 upto (1- (length array))
               when (aref array i)
               collect (struct-to-vector (aref array i)))))))

(defun vectorize-array (array)
  (typecase array
    (vacietis::memptr
     (slot-value array 'vacietis::mem))))

(defmacro do-elps (start end)
  `(progn
     ,@(loop for n from start to end
          collect `(setq ,(intern (format nil "ELP~D" n))
                         (vectorize-struct-array
                          ,(intern (format nil "ELP~D" n)))))))

(defun vectorize-stuff (elp)
  (setq W1 (vectorize-array W1))
  (setq W2 (vectorize-array W2))
  (setq W3 (vectorize-array W3))
  (setq earth (vectorize-array earth))
  (setq peri (vectorize-array peri))
  (setq del (vectorize-struct-array del))
  (setq zeta (vectorize-array zeta))
  (setq p (vectorize-struct-array p))
  (when elp
    ;;(setq elp1 (vectorize-struct-array elp1))
    (do-elps 1 36))
  (setq moon (vectorize-array moon)))

(defun compile-stuff (&optional elp)
  (vacietis:load-c-file "elp/lunar.c")
  (when elp
    (loop for n from 1 to 36 do
         (vacietis:load-c-file (format nil "elp/elp~D.c" n))))
  (vectorize-stuff elp))
  
(defmacro macroexpansion-of (form &environment env)
  `',(sb-cltl2:macroexpand-all form env))

(defmacro my-defun/1 (name arglist &body body)
  (let ((declarations (loop for x in body
                         when (and (listp x) (eq 'declare (car x)))
                         collect x))
        (body (loop for x in body
                 when (not (and (listp x) (eq 'declare (car x))))
                 collect x)))
    `(macrolet ((ceil (x)
                  `(ceiling ,x))
                (vacietis.c:< (&rest rest)
                  `(< ,@rest))
                (vacietis.c:+ (&rest rest)
                  `(+ ,@rest))
                (vacietis.c:- (&rest rest)
                  `(- ,@rest))
                (vacietis.c:* (&rest rest)
                  `(* ,@rest))
                (vacietis.c:/ (&rest rest)
                  `(/ ,@rest))
                (vacietis.c:[] (array-var index)
                  `(aref ,array-var ,index))
                (vacietis.c:|.|
                  (struct-var slot-index)
                  `(aref ,struct-var ,slot-index))
                (vacietis:allocate-memory (size)
                  `(make-array ,size :element-type 'double-float))
                (vacietis.c:for ((variable-declarations
                                  initializations
                                  test
                                  step)
                                 &body body)
                  (declare (ignore variable-declarations))
                  `(progn
                     ,initializations
                     (loop while ,test
                        do 
                          ,@body
                          ,step))))
       (format t "~A ~S~%~S~%~S~%~S~%" ',name ',arglist
               ',declarations
               (macroexpansion-of ,@body)
               ;;body
               nil
               )
       (eval (append nil (list 'defun ',name ',arglist
                               ;;'(declare (optimize (speed 3) (safety 0)))
                               '(declare (optimize (speed 3)))
                               (when ',declarations ',@declarations)
                               (macroexpansion-of ,@body))))
       #+nil
       (macroexpansion-of ,@body))))


(defun test-posn ()
  (let ((ct 6002.8223d0))
    (dotimes (i 10)
      (format t "~S~%" (ln_get_lunar_geo_posn ct))
      (incf ct 1.0))))

(compile-stuff t)
(setq *use-elp* t)


;;   seconds  |     gc     |     consed    |   calls   |  sec/call  |  name  
;; ---------------------------------------------------------------
;;      1.733 |      0.216 | 1,304,590,000 |        67 |   0.025860 | SUM_SERIES_ELP10
;;      0.786 |      0.080 |   604,530,752 |        67 |   0.011736 | SUM_SERIES_ELP12
;;      0.624 |      0.080 |   477,227,104 |        67 |   0.009313 | SUM_SERIES_ELP11
;;      0.546 |      0.072 |   421,097,920 |    18,224 |   0.000030 | EVALUATE-SERIES
;;      0.531 |      0.064 |   409,989,888 |        67 |   0.007926 | SUM_SERIES_ELP13
;;      0.431 |      0.004 |    84,279,296 | 2,537,424 |   0.000000 | LN_RANGE_RADIANS2
;;      0.190 |      0.020 |   106,920,736 |        67 |   0.002832 | SUM_SERIES_ELP1
;;      0.161 |      0.024 |   159,317,280 |        67 |   0.002406 | SUM_SERIES_ELP15
;;      0.122 |      0.016 |    77,561,856 |        67 |   0.001815 | SUM_SERIES_ELP14
;;      0.118 |      0.024 |    96,892,480 |        67 |   0.001763 | SUM_SERIES_ELP2
;;      0.079 |      0.000 |    75,104,256 |        67 |   0.001178 | SUM_SERIES_ELP3
;;      0.031 |      0.000 |    15,794,176 |        67 |   0.000462 | SUM_SERIES_ELP5
;;      0.031 |      0.000 |    22,642,688 |        67 |   0.000457 | SUM_SERIES_ELP19
;;      0.029 |      0.000 |    17,103,968 |        67 |   0.000433 | SUM_SERIES_ELP21
;;      0.024 |      0.000 |        32,768 |       134 |   0.000179 | FRAG-POINT-LIGHT-BUMP
;;      0.024 |      0.000 |        32,768 |     1,162 |   0.000020 | RTT-DRAW-TEXT
;;      0.017 |      0.000 |    16,777,216 |        67 |   0.000253 | SUM_SERIES_ELP16
;;      0.011 |      0.012 |       393,216 |        67 |   0.000171 | SUM_SERIES_ELP29
;;      0.008 |      0.000 |    19,300,352 |        67 |   0.000123 | SUM_SERIES_ELP20
;;      0.008 |      0.000 |             0 |        67 |   0.000119 | FRAG-POINT-LIGHT-CLOUDS-BUMP
;;      0.008 |      0.000 |        32,768 |       469 |   0.000017 | FRAG-POINT-LIGHT-NONM
;;      0.007 |      0.000 |    11,632,640 |        67 |   0.000109 | SUM_SERIES_ELP18
;;      0.006 |      0.000 |    11,008,800 |        67 |   0.000092 | SUM_SERIES_ELP6
;;      0.006 |      0.000 |    14,974,976 |        67 |   0.000086 | SUM_SERIES_ELP17
;;      0.006 |      0.000 |    16,809,984 |        67 |   0.000084 | SUM_SERIES_ELP4
;;      0.004 |      0.000 |             0 |        67 |   0.000059 | DRAW-RTTCONSOLE
;;      0.004 |      0.000 |             0 |        67 |   0.000059 | DRAW-SKY
;;      0.004 |      0.000 |        32,768 |        67 |   0.000059 | FPS-DISPLAY
;;      0.004 |      0.000 |             0 |       134 |   0.000030 | DRAW-RINGS
;;      0.004 |      0.000 |       262,144 |        67 |   0.000057 | SUM_SERIES_ELP26
;;      0.004 |      0.000 |       131,040 |        67 |   0.000056 | STEP-GL
;;      0.004 |      0.000 |       524,080 |       200 |   0.000019 | RENDER-TEXT
;;      0.004 |      0.000 |       294,784 |       938 |   0.000004 | WORLD->CAM
;;      0.004 |      0.000 |             0 |     1,206 |   0.000003 | ATAN2
;;      0.004 |      0.000 |       458,752 |     3,618 |   0.000001 | EVALUATE-SERIES-SET
;;      0.003 |      0.000 |             0 |     1,474 |   0.000002 | GL-PLANET-DAY
;;      0.003 |      0.000 |       327,680 |        67 |   0.000051 | SUM_SERIES_ELP35
;;      0.003 |      0.000 |       753,664 |        67 |   0.000051 | SUM_SERIES_ELP7
;;      0.003 |      0.000 |             0 |     2,022 |   0.000002 | CALC-GLYPH-X-SIZE
;;      0.003 |      0.000 |     1,179,648 |        67 |   0.000048 | SUM_SERIES_ELP36
;;      0.003 |      0.000 |     1,048,576 |        67 |   0.000047 | SUM_SERIES_ELP28
;;      0.003 |      0.000 |        98,304 |     1,206 |   0.000002 | CONVERT-TO-SPHERICAL
;;      0.003 |      0.000 |        32,768 |       871 |   0.000003 | WORLD->CLIP
;;      0.003 |      0.000 |       163,808 |       180 |   0.000014 | CALC-TEXT-WIDTH
;;      0.003 |      0.000 |       491,072 |       670 |   0.000004 | CAM-LIGHT-MODEL->WORLD
;;      0.002 |      0.000 |        32,768 |       603 |   0.000003 | FIND-GL-PLANET
;;      0.001 |      0.000 |        98,144 |       737 |   0.000002 | MODEL->CLIP
;;      0.000 |      0.000 |             0 |       737 |   0.000000 | GL-PLANET-STREAM
;;      0.000 |      0.000 |       848,928 |     2,022 |   0.000000 | GET-GLYPH-SIZE
;;      0.000 |      0.000 |       753,664 |        67 |   0.000000 | SUM_SERIES_ELP8
;;      0.000 |      0.000 |             0 |       670 |   0.000000 | GL-PLANET-NORMAL-TEXTURE
;;      0.000 |      0.000 |             0 |     3,015 |   0.000000 | GL-PLANET-NAME
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | FRAG-POINT-LIGHT
;;      0.000 |      0.000 |             0 |       871 |   0.000000 | GL-PLANET-RADIUS
;;      0.000 |      0.000 |       753,520 |       670 |   0.000000 | RENDER-PLANET
;;      0.000 |      0.000 |     1,736,704 |        67 |   0.000000 | SUM_SERIES_ELP34
;;      0.000 |      0.000 |        65,408 |        67 |   0.000000 | UPDATE-CONSOLE
;;      0.000 |      0.000 |        32,768 |       670 |   0.000000 | INTEGRATE-ACC-TO-VEL
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | TIMESTEP
;;      0.000 |      0.000 |     1,078,112 |     2,022 |   0.000000 | GET-GLYPH
;;      0.000 |      0.000 |        32,768 |        20 |   0.000000 | RENDER-OVERLAY
;;      0.000 |      0.000 |             0 |     2,077 |   0.000000 | GL-PLANET-CLOUDS-TEXTURE
;;      0.000 |      0.000 |             0 |       134 |   0.000000 | MODEL->CLIP-NOROT
;;      0.000 |      0.000 |             0 |       670 |   0.000000 | INTEGRATE-VEL-TO-POS
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | TANSTAAFL-MAIN-LOOP-STEP
;;      0.000 |      0.000 |        65,520 |       603 |   0.000000 | VSOP-COMPUTE-POSITION
;;      0.000 |      0.000 |        65,488 |        67 |   0.000000 | RENDER-RINGS
;;      0.000 |      0.000 |       228,912 |        67 |   0.000000 | LN_GET_LUNAR_GEO_POSN
;;      0.000 |      0.000 |       131,072 |        67 |   0.000000 | SUM_SERIES_ELP23
;;      0.000 |      0.000 |       557,056 |        67 |   0.000000 | SUM_SERIES_ELP31
;;      0.000 |      0.000 |     1,048,576 |        67 |   0.000000 | SUM_SERIES_ELP30
;;      0.000 |      0.000 |             0 |       670 |   0.000000 | GL-PLANET-TEXTURE
;;      0.000 |      0.000 |        32,752 |       737 |   0.000000 | RELATIVE-POSITION
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | UPDATE-POSITIONS
;;      0.000 |      0.000 |       163,824 |     1,340 |   0.000000 | SUB
;;      0.000 |      0.000 |             0 |     1,541 |   0.000000 | ROT
;;      0.000 |      0.000 |             0 |       737 |   0.000000 | (SETF ROT)
;;      0.000 |      0.000 |        32,704 |       134 |   0.000000 | MODEL->WORLD-NOROT
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | (SETF DIR)
;;      0.000 |      0.000 |             0 |       670 |   0.000000 | ADD
;;      0.000 |      0.000 |       229,312 |       670 |   0.000000 | INTEGRATE-ANG-VEL-TO-ANG-POS
;;      0.000 |      0.000 |             0 |       670 |   0.000000 | GL-PLANET-BUMP-TEXTURE
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | %KEYBOARD-CALLBACK
;;      0.000 |      0.000 |        98,288 |        67 |   0.000000 | RENDER-CLOUDS
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | CAMERA-FOLLOW
;;      0.000 |      0.000 |       425,984 |        67 |   0.000000 | SUM_SERIES_ELP27
;;      0.000 |      0.000 |             0 |       670 |   0.000000 | INTEGRATE-ANG-ACC-TO-ANG-VEL
;;      0.000 |      0.000 |        32,768 |        67 |   0.000000 | SUM_SERIES_ELP24
;;      0.000 |      0.000 |             0 |       670 |   0.000000 | GL-PLANET-NIGHT-TEXTURE
;;      0.000 |      0.000 |             0 |       670 |   0.000000 | MAKE-QUATERNION
;;      0.000 |      0.000 |       130,976 |        67 |   0.000000 | %RUN-LOOP
;;      0.000 |      0.000 |        65,536 |        67 |   0.000000 | SUM_SERIES_ELP22
;;      0.000 |      0.000 |       196,608 |        67 |   0.000000 | SUM_SERIES_ELP25
;;      0.000 |      0.000 |        65,536 |       603 |   0.000000 | CONVERT-FROM-SPHERICAL
;;      0.000 |      0.000 |       229,200 |       737 |   0.000000 | MODEL->WORLD
;;      0.000 |      0.000 |        98,304 |        67 |   0.000000 | SUM_SERIES_ELP32
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | FPS-LIMIT-DELAY
;;      0.000 |      0.000 |             0 |     1,608 |   0.000000 | GL-PLANET-OBLIQUITY
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | (SETF WORLD-UP)
;;      0.000 |      0.000 |       458,576 |     1,206 |   0.000000 | VSOP-COMPUTE-REFERENCE-POINT
;;      0.000 |      0.000 |       229,312 |       670 |   0.000000 | COMPUTE-FORCES
;;      0.000 |      0.000 |       294,912 |        67 |   0.000000 | SUM_SERIES_ELP9
;;      0.000 |      0.000 |             0 |       938 |   0.000000 | CAM->CLIP
;;      0.000 |      0.000 |             0 |        67 |   0.000000 | RENDER-SKY
;;      0.000 |      0.000 |       262,096 |     2,010 |   0.000000 | MULT
;;      0.000 |      0.000 |       294,912 |        67 |   0.000000 | SUM_SERIES_ELP33
;; ---------------------------------------------------------------
;;      5.608 |      0.612 | 3,980,717,984 | 2,606,156 |            | Total

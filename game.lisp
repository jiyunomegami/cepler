(in-package :cepler)

(defvar *game-dir*
  (asdf:system-relative-pathname :cepler ""))

(defvar *fps* 0)
(defun fps-multiplier (n)
  (if (> *fps* 0)
      (/ n *fps*)
      n))
(defvar *is-fullscreen* nil)
(defvar *show-console* nil)

(defvar *targeting* nil)
(defvar *following* nil)
(defvar *follow-sun-lock* nil)

(defparameter *time-acceleration-factor* 2)

(defparameter *movement-scale* 0.07)
(defparameter *mouse-sensitivity* 0.005)

;; Should be far enough away from the edges of the window
(defparameter *mouse-x-pos* 400)
(defparameter *mouse-y-pos* 300)

;;;;;;;;;; fonts ;;;;;;;;;;;;;
(defvar *face* nil)
(defvar *glyphs* (make-hash-table :test #'equal))
(defvar *glyph-sizes* (make-hash-table :test #'equal))
(defparameter *base-glyph-array-size* 32)
(defvar *glyph-array-size* *base-glyph-array-size*)

;; the real function is in ft.lisp
(defun init-font (&optional (font-file "/usr/share/fonts/opentype/ipafont-gothic/ipag.ttf"))
  (declare (ignore font-file))
  (setq *glyphs* (make-hash-table :test #'equal)))
;; the real function is in ft.lisp
(defun get-glyph (c)
  (let ((str (princ-to-string c)))
    (gethash str *glyphs*)))
;; the real function is in ft.lisp
(defun get-glyph-size (c)
  (let ((str (princ-to-string c)))
    (gethash str *glyph-sizes*)))

(defvar *paused* nil)
(defvar *was-paused* nil)
(defun pause ()
  (when *paused*
    (setq *was-paused* t))
  (setq *paused* (not *paused*)))

;; a box is just the position and the rotation quaternion.
;; left over from the cepl example.
(defstruct box
  (pos (v! 0 0 -10))
  (rot (q:identity)))

(defstruct gl-planet
  (name "")
  (radius 1)
  (day 1)
  (box (make-box))
  (data)
  (index)
  (stream)
  (texture)
  (normal-texture)
  (bump-texture))

(defvar *gl-planets* nil)

(defvar *gl-pluto* nil)

(defvar *sphere-data* nil)
(defvar *sphere-index* nil)
(defvar *sphere-stream* nil)

;; calling cepl.devil:load-image-to-texture too many times
;; results in OpenGL signalled (1285 . OUT-OF-MEMORY) from TEX-STORAGE-2D.
(defvar *texture-cache* (make-hash-table :test #'equal))
(defun load-planet-texture (texture &optional (dir "planets/"))
  (or (gethash texture *texture-cache*)
      (setf (gethash texture *texture-cache*)
            (cepl.devil:load-image-to-texture
             (merge-pathnames texture
                              (merge-pathnames dir *game-dir*))))))

(defvar *sun-actual-size* nil)

(defun add-gl-planet (&key
                        (name "")
                        (day 1)
                        (radius 0.5)
                        pos
                        (texture "ear.jpg"))
  (when *sun-actual-size*
    (unless (equalp name "Sun")
      (setq radius (* (/ 5.00 109) radius))))
  (let ((gl-planet
         (make-gl-planet
          :name name
          :radius radius
          :day day
          :box (make-box :pos pos)
          :data *sphere-data*
          :index *sphere-index*
          :stream *sphere-stream*
          :texture (load-planet-texture texture))))
    (push gl-planet *gl-planets*)
    gl-planet))

#+nil
(defun withnewdata-add-gl-planet (&key
                                    (name "")
                                    (day 1)
                                    (radius 0.5)
                                    pos
                                    (texture "ear.jpg"))
  (destructuring-bind (d i) (dendrite.primitives:sphere-data
                             ;;:radius radius
                             :lines-of-latitude 50
                             :lines-of-longitude 50)
    (let* ((data (make-gpu-array d :element-type 'g-pnt))
           (index (make-gpu-array i :element-type :ushort))
           (gl-planet
            (make-gl-planet
             :name name
             :radius radius
             :day day
             :box (make-box :pos pos)
             :data data
             :index index
             :stream (make-buffer-stream data :index-array index)
             :texture (load-planet-texture texture))))
      (push gl-planet *gl-planets*)
      gl-planet)))

;; Body	Diameter
;; (Earth = 1)
;; Sun	109
;; Mercury	.38
;; Venus	.95
;; Earth	1
;; Mars	.53
;; Jupiter	11.19
;; Saturn	9.40
;; Uranus	4.04
;; Neptune	3.88
;; Pluto	.18
;;                         Distance  O_Period				   
;; Name       #     Orbits (000 km)   (days)    Incl Eccen  Discoverer   Date   A.K.A.
;; -------  ---- ------- --------  --------  ----- -----  ----------   ----  -------
;; Sun         -      -        -         -       -     -       -           -   Sol (0)

;; Mercury    I    Sun        57910    87.97    7.00  0.21     -           -   (0)
;; Venus      II   Sun       108200   224.70    3.39  0.01     -           -   (0)
;; Earth      III  Sun       149600   365.26    0.00  0.02     -           -   (0)
;; Mars       IV   Sun       227940   686.98    1.85  0.09     -           -   (0)
;; Jupiter    V    Sun       778330  4332.71    1.31  0.05     -           -   (0)
;; Saturn     VI   Sun      1429400 10759.50    2.49  0.06     -           -   (0)
;; Uranus     VII  Sun      2870990 30685.00    0.77  0.05  Herschel     1781  (0)
;; Neptune    VIII Sun      4504300 60190.00    1.77  0.01  Adams(9)     1846  (0)
;; Pluto      IX   Sun      5913520 90550      17.15  0.25  Tombaugh     1930  (0)

(defvar *planet-names* '("Sun" "Mercury" "Venus" "Earth" "Mars" "Jupiter" "Saturn" "Uranus" "Neptune" "Pluto"))
(defvar *planet-texture-sets* (make-hash-table :test #'equalp))
(defvar *planet-texture-set* "set2")

(defmacro deftexset (name dir &rest planets)
  (let ((varname (read-from-string (format nil "*planet-texture-set-~A*" name))))
    `(progn
       (defvar ,varname)
       (setq ,varname
         (list ,name ,dir ,@planets))
       (setf (gethash ,name *planet-texture-sets*) ,varname))))

;; SOLAR SYSTEM SIMULATOR: http://maps.jpl.nasa.gov/
;; Mercury: http://laps.noaa.gov/albers/sos/sos.html (mercury_rgb_cyl_www.jpg)
(deftexset "JPL" "planets/"
  "sun.jpg" "mercury.jpg" "ven0mss2.jpg" "ear0xuu2.jpg" "mar0kuu2.jpg" "jup0vss1.jpg" "sat0fds1.jpg" "ura0fss1.jpg" "nep0fds1.jpg" "plu0rss1.jpg")

(deftexset "set2" "set2/"
  "sun.jpg"
  ;; http://planetpixelemporium.com/mercury.html
  '("mercurymap.jpg"
    nil
    "mercurybump.jpg")
  ;; http://planetpixelemporium.com/venus.html
  "venusmap.jpg" ;;"ven0mss2.jpg"
  ;; http://planetpixelemporium.com/earth.html
  (list "earthmap1k.jpg"
        nil
        "earthbump1k.jpg")
  ;; http://planetpixelemporium.com/mars.html
  (list "mars_1k_color.jpg"
        "mars_1k_normal.jpg")
  ;; http://planetpixelemporium.com/jupiter.html
  "jupitermap.jpg"
  ;; http://planetpixelemporium.com/saturn.html
  "saturnmap.jpg"
  ;; http://planetpixelemporium.com/uranus.html
  "uranusmap.jpg"
  ;; http://planetpixelemporium.com/neptune.html
  "neptunemap.jpg"
  "plu0rss1.jpg")

(defun load-planet-textures (texture-set)
  (let* ((texture-set (if (stringp texture-set)
                          (gethash texture-set *planet-texture-sets*)
                          texture-set))
         (dir (cadr texture-set)))
    (dolist (gl-planet *gl-planets*)
      (let* ((name (if (eq *gl-pluto* gl-planet)
                       "Pluto"
                       (gl-planet-name gl-planet)))
             (tuple (let ((idx (position name *planet-names* :test #'string=)))
                      (when idx
                        (let ((entry (nth (+ 2 idx) texture-set)))
                          (if (listp entry)
                              entry
                              (list entry)))))))
        (when tuple
          (let ((texture (car tuple))
                (normal-map (cadr tuple))
                (bump-map (caddr tuple)))
            (setf (gl-planet-texture gl-planet) (load-planet-texture texture dir)
                  (gl-planet-normal-texture gl-planet) (if normal-map (load-planet-texture normal-map dir) nil)
                  (gl-planet-bump-texture gl-planet) (if bump-map (load-planet-texture bump-map dir) nil))))))))

(defun init-gl-planets ()
  (setq *gl-planets* nil)
  (add-gl-planet :name "Sun"     :radius  5.00   :pos (v!   0.0000 0 0)   :texture "sun.jpg"       :day 0)
  ;;(add-gl-planet :name "Mercury" :radius  0.38   :pos (v!   5.7910 0 0)   :texture "mer0muu2.jpg"       :day (/ 24 1407.6))
  (add-gl-planet :name "Mercury" :radius  0.38   :pos (v!   5.7910 0 0)   :texture "mercury.jpg"       :day (/ 24 1407.6))
  (add-gl-planet :name "Venus"   :radius  0.95   :pos (v!  10.8200 0 0)   :texture "ven0mss2.jpg"       :day (/ 24 -5832.5))
  (add-gl-planet :name "Earth"   :radius  1.00   :pos (v!  14.9600 0 0)   :texture "ear0xuu2.jpg"       :day (/ 24 23.9))
  (add-gl-planet :name "Mars"    :radius  0.53   :pos (v!  22.7900 0 0)   :texture "mar0kuu2.jpg"       :day (/ 24 24.6))
  (add-gl-planet :name "Jupiter" :radius 11.19   :pos (v!  77.8330 0 0)   :texture "jup0vss1.jpg"       :day (/ 24 9.9))
  (add-gl-planet :name "Saturn"  :radius  9.40   :pos (v! 142.9400 0 0)   :texture "sat0fds1.jpg"       :day (/ 24 10.7))
  (add-gl-planet :name "Uranus"  :radius  4.04   :pos (v! 287.0990 0 0)   :texture "ura0fss1.jpg"       :day (/ 24 -17.2))
  (add-gl-planet :name "Neptune" :radius  3.88   :pos (v! 450.4300 0 0)   :texture "nep0fds1.jpg"       :day (/ 24 16.1))
  #+nil
  (add-gl-planet :name "Pluto"   :radius  0.18   :pos (v! 591.3520 0 0)   :texture "plu0rss1.jpg"       :day (/ 24 -153.3))
  (init-rings)
  (load-planet-textures *planet-texture-set*))

;;Rotation Period (hours)	1407.6	-5832.5	23.9   	24.6	9.9	10.7	-17.2	16.1	-153.3
;; Length of Day (hours)	4222.6	2802.0	24.0   	24.7	9.9	10.7	17.2	16.1	153.3

;; Gl-Planetary Fact Sheet - Metric

;;  	 MERCURY 	 VENUS 	 EARTH 	 MOON 	 MARS 	 JUPITER 	 SATURN 	 URANUS 	 NEPTUNE 	 PLUTO 
;; Mass (1024kg)	0.330	4.87	5.97	0.073	0.642	1898	568	86.8	102	0.0146
;; Diameter (km)	4879	12,104	12,756	3475	6792	142,984	120,536	51,118	49,528	2370
;; Density (kg/m3)	5427	5243	5514	3340	3933	1326	687	1271	1638	2095
;; Gravity (m/s2)	3.7	8.9	9.8	1.6	3.7	23.1	9.0	8.7	11.0	0.7
;; Escape Velocity (km/s)	4.3	10.4	11.2	2.4	5.0	59.5	35.5	21.3	23.5	1.3
;; Rotation Period (hours)	1407.6	-5832.5	23.9	655.7	24.6	9.9	10.7	-17.2	16.1	-153.3
;; Length of Day (hours)	4222.6	2802.0	24.0	708.7	24.7	9.9	10.7	17.2	16.1	153.3
;; Distance from Sun (106 km)	57.9	108.2	149.6	0.384*	227.9	778.6	1433.5	2872.5	4495.1	5906.4
;; Perihelion (106 km)	46.0	107.5	147.1	0.363*	206.6	740.5	1352.6	2741.3	4444.5	4436.8
;; Aphelion (106 km)	69.8	108.9	152.1	0.406*	249.2	816.6	1514.5	3003.6	4545.7	7375.9
;; Orbital Period (days)	88.0	224.7	365.2	27.3	687.0	4331	10,747	30,589	59,800	90,560
;; Orbital Velocity (km/s)	47.4	35.0	29.8	1.0	24.1	13.1	9.7	6.8	5.4	4.7
;; Orbital Inclination (degrees)	7.0	3.4	0.0	5.1	1.9	1.3	2.5	0.8	1.8	17.2
;; Orbital Eccentricity	0.205	0.007	0.017	0.055	0.094	0.049	0.057	0.046	0.011	0.244
;; Obliquity to Orbit (degrees)	0.01	177.4	23.4	6.7	25.2	3.1	26.7	97.8	28.3	122.5
;; Mean Temperature (C)	167	464	15	-20	-65	-110	-140	-195	-200	-225
;; Surface Pressure (bars)	0	92	1	0	0.01	Unknown*	Unknown*	Unknown*	Unknown*	0
;; Number of Moons	0	0	1	0	2	67	62	27	14	5
;; Ring System?	No	No	No	No	No	Yes	Yes	Yes	Yes	No
;; Global Magnetic Field?	Yes	No	Yes	No	No	Yes	Yes	Yes	Yes	Unknown
;;  	 MERCURY 	 VENUS 	 EARTH 	 MOON 	 MARS 	 JUPITER 	 SATURN 	 URANUS 	 NEPTUNE 	 PLUTO 

(defvar *fps-text* "")
(defvar *console-text* "")
(defvar *camera-text* "")
(defvar *date-text* "")

(defvar *console-data* nil)
(defvar *console-index* nil)
(defvar *console-stream* nil)
(defvar *console-texture* nil)

;;- - - - - - - - - - - - - - - - - -

(defun model->world (x)
  (m4:* (m4:translation (box-pos x)) (q:to-mat4 (box-rot x))))

(defun world->clip (c)
  (m4:* (cam->clip c) (world->cam c)))

(defun model->clip (m c)
  (m4:* (world->clip c) (model->world m)))

(defun model->world-norot (x)
  (m4:translation (box-pos x)))
(defun model->clip-norot (m c)
  (m4:* (world->clip c) (model->world-norot m)))

(defun cam-light-model->world (x factor gl-planet)
  (let ((box-pos (box-pos x)))
    (m4:* (m4:* (q:to-mat4 (q:*
                            #.(q:from-axis-angle (v! -1 0 0) (coerce (/ pi 2) 'single-float))
                            (q:from-axis-angle (v! 0 0 -1) (* (gl-planet-day gl-planet)
                                                              (/ *time-acceleration* (* 24 60 60))
                                                              factor))))
                (m4:translation box-pos))
          (q:to-mat4 (box-rot x)))))

(defvar *tangent-function* t)
(defvar *tangent-axis* (v! 0 1 0))

(defun cam-light-model->world-tangent (x factor gl-planet)
  (if *tangent-function*
      (let ((box-pos (box-pos x)))
        (m4:*
         (m4:*
          ;;(q:to-mat4 (q:from-axis-angle (v! -1 0 0) (coerce (/ pi 4) 'single-float)))
          (q:to-mat4 (q:from-axis-angle *tangent-axis* (coerce (/ pi 2) 'single-float)))
          (m4:* (q:to-mat4 (q:*
                            #.(q:from-axis-angle (v! -1 0 0) (coerce (/ pi 2) 'single-float))
                            (q:from-axis-angle (v! 0 0 -1) (* (gl-planet-day gl-planet)
                                                              (/ *time-acceleration* (* 24 60 60))
                                                              factor))))
                (m4:translation box-pos)))
         (q:to-mat4 (box-rot x))))
      (cam-light-model->world x factor gl-planet)))
;;- - - - - - - - - - - - - - - - - -

(defvar *blending-params* (make-blending-params))
(defvar *camera* nil)
(defvar *rotation-factor* 0)
(defvar *follow-camera* nil)

(defun-g box-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values (* model->clip (v! (pos vert) 1))
          (norm vert)
          (tex vert)))

(defun-g box-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
  (v! (s~ (texture tex (* tc 1)) :xyz) fac))

(defpipeline draw-box () (g-> #'box-vert #'box-frag))



;;;;;;;;;;;; console ;;;;;;;;;;;;;;;;;;;
(defun-g console-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values ;;(v! (pos vert) 1)
   (* model->clip (v! (pos vert) 1))
   (norm vert)
   (tex vert)))

(defun-g console-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
  ;;(v! (s~ (texture tex (* tc 1)) :xyz) fac))
  (* fac (v! (s~ (texture tex (* tc 1)) :xyzw))))

(defpipeline draw-console () (g-> #'console-vert #'console-frag))


;;;;;;;;;;;;;; text ;;;;;;;;;;;;;;;;;;;

(defun-g text-vert ((vert g-pnt) &uniform (model->clip :mat4) (size :float))
  (values ;;(v! (pos vert) 1)
   (* model->clip (v! (pos vert) (/ 1 size)))
   (norm vert)
   (tex vert)))

(defun color-matrix (v)
  "Return a 4x4 color matrix"
  (make-array 16 :element-type 'single-float
              :initial-contents (list (coerce (v:x v) 'single-float) 0f0 0f0 0f0
                                      0f0 (coerce (v:y v) 'single-float) 0f0 0f0
                                      0f0 0f0 (coerce (v:z v) 'single-float) 0f0
                                      0f0 0f0 0f0 1f0)))

(defun-g text-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float) (col :mat4))
  (* fac col (v! (s~ (texture tex (* tc 1)) :xyzw))))

(defpipeline draw-text () (g-> #'text-vert #'text-frag))

(defvar *text-data* nil)
(defvar *text-index* nil)
(defvar *text-stream* nil)
(defun init-text ()
  (destructuring-bind (d i) (dendrite.primitives:plain-data :width 0.05 :height 0.05)
    (setf *text-data* (make-gpu-array d :element-type 'g-pnt)
          *text-index* (make-gpu-array i :element-type :ushort)
          *text-stream* (make-buffer-stream *text-data* :index-array *text-index*))))


;;;;;;;;;;; sphere without lighting ;;;;;;;;;;;;;;;;
(defun-g sphere-vert ((vert g-pnt) &uniform (model->clip :mat4) (radius :float))
  (values (* model->clip (v! (pos vert) (/ 1 radius)))
          (norm vert)
          (tex vert)
          (pos vert)))

;; naiive texture mapped sphere
;;(defun-g sphere-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
;;  (v! (s~ (texture tex (* tc 1)) :xyz) fac))

;; prevent artifacts at seam
(defun-g sphere-frag ((norm :vec3) (tc :vec2) (vert :vec3) &uniform (tex :sampler-2d) (fac :float))
  (let ((%tc (v! (/ (+ 3.14159 (atan (v:x vert) (v:z vert))) (* 2 3.14159))
                 (v:y tc))))
    (v! (s~ (texture tex (* %tc 1)) :xyz) fac)
    ;; fix line protruding from bottom at low res
    ;; -> lat/long lines should be a multiple of 36?
    #+nil
    (if (>= (v:y tc) 0.98)
        (v! 0 0 0 0)
        (v! (s~ (texture tex (* %tc 1)) :xyz) fac))))

(defpipeline draw-sphere () (g-> #'sphere-vert #'sphere-frag))


(defvar *lighting-enabled* t)

(defclass light ()
  ((position :initform (v! 0 0 0) :initarg :pos :accessor pos)
   (radius :initform 8.0 :initarg :radius :accessor radius)))

(defvar *light* (make-instance 'light))

(defun-g nm-vert ((data g-pnt)
                  &uniform
                  (model->clip :mat4)
                  (radius :float))
  (values (* model->clip (v! (pos data) (/ 1 radius)))
          (pos data)
          (norm data)
          (v! 0.5 0.5 0.5 0)
          (tex data)))

(defun-g nonm-frag ((model-space-pos :vec3)
                    (vertex-normal :vec3)
                    (diffuse-color :vec4)
                    (tex-coord :vec2)
                    &uniform
                    (model->clip :mat4)
                    (to-tangent :mat4)
                    ;;(bump-dimensions :vec2)
                    (model-space-light-pos :vec3)
                    (light-intensity :vec4)
                    (ambient-intensity :vec4)
                    (tex :sampler-2d)
                    (norm-map :sampler-2d))
  (let* ((light-dir
          (normalize (- model-space-pos
                        model-space-light-pos)))
         (cos-ang-incidence
          (clamp (dot (normalize vertex-normal) light-dir)
                 0.0 1.0))
         (%tex-coord (v! (/ (+ 3.14159 (atan (v:x model-space-pos) (v:z model-space-pos))) (* 2 3.14159))
                         (v:y tex-coord)))
         (t-col (texture tex %tex-coord)))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))
    ;; fix line protruding from bottom at low res
    #+nil
    (if (>= (v:y tex-coord) 0.98)
        (v! 0 0 0 0)
        (+ (* t-col light-intensity cos-ang-incidence)
           (* t-col ambient-intensity)))))

(defun-g nm-frag ((model-space-pos :vec3)
                  (vertex-normal :vec3)
                  (diffuse-color :vec4)
                  (tex-coord :vec2)
                  &uniform
                  (model->clip :mat4)
                  (to-tangent :mat4)
                  ;;(bump-dimensions :vec2)
                  (model-space-light-pos :vec3)
                  (light-intensity :vec4)
                  (ambient-intensity :vec4)
                  (tex :sampler-2d)
                  (norm-map :sampler-2d))
  (let* ((%tex-coord (v! (/ (+ 3.14159 (atan (v:x model-space-pos) (v:z model-space-pos))) (* 2 3.14159))
                         (v:y tex-coord)))
         (full-tangent-norm (- (* (s~ (texture norm-map %tex-coord) :xyzw) 2)
                               (v! 1 1 1 1)))
         (tangent-normal (s~ full-tangent-norm :xyz))
         (binormal (* (cross vertex-normal (s~ full-tangent-norm :xyz)) (v:w full-tangent-norm)))
         (temp (- model-space-pos
                  model-space-light-pos))
         (light-dir
          (normalize (v! (dot temp tangent-normal)
                         (dot temp binormal)
                         (dot temp vertex-normal))))
         (dummy-light-dir
          (* -1 (s~ (* to-tangent (v! vertex-normal 1)) :xyz)))
         (dummy-cos-ang-incidence
          (clamp (dot (normalize tangent-normal) dummy-light-dir)
                 0.0 1.0))
         (cos-ang-incidence
          (clamp (dot (normalize tangent-normal) light-dir)
                 0.0 1.0))
         (t-col (texture tex %tex-coord)))
    (+ (* t-col light-intensity cos-ang-incidence)
           (* t-col ambient-intensity dummy-cos-ang-incidence 0.1)
           (* t-col ambient-intensity))
    ;; fix line protruding from bottom at low res
    #+nil
    (if (>= (v:y tex-coord) 0.98)
        (v! 0 0 0 0)
        (+ (* t-col light-intensity cos-ang-incidence)
           (* t-col ambient-intensity dummy-cos-ang-incidence 0.1)
           (* t-col ambient-intensity)))))

(defun-g bump-frag ((model-space-pos :vec3)
                    (vertex-normal :vec3)
                    (diffuse-color :vec4)
                    (tex-coord :vec2)
                    &uniform
                    (model->clip :mat4)
                    (to-tangent :mat4)
                    ;;(bump-dimensions :vec2)
                    (model-space-light-pos :vec3)
                    (light-intensity :vec4)
                    (ambient-intensity :vec4)
                    (tex :sampler-2d)
                    (norm-map :sampler-2d))
  (let* ((tex-size (texture-size tex 0))
         (bump-dimensions (texture-size norm-map 0))
         (%tex-coord (v! (/ (+ 3.14159 (atan (v:x model-space-pos) (v:z model-space-pos))) (* 2 3.14159))
                         (v:y tex-coord)))
         (normal (normalize vertex-normal))
         (tangent
          (normalize (s~ (* to-tangent (v! normal 1)) :xyz)))
         (binormal (normalize (cross normal tangent)))
         (bumpmap-strength 1.3)
         (bm0 (v:x (texture norm-map %tex-coord)))
         (bmup (v:x (texture norm-map (+ %tex-coord (v! 0 (/ 1.0 (v:y bump-dimensions)))))))
         (bmright (v:x (texture norm-map (+ %tex-coord (v! (/ 1.0 (v:x bump-dimensions)) 0)))))
         (bump-vector (+ (* (- bmright bm0) tangent)
                         (* (- bmup bm0) binormal)))
         (bumped-normal (normalize (+ normal (* bumpmap-strength bump-vector))))
         (full-tangent-norm (- (* (s~ (texture norm-map %tex-coord) :xyzw) 2)
                               (v! 1 1 1 1)))
         (tangent-normal (s~ full-tangent-norm :xyz))
         (temp (- model-space-pos
                  model-space-light-pos))
         (light-dir
          (normalize temp))
         (dummy-light-dir
          (* -1 binormal))
         (dummy-cos-ang-incidence
          (clamp (dot bumped-normal dummy-light-dir)
                 0.0 1.0))
         (cos-ang-incidence
          (clamp (dot bumped-normal light-dir)
                 0.0 1.0))
         (t-col (texture tex %tex-coord)))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity dummy-cos-ang-incidence)
       (* t-col ambient-intensity))
    ;; fix line protruding from bottom at low res
    #+nil
    (if (>= (v:y tex-coord) 0.98)
        (v! 0 0 0 0)
        (+ (* t-col light-intensity cos-ang-incidence)
           (* t-col ambient-intensity dummy-cos-ang-incidence)
           (* t-col ambient-intensity)))))

(defpipeline frag-point-light () (g-> #'nm-vert #'nm-frag))
(defpipeline frag-point-light-bump () (g-> #'nm-vert #'bump-frag))
(defpipeline frag-point-light-nonm () (g-> #'nm-vert #'nonm-frag))

(defvar *normal-mapping-enabled* t)

(defun render-planet (gl-planet factor)
  (let ((box (gl-planet-box gl-planet)))
    (if *lighting-enabled*
        (let* ((norm-map (gl-planet-normal-texture gl-planet))
               (bump-map (gl-planet-bump-texture gl-planet))
               (to-model-space-transformation (cam-light-model->world box factor gl-planet))
               (to-tangent (cam-light-model->world-tangent box factor gl-planet))
               (cam-light-vec (m4:*v to-model-space-transformation
                                     (v! (pos *light*) 1.0))))
          (map-g (cond
                   ((and *normal-mapping-enabled* norm-map)
                    #'frag-point-light)
                   ((and *normal-mapping-enabled* bump-map)
                    #'frag-point-light-bump)
                   (t
                     #'frag-point-light-nonm))
                 (gl-planet-stream gl-planet)
                 :model->clip (model->clip box *camera*)
                 :radius (gl-planet-radius gl-planet)
                 :model-space-light-pos (v:s~ cam-light-vec :xyz)
                 :to-tangent to-tangent
                 :light-intensity (v4:*s (v! 1 1 1 0) 1.5)
                 :ambient-intensity (v! 0.2 0.2 0.2 1.0)
                 :norm-map (if bump-map bump-map norm-map)
                 ;; :bump-dimensions (if bump-map
                 ;;                      (let ((dimensions (slot-value bump-map 'jungl::base-dimensions)))
                 ;;                        (v! (first dimensions) (second dimensions)))
                 ;;                      (v! 0 0))
                 :tex (gl-planet-texture gl-planet)))
        (map-g #'draw-sphere (gl-planet-stream gl-planet)
               :model->clip (model->clip box *camera*)
               :radius (gl-planet-radius gl-planet)
               :fac 1
               :tex (gl-planet-texture gl-planet)))))

(defun calc-glyph-x-size (c glyph-size)
  (let ((x-size (car glyph-size)))
    (if (= 0 x-size)
        (case c
          (#\Space (* 0.25 (/ *base-glyph-array-size* *glyph-array-size*)))
          (t 0))
        ;; fix sizes of various glyphs
        (case c
          (#\w
           (* 0.45 (/ *base-glyph-array-size* *glyph-array-size*)))
          (#\W
           (* 0.60 (/ *base-glyph-array-size* *glyph-array-size*)))
          (#\V
           (* 0.38 (/ *base-glyph-array-size* *glyph-array-size*)))
          (#\Y
           (* 0.40 (/ *base-glyph-array-size* *glyph-array-size*)))
          (#\A
           (* 0.40 (/ *base-glyph-array-size* *glyph-array-size*)))
          (#\O
           (* 0.50 (/ *base-glyph-array-size* *glyph-array-size*)))
          (#\y
           (* 0.32 (/ *base-glyph-array-size* *glyph-array-size*)))
          (#\1
           (* 0.3 (/ *base-glyph-array-size* *glyph-array-size*)))
          (#\.
           (* 0.2 (/ *base-glyph-array-size* *glyph-array-size*)))
          (t
           x-size)))))

(defun calc-text-width (str &optional (size 1.0))
  (let ((pos-x -0.90)
        (pos-y 0.80)
        (text-width 0.0)
        widths)
    (loop for c across str
       do
         (let* ((glyph-texture (get-glyph c))
                (glyph-size (get-glyph-size c))
                (dx (* 0.11 size (calc-glyph-x-size c glyph-size)))
                (new-x (+ pos-x dx))
                (new-y pos-y))
           (declare (ignore glyph-texture))
           (when (or (> new-x 0.98) (char= c #\Newline))
             (push text-width widths)
             (setq text-width 0.0)
             (setf new-x -0.90)
             (decf new-y (* 0.08 size (/ *base-glyph-array-size* *glyph-array-size*))))
           (setf pos-x new-x
                 pos-y new-y)
           (incf text-width dx)))
    (if widths
        (values (apply #'max widths) (nreverse (push text-width widths)))
        (values text-width (list text-width)))))

(defvar *scrolling-help* t)
(defvar *funky* nil)
(defvar *funky-y* -1.0)

(defun render-text (text-string &key (x -0.90) (y 0.80) (color (v! 1 1 1)) (size 1.0) (transparency 0.8))
  (multiple-value-bind (maximum widths)
      (if (keywordp x)
          (calc-text-width text-string)
          (values x (list x)))
    (declare (ignore maximum))
    (let ((alignment (cond ((eq :right x) :right)
                           ((eq :center x) :center)
                           ((eq :left x) :left)
                           (t :left))))
      (labels ((start-x ()
                 (cond
                   ((eq :right alignment)
                    (- 1.00 (let ((max (apply #'max widths)))
                              (pop widths)
                              max)))
                   ((eq :center alignment)
                    (- 0.04 (* 0.5 (or (pop widths) 0.0))))
                   (t ;;(eq :left alignment)
                    (if (numberp x) x -0.90)))))
        (with-blending *blending-params*
          (let ((pos-x (start-x))
                (pos-y y)
                (color (color-matrix color)))
            (loop for c across text-string
               do
                 (let* ((glyph-texture (get-glyph c))
                        (glyph-size (get-glyph-size c))
                        (this-x pos-x)
                        (this-y pos-y)
                        (dx (* (if *funky* 0.13 0.11) size (calc-glyph-x-size c glyph-size)))
                        (new-x (+ pos-x dx))
                        (new-y pos-y))
                   (when (or (> new-x 1.00) ;;(and (not *funky*) (> new-x 0.98))
                             (char= c #\Newline))
                     (setf new-x (start-x))
                     (decf new-y (* (if *funky* 0.10 0.08) size (/ *base-glyph-array-size* *glyph-array-size*))))
                   (setf pos-x new-x
                         pos-y new-y)
                   (unless (char= c #\Newline)
                     (when *funky*
                       (incf this-y *funky-y*))
                     (map-g #'draw-text *text-stream*
                            :model->clip (if *funky*
                                             (m4:*
                                              (let ((box (make-box :pos (v! (v:x (pos *camera*)) (v:y (pos *camera*)) (- (v:z (pos *camera*)) 1)))))
                                                (model->clip box *camera*))
                                              (m4:*
                                               (q:to-mat4 (q:from-axis-angle (v! -1 0 0)
                                                                             (+
                                                                              (coerce (/ pi 4) 'single-float)
                                                                              (tan (* (- this-y -0) 0.0)))))
                                               (m4:translation (v! this-x this-y 0))))
                                             (m4:translation (v! this-x this-y 0)))
                            :col color
                            :fac transparency
                            :size (if *funky*
                                      (* size (+ 0.618
                                                 (* 0.618 (atan (/ (/ 1 (- this-y -1)) 1)))))
                                      size)
                            :tex glyph-texture))))))))))

;; not much faster...
(defun render-text-faster (text-string &key (x -0.90) (y 0.80) (color (v! 1 1 1)) (size 1.0) (transparency 0.8) (funky *funky*))
  (with-blending *blending-params*
    (let ((pos-x x)
          (pos-y y)
          (color (color-matrix color)))
      (loop for c across text-string
         do
           (let* ((glyph-texture (get-glyph c))
                  (glyph-size (get-glyph-size c))
                  (this-x pos-x)
                  (this-y pos-y)
                  (dx (* (if funky 0.13 0.11) size (calc-glyph-x-size c glyph-size)))
                  (new-x (+ pos-x dx))
                  (new-y pos-y))
             (when (or (and (not funky) (> new-x 1.00))
                       (char= c #\Newline))
               (setf new-x x)
               (decf new-y (* (if funky 0.10 0.08) size (/ *base-glyph-array-size* *glyph-array-size*)))
               (setf pos-y new-y))
             (setf pos-x new-x)
             (unless (char= c #\Newline)
               (when funky
                 (incf this-y *funky-y*))
               (map-g #'draw-text *text-stream*
                      :model->clip (if funky
                                       (m4:*
                                        (let ((box (make-box :pos (v! (v:x (pos *camera*)) (v:y (pos *camera*)) (- (v:z (pos *camera*)) 1)))))
                                          (model->clip box *camera*))
                                        (m4:*
                                         #.(q:to-mat4 (q:from-axis-angle (v! -1 0 0) #.(coerce (/ pi 4) 'single-float)))
                                         (m4:translation (v! this-x this-y 0))))
                                       (m4:translation (v! this-x this-y 0)))
                      :col color
                      :fac transparency
                      :size (if funky
                                (* size (+ 0.618 (* 0.618 (atan (/ (/ 1 (- this-y -1)) 1)))))
                                size)
                      :tex glyph-texture
                      )))))))

(defvar *show-help* t)
(defvar *hide-help* nil)

(defvar *rings-data* nil)
(defvar *rings-index* nil)
(defvar *rings-stream* nil)
(defvar *rings-texture* nil)
(defvar *gl-saturn* nil)
(defun init-rings ()
  (let ((saturn (find-gl-planet "Saturn")))
    (setq *gl-saturn* saturn)
    (destructuring-bind (d i) (dendrite.primitives:plain-data)
      (setf *rings-data* (make-gpu-array d :element-type 'g-pnt)
            *rings-index* (make-gpu-array i :element-type :ushort)
            *rings-stream* (make-buffer-stream *rings-data* :index-array *rings-index*)
            *rings-texture* (cepl.devil:load-image-to-texture
                             (merge-pathnames "rings-saturn.png"
                                              (merge-pathnames "set2/" *game-dir*)))))))

(defun-g rings-vert ((vert g-pnt) &uniform (model->clip :mat4) (radius :float))
  (values
   (* model->clip (v! (pos vert) (/ 1 radius)))
   (norm vert)
   (tex vert)
   (pos vert)))

(defun-g rings-frag ((norm :vec3) (tc :vec2) (pos :vec3) &uniform (model->clip :mat4) (model-space-light-pos :vec3) (tex :sampler-2d) (radius :float) (fac :float))
  (let* ((x (v:x pos))
         (y (v:y pos))
         (r (sqrt (+ (* x x) (* y y))))
         (rin 0.55))
    (cond
      ((> r 1)
       (v! 0 0 1 0))
      ((< r rin)
       (v! 0 1 0 0))
      (t
       (let ((%tc (v! (* (/ 1 (- 1 rin)) (- r rin)) 0)))
         (let* ((dir (s~ (normalize pos) :xyz))
                (ldir (s~ (v! (normalize model-space-light-pos) 1) :xyz))
                (incidence (dot dir ldir))
                (angle (acos incidence))
                (dir2
                 (* r (v! (sin angle) (cos angle))))
                (x (v:x dir2))
                (y (abs (v:y dir2)))
                (f fac)
                (shadow (and (< incidence -0.35)
                             (< x 0.5)
                             (< y (* 1 (cos (* 2.9 x)))))))
           (if shadow
               (let ((sf 0.05)
                     (c (v! (s~ (texture tex (* %tc 1)) :xyzw))))
                 (* f (v! (* sf (v:x c)) (* sf (v:y c)) (* sf (v:z c)) (* 10 (v:w c)))))
               (* f (v! (s~ (texture tex (* %tc 1)) :xyzw))))))))))

(defpipeline draw-rings () (g-> #'rings-vert #'rings-frag))

(defun render-rings (gl-planet factor)
  (declare (ignore factor))
  (let* ((box (gl-planet-box gl-planet))
         (cam-light-vec (v3:- (box-pos box) (pos *light*))))
    (map-g #'draw-rings *rings-stream*
           :model->clip (m4:* (model->clip-norot box *camera*)
                              #.(q:to-mat4 (q:from-axis-angle (v! 0 0 1) (coerce (/ pi 1) 'single-float))))
           :model-space-light-pos (v:s~ cam-light-vec :xyz)
           :radius (* 1.3 (gl-planet-radius gl-planet))
           :fac 1
           :tex *rings-texture*)

    (setq cam-light-vec (v! (* -1.0 (v:x cam-light-vec)) (* 1.0 (v:y cam-light-vec)) (* -1.0 (v:z cam-light-vec))))

    (map-g #'draw-rings *rings-stream*
           :model->clip (m4:* (model->clip-norot box *camera*)
                              #.(q:to-mat4 (q:from-axis-angle (v! -1 0 0) (coerce (/ pi 1) 'single-float))))
           :model-space-light-pos (v:s~ cam-light-vec :xyz)
           :radius (* 1.3 (gl-planet-radius gl-planet))
           :fac 1
           :tex *rings-texture*)))

(defun step-gl ()
  (unless *paused*
    (incf *rotation-factor* (* 0.01)))

  (clear)
  (render-sky)

  (with-blending *blending-params*
    (dolist (gl-planet *gl-planets*)
      (let ((box (gl-planet-box gl-planet)))
        (setf (box-rot box)
              (q:* (q:from-axis-angle (v! 0 0 1) (* (gl-planet-day gl-planet) *rotation-factor*
                                                    (/ *time-acceleration* (* 24 60 60))))
                   ;; align with the texture
                   #.(q:from-axis-angle (v! 1 0 0) (coerce (/ pi 2) 'single-float))))
        (render-planet gl-planet *rotation-factor*)))
    (when *gl-saturn*
      (render-rings *gl-saturn* *rotation-factor*)))

  (when *show-console*
    (gl:depth-func :lequal)
    (gl:disable :depth-test)
    (with-blending *blending-params*
      (map-g #'draw-console *console-stream*
             :model->clip (m4:translation (v! 0 0 0))
             :fac 0.5
             :tex *console-texture*))
    (gl:enable :depth-test)
    (gl:depth-func :less))

  (when *console-text*
    (gl:depth-func :lequal)
    (gl:disable :depth-test)

    (render-text *console-text*)
    (render-text *camera-text* :x :right)
    (render-text *date-text* :x :center)
    (when *vessel*
      (render-text (format nil "~A ~A"
                           (if (> *prev-fuel* 0)
                               (if (reverse-thrust *vessel*)
                                   "(((((("
                                   "))))))")
                               "")
                           (slot-value *vessel* 'name))
                   :x :right :y 0.5 :color (v! 0 1 1))
      (if (> (fuel-remaining *vessel*) 0)
          (render-text (format nil "FUEL: ~,1f"
                               (max 0 (fuel-remaining *vessel*)))
                       :x :right :y 0.4 :color (v! 0 1 1))
          (render-text (format nil "NO FUEL")
                       :x :right :y 0.4 :color (v! 1 0 0))))
    (cond
      (*show-help*
       (let ((*funky* *scrolling-help*)
             (transparency 0.95)
             (color (v! (/ #xff 255.0) (/ #xff 255.0) (/ #x66 255.0))))
         (when *funky*
           ;; scroll speed controlled here
           (incf *funky-y* (* 0.005 (fps-multiplier 20)))
           (when (> (v:z (pos *camera*)) 20)
             (decf (v:z (pos *camera*)) (* 0.5 (fps-multiplier 20))))
           (when (> *funky-y* 1.5)
             (setq *scrolling-help* nil
                   *funky* nil
                   *funky-y* -1.0))
           (when *funky*
             (render-text (format nil "PLUTO STRIKES BACK")
                          :x :center
                          :y 0.5
                          :transparency transparency
                          :color color)
             (render-text (format nil "Lonely and no longer considered the ninth planet,~% PLUTO launches a daring plan using F = ma.")
                          ;;:x :left
                          :y 0.3
                          :transparency transparency
                          :color color)))
         (render-text (format nil "CLICK MOUSE BUTTON TO PLAY")
                      :x :center
                      :y 0.0
                      :transparency transparency
                      :color color)
         (render-text-faster (format nil "HOLD MOUSE BUTTON DOWN TO ACCELERATE TOWARDS CURSOR POSITION")
                             :size 0.6
                             :transparency transparency
                             :y -0.1 :color color)
         (render-text-faster #.(format nil
                                       #.(concatenate
                                          'string
                                          "KEYS:      ESC: toggle POV  `: toggle fullscreen  space or p: pause~%"
                                          "    overhead mode:  x: start over   w/s/a/d: move camera   h: toggle help~%"
                                          "    normal mode: ~%"
                                          "              1-5: switch camera    f: follow planets (TAB: switch, g: sun lock, x: view direction)~%"
                                          "~%                   Movement:~%"
                                          "              w: forward   s: backward   a: left   d: right   Hold left shift for speed boost.~%"
                                          "~%                         Simulation:~%"
                                          "              v: toggle VSOP87     q/e: change speed (step)     r: reset~%"
                                          "~%                         Other:~%"
                                          "              l: toggle lighting     z/c: change fov    enter: reset game~%"
                                          "              +/-: change visible size of pluto. Has no effect on collision detection.~%"
                                          ))
                             :size 0.6
                             :x -0.9
                             :y -0.2
                             :transparency transparency
                             :color color))))
    (cond
      (*hit-sun*
       (render-text (format nil "GAME OVER")
                    :x :center :y 0.5 :color (v! 1 0 0)))
      (*hit-other*
       (render-text (format nil "YOU WIN~%Collided with ~A~%with remaining fuel ~,1f"
                            (slot-value *hit-other* 'name)
                            *fuel-remaining*)
                    :x :center :y 0.5 :color (v! 0 1 0)))
      (*colliding*
       (render-text (format nil "Collided with ~A" (slot-value *colliding* 'name))
                    :x :center :y 0.5 :color (v! 0 1 0))))
    (when *paused*
      (render-text "PAUSED" :x :center :y 0.3 :color (v! 1 1 0))
      (render-text "PRESS SPACE TO CONTINUE" :x :center :y 0.2))
    
    (gl:enable :depth-test)
    (gl:depth-func :less))

  (swap))

;;- - - - - - - - - - - - - - - - - -

(defun init-sphere ()
  (destructuring-bind (d i) (dendrite.primitives:sphere-data
                             :lines-of-latitude 72
                             :lines-of-longitude 72)
    (setf *sphere-data* (make-gpu-array d :element-type 'g-pnt)
          *sphere-index* (make-gpu-array i :element-type :ushort)
          *sphere-stream* (make-buffer-stream *sphere-data* :index-array *sphere-index*)))
  (init-gl-planets))

(defun init-console ()
  (destructuring-bind (d i) (dendrite.primitives:plain-data)
    (setf *console-data* (make-gpu-array d :element-type 'g-pnt)
          *console-index* (make-gpu-array i :element-type :ushort)
          *console-stream* (make-buffer-stream *console-data* :index-array *console-index*)
          *console-texture* (cepl.devil:load-image-to-texture
                             (merge-pathnames "console.png" *game-dir*)))))

(defun reload-textures ()
  (setf *console-texture* (cepl.devil:load-image-to-texture
                           (merge-pathnames "console.png" *game-dir*))))

(defun controls-rolling-p ()
  (skitter:mouse-down-p 3))

(defun mouse-pov (event)
  (let ((d (skitter:xy-pos-vec event)))
    (let ((change (v2:*s (v! (- (v:x d) *mouse-x-pos*)
                             (* -1 (- (v:y d) *mouse-y-pos*)))
                         *mouse-sensitivity*)))
      (if (or (> (- (v:x d) *mouse-x-pos*) 100)
              (> (* -1 (- (v:y d) *mouse-y-pos*)) 100))
          (setq change (v! 0 0)))
      (sdl2:warp-mouse-in-window jungl::*gl-window* *mouse-x-pos* *mouse-y-pos*)
      (if (controls-rolling-p)
          ;; roll
          (let* ((up (v3:normalize
                      (v3:- (world-up *camera*)
                            (v3:*s (dir *camera*) (v3:dot (world-up *camera*) (dir *camera*))))))
                 (forward (dir *camera*))
                 (rotx (m4:rotation-from-axis-angle forward (* 1 (v:x change))))
                 (side (v3:cross (dir *camera*) up))
                 (roty (m4:rotation-from-axis-angle side (v:y change))))
            (setf (world-up *camera*) (m4:*v3 rotx (world-up *camera*)))
            (setf (dir *camera*) (m4:*v3 roty (dir *camera*)))
            (setf (world-up *camera*) (m4:*v3 roty (world-up *camera*))))
          ;; normal movement
          (let* ((up (v3:normalize
                      (v3:- (world-up *camera*)
                            (v3:*s (dir *camera*) (v3:dot (world-up *camera*) (dir *camera*))))))
                 (rotx (m4:rotation-from-axis-angle up (* -1 (v:x change))))
                 (side (v3:cross (dir *camera*) up))
                 (roty (m4:rotation-from-axis-angle side (v:y change))))
            (setf (dir *camera*) (m4:*v3 rotx (dir *camera*)))
            (setf (dir *camera*) (m4:*v3 roty (dir *camera*)))
            (setf (world-up *camera*) (m4:*v3 roty (world-up *camera*))))))))

(defun target (mouse-pos)
  ;; pick a point on the x-y plane.
  (let* ((fov (fov *camera*))
         (near (near *camera*))
         (frame-size (frame-size *camera*))
         (width (car frame-size))
         (height (cadr frame-size))
         (view (dir *camera*))
         (up (world-up *camera*))
         (h (v3:normalize (v3:cross view up)))
         (v (v3:normalize (v3:cross h view))) ;; same as up
         (v-length (* (tan (/ fov 2)) near))
         (h-length (* v-length (/ width height)))
         (v (v3:*s v v-length))
         (h (v3:*s h h-length))
         (x (/ (- (v:x mouse-pos) (/ width 2)) (/ width 4)))
         (y (* -1 (/ (- (v:y mouse-pos) (/ height 2)) (/ height 4))))
         (camera-pos (pos *camera*))
         (pos (v3:+ (v3:+ camera-pos (v3:*s view near))
                    (v3:*s h x)
                    (v3:*s v y)))
         (ray (v3:normalize (v3:- pos camera-pos))))
    ;; Ray: P = P0 + t * V
    ;; Plane: P * N + d = 0
    ;; t = -(P0 * N + d) / (V * N)
    ;; P = P0 + t * V
    ;; Z plane:  N = (0, 0, 1)  d = 0
    (let* ((d (* -1 (v:z (relative-position *earth* *sun*)) *gl-scale*))
           (plane-normal (v! 0.0 0.0 1.0))
           (tti (/ (* -1 (+ (v3:dot camera-pos plane-normal) d))
                   (v3:dot ray plane-normal)))
           (p (v3:+ camera-pos (v3:*s ray tti))))
      ;;(format t "P: ~A~%" p)
      (let* ((rp (let ((x (/ (v:x p) *gl-scale*))
                       (y (/ (v:y p) *gl-scale*))
                       (z (/ (v:z p) *gl-scale*)))
                   (v! x y z)))
             (obj-pos (v3:+ rp (slot-value *sun* 'pos))))
        ;;(format t "OBJ-POS: ~A~%" obj-pos)
        (unless *vessel*
          (when (> (v:z (pos *camera*)) 20.0)
            (setf (v:z (pos *camera*)) 20.0)))
        (add-planet obj-pos))
      (values ray tti p))))

(defun %mouse-callback (event timestamp)
  (declare (ignore timestamp))
  (when (and event (not *paused*))
    (when (or (skitter:mouse-down-p 1) (skitter:mouse-down-p 2)) ;; left button is 1 on my system
      (unless *vessel*
        (setq *hit-sun* nil
              *show-help* nil
              *hit-other* nil))
      (if *targeting*
          (let ((d (skitter:xy-pos-vec event)))
            (target d))
          (progn
            (add-planet)
            (follow *vessel*)))))
  (unless *targeting*
    (mouse-pov event)))

(defun mouse-callback (event timestamp)
  (%mouse-callback event timestamp))

(defvar *last-followed* nil)

(defun follow (obj)
  (setq *following* obj)
  (when *following*
    (setq *camera-text* (format nil "Following ~A~A"
                                (slot-value *following* 'name)
                                (if *follow-sun-lock* "-Sun" "")))
    (camera-follow-start *following*)))

(defun unfollow ()
  (when *following*
    (setq *last-followed* *following*)
    (setq *following* nil)
    (setq *camera-text* "")))

(defun camera-change (name &optional (up (v! 0.0 0.0 1.0)))
  (setf (fov *camera*) (coerce (/ pi 3) 'single-float))
  (setf (world-up *camera*) up)
  (unfollow)
  (setq *camera-text* name))

(defun camera-1 ()
  (setf (pos *camera*) (v! 0.0 50.0 0.0)
        (dir *camera*) (v3:normalize (v! 0.0 -1.0 0.0)))
  (camera-change "CAM 1"))

(defun camera-2 ()
  (setf (pos *camera*) (v! 15.0 5.0 0.0))
  (let ((earth (find-gl-planet "Earth")))
    (when earth
      (setf (dir *camera*) (v3:normalize
                            (v3:- (box-pos (gl-planet-box earth))
                                  (pos *camera*))))))
  (camera-change "CAM 2"))

(defun reset-camera ()
  (camera-2))

(defun camera-3 ()
  (setf (pos *camera*) (v! 0.0 0.0 200.0)
        (dir *camera*) (v3:normalize (v! 0.0 0.0 -1.0)))
  (camera-change "CAM 3" (v! 0.0 1.0 0.0)))

(defun camera-4 ()
  (setf (pos *camera*) (v! 0.0 0.0 10.0)
        (dir *camera*) (v3:normalize (v! 0.0 0.0 -1.0)))
  (camera-change "CAM 4" (v! 0.0 1.0 0.0)))

(defun camera-5 ()
  (setf (pos *camera*) (v! 0.0 0.0 20.0)
        (dir *camera*) (v3:normalize (v! 0.0 0.0 -1.0)))
  (camera-change "CAM 5" (v! 0.0 1.0 0.0)))

(defvar *follow-direction* -1.0)

(defun camera-follow (obj)
  (let* ((rel-pos (relative-position obj *sun*))
         (dir (v3:*s (v3:normalize rel-pos) *follow-direction*))
         (radius (slot-value obj 'radius))
         (gl-radius (coerce (* 1520 (if *sun-actual-size* (/ 5.0 109) 1) *gl-scale* radius) 'single-float))
         (off (v3:*s (v3:+ (v! 0.0 0.0 (if (< *follow-direction* 0)
                                           (max (* gl-radius 0.05) 0.5)
                                           0.0))
                           (v3:normalize rel-pos))
                     (* -1 *follow-direction* (max (* gl-radius 1.5)))))
         (pos (slot-value obj 'pos))
         (gl-pos (v3:*s pos *gl-scale*)))
    (when *follow-camera*
      (setq off (v3:+ off (pos *follow-camera*))))
    (when *follow-sun-lock*
      (setf (dir *camera*) dir)
      (setf (world-up *camera*) (v! 0.0 0.0 1.0)))
    (setf (pos *camera*) (v3:+ gl-pos off))))

(defun camera-follow-start (obj)
  (setq *follow-camera* (make-camera))
  (setq *last-followed* obj)
  (let* ((rel-pos (relative-position obj *sun*))
         (dir (v3:*s (v3:normalize rel-pos) *follow-direction*)))
    (setf (dir *camera*) dir)
    (setf (world-up *camera*) (v! 0.0 0.0 1.0))
    (camera-follow obj)))

(defun init-sim ()
  (setq *time-acceleration* (* 24 60 60))
  (setq *epoch-time* (- (get-universal-time) (encode-universal-time 0 0 0 1 1 2000))))

(defun reset-planets ()
  (init-gl-planets)
  (init-planets)
  (setq *vessel* nil))

(defun reset-sim ()
  (setq *following* nil)
  (setq *camera-text* "")
  (setq *use-vsop* *vsop-available*)
  (init-gl-planets)
  (init-planets)
  (setq *vessel* nil)
  (init-sim))

(defun start-targeting ()
  (ignore-errors (stop-sound-thread))
  (start-sound-thread)
  (play-sound :intro)
  (when *paused*
    (pause))
  (setq *hit-sun* nil)
  (setq *hit-other* nil)
  (setq *hide-help* nil)
  (setq *show-help* (not *hide-help*))
  (setq *scrolling-help* t)
  (setq *funky-y* -1.4)

  (reset-sim)
  (setq *show-help* t)
  (setq *show-console* t)
  (setq *targeting* t)
  (camera-5)
  (setf (pos *camera*) (v! 0 0 260))
  (sdl2:show-cursor))

;; called periodically with a null event
(defun %keyboard-callback (event timestamp)
  (declare (ignore timestamp))

  (unless event
    (when *targeting*
      (when *vessel*
        (setf (reverse-thrust *vessel*) nil))
      (when (skitter:mouse-down-p 3) ;; right button
        (when *vessel*
          (unless (or (skitter:mouse-down-p 1) (skitter:mouse-down-p 2)) ;; left button is 1 on my system
            (when (> (fuel-remaining *vessel*) 0)
              (decf (fuel-remaining *vessel*) (fps-multiplier 10))
              (when (<= (fuel-remaining *vessel*) 0)
                (setf (fuel-remaining *vessel*) +0.0))
              (incf (fuel *vessel*) (if (skitter:key-down-p key.lshift) 2 1))))
          (setf (reverse-thrust *vessel*) t)))
      (when (or (skitter:mouse-down-p 1) (skitter:mouse-down-p 2)) ;; left button is 1 on my system
        (when *vessel*
          (when (> (fuel-remaining *vessel*) 0)
            (decf (fuel-remaining *vessel*) (fps-multiplier 10))
            (when (<= (fuel-remaining *vessel*) 0)
              (setf (fuel-remaining *vessel*) +0.0))
            (incf (fuel *vessel*) (if (skitter:key-down-p key.lshift) 2 1)))))))

  ;;(when *vessel* (format t "fuel: ~S~%" (fuel *vessel*)))

  (when event
    (when (skitter:key-down-p key.1)
      (unless *targeting*
        (camera-1)))
    (when (skitter:key-down-p key.2)
      (unless *targeting*
        (camera-2)))
    (when (skitter:key-down-p key.3)
      (unless *targeting*
        (camera-3)))
    (when (skitter:key-down-p key.4)
      (camera-4))
    (when (skitter:key-down-p key.5)
      (camera-5))

    (when (skitter:key-down-p key.return)
      (start-targeting))
    (when (skitter:key-down-p key.space)
      (pause))
    (when (skitter:key-down-p key.p)
      (pause))
    (when (skitter:key-down-p key.r)
      (reset-sim))
    (when (skitter:key-down-p key.v)
      (when *vsop-available*
        (setq *use-vsop* (not *use-vsop*))))

    (when (skitter:key-down-p key.h)
      (when *targeting*
        (setq *hide-help* (not *hide-help*))
        (setq *show-help* (not *hide-help*))))

    (when *gl-pluto*
      (when (or (skitter:key-down-p key.equals) (skitter:key-down-p key.kp_plus))
        (setf (gl-planet-radius *gl-pluto*) (* 1.5 (gl-planet-radius *gl-pluto*))))
      (when (or (skitter:key-down-p key.minus) (skitter:key-down-p key.kp_minus))
        (setf (gl-planet-radius *gl-pluto*) (/ (gl-planet-radius *gl-pluto*) 1.5))))

    (when (skitter:key-down-p key.grave)
      (if *is-fullscreen*
          (progn
            (sdl2:show-cursor)
            (sdl2:set-window-fullscreen jungl::*gl-window* nil)
            (set-original-window-size))
          (progn
            (unless *targeting*
              (sdl2:hide-cursor))
            (set-fullscreen-window-size)
            (sdl2:set-window-fullscreen jungl::*gl-window* :windowed)))
      (setq *is-fullscreen* (not *is-fullscreen*)))

    (when (skitter:key-down-p key.escape)
      (setq *show-console* (not *show-console*))
      (setq *targeting* (not *targeting*))
      (when (and (not *targeting*) *is-fullscreen*)
        (sdl2:hide-cursor))
      (when (and (not *targeting*) *vessel*)
        (follow *vessel*))
      (when *targeting*
        (camera-5)
        (sdl2:show-cursor)))

    (when (skitter:key-down-p key.l)
      (setq *lighting-enabled* (not *lighting-enabled*)))

    #+nil
    (progn
      (when (skitter:key-down-p key.m)
        (setq *normal-mapping-enabled* (not *normal-mapping-enabled*)))
      (when (skitter:key-down-p key.t)
        (setq *tangent-function* (not *tangent-function*))))

    (when (skitter:key-down-p key.q)
      (if (< *time-acceleration* 0)
          (progn
            (setq *time-acceleration* (/ *time-acceleration* *time-acceleration-factor*))
            (when (> *time-acceleration* -0.01)
              (setq *time-acceleration* (* -1 *time-acceleration*))))
          (setq *time-acceleration* (* *time-acceleration* *time-acceleration-factor*))))
    (when (skitter:key-down-p key.e)
      (if (< *time-acceleration* 0)
          (setq *time-acceleration* (* *time-acceleration* *time-acceleration-factor*))
          (progn
            (setq *time-acceleration* (/ *time-acceleration* *time-acceleration-factor*))
            (when (< *time-acceleration* 0.01)
              (setq *time-acceleration* (* -1 *time-acceleration*))))))

    (when (skitter:key-down-p key.tab)
      (unless *targeting*
        (when *following*
          (let* ((prev-p (skitter:key-down-p key.lshift))
                 (next (ignore-errors
                         (nth (funcall (if prev-p #'1- #'1+)
                                       (position *following* *all-objs*))
                              *all-objs*))))
            (when (eq *sun* next)
              (setq next (car (last *all-objs*))))
            (unless next
              (setq next *mercury*))
            (follow next)))))

    (when (skitter:key-down-p key.f)
      (unless *targeting*
        (if *following*
            (unfollow)
            (follow (if (and *last-followed* (find *last-followed* *all-objs*))
                        *last-followed*
                        *earth*))))))

  (when (skitter:key-down-p key.z)
    (incf (fov *camera*) rtg-math.base-maths:+one-degree-in-radians+))
  (when (skitter:key-down-p key.c)
    (decf (fov *camera*) rtg-math.base-maths:+one-degree-in-radians+))

  (when (skitter:key-down-p key.x)
    (when *targeting*
      (reset-planets))
    (if *following*
        (when event
          (setq *follow-direction* (* -1.0 *follow-direction*)))))
  (when (skitter:key-down-p key.g)
    (if *following*
        (when event
          (setq *follow-sun-lock* (not *follow-sun-lock*))
          (setq *camera-text* (format nil "Following ~A~A"
                                      (slot-value *following* 'name)
                                      (if *follow-sun-lock* "-Sun" ""))))))
  (let ((movement-scale
         (* (if (skitter:key-down-p key.lshift) 10 1)
            *movement-scale*))
        (camera (if (and *following* *follow-camera*)
                    *follow-camera*
                    *camera*)))
    (if (and *following* (eq *following* *vessel*))
        (labels ((thrust ()
                   (setf (reverse-thrust *vessel*) nil)
                   (when (> (fuel-remaining *vessel*) 0)
                     (decf (fuel-remaining *vessel*) (fps-multiplier 10))
                     (when (<= (fuel-remaining *vessel*) 0)
                       (setf (fuel-remaining *vessel*) +0.0))
                     (incf (fuel *vessel*) (if (skitter:key-down-p key.lshift) 2 1)))))
          (when (skitter:key-down-p key.w)
            (thrust)
            (add-planet (dir *camera*) t))
          (when (skitter:key-down-p key.s)
            (thrust)
            (add-planet (v3:*s (dir *camera*) -1.0) t))
          (when (skitter:key-down-p key.a)
            (let* ((rotm (m4:rotation-from-axis-angle (world-up *camera*) (* 1 (coerce (/ pi 2) 'single-float))))
                   (left-dir (m4:*v3 rotm (dir *camera*))))
              (thrust)
              (add-planet left-dir t)))
          (when (skitter:key-down-p key.d)
            (let* ((rotm (m4:rotation-from-axis-angle (world-up *camera*) (* -1 (coerce (/ pi 2) 'single-float))))
                   (right-dir (m4:*v3 rotm (dir *camera*))))
              (thrust)
              (add-planet right-dir t))))
        (progn
          (when (skitter:key-down-p key.w)
            (let ((change (v3:*s (dir *camera*)
                                 movement-scale)))
              (setf (pos camera) (v3:+ (pos camera) change))))
          (when (skitter:key-down-p key.s)
            (let ((change (v3:*s (dir *camera*)
                                 (* -1.0 movement-scale))))
              (setf (pos camera) (v3:+ (pos camera) change))))
          (when (skitter:key-down-p key.a)
            (let* ((rotm (m4:rotation-from-axis-angle (world-up *camera*) (* 1 (coerce (/ pi 2) 'single-float))))
                   (left-dir (m4:*v3 rotm (dir *camera*)))
                   (change (v3:*s left-dir movement-scale)))
              (setf (pos camera) (v3:+ (pos camera) change))))
          (when (skitter:key-down-p key.d)
            (let* ((rotm (m4:rotation-from-axis-angle (world-up *camera*) (* -1 (coerce (/ pi 2) 'single-float))))
                   (right-dir (m4:*v3 rotm (dir *camera*)))
                   (change (v3:*s right-dir movement-scale)))
              (setf (pos camera) (v3:+ (pos camera) change)))))))

  #+nil
  (progn
    (format t "dir: ~A~%" (dir *camera*))
    (format t "pos: ~A~%" (pos *camera*))
    (format t "fov: ~A~%" (fov *camera*))
    (force-output)))

(defun keyboard-callback (event timestamp)
  (%keyboard-callback event timestamp))


(let ((fps-frame-start-time (get-internal-real-time))
      (fps-start-time (get-internal-real-time))
      (fps-frames 0)
      (max-fps 40)
      (fps 0)
      (update-interval 0.1))
  (defun fps-limit-init (&optional (fps 40) (interval 0.5))
    (setq max-fps fps
          update-interval interval
          fps-frame-start-time (get-internal-real-time)
          fps-start-time (get-internal-real-time)))
  (defun fps-limit-delay ()
    (let ((elapsed-time (- (get-internal-real-time) fps-frame-start-time))
          (spf (/ internal-time-units-per-second
                  max-fps
                  #+nil (if *paused* 5 max-fps))))
      (when (< elapsed-time spf)
        (sdl2:delay (floor (- spf elapsed-time))))
      (setf fps-frame-start-time (get-internal-real-time))))
  (defun fps-display ()
    (incf fps-frames)
    (let* ((now (get-internal-real-time))
           (elapsed-time (/ (- now fps-start-time) internal-time-units-per-second)))
      (setq fps (/ fps-frames elapsed-time))
      (setq *fps* fps)
      (when (> elapsed-time update-interval)
        (setq *fps-text* (format nil "FPS: ~,1f" fps))
        (setq *console-text*
              (format nil "~A ~A" *fps-text*
                      (format nil "~%step: ~,2f days/s~%VSOP87: ~A~%~A~A"
                              (/ *time-acceleration* (* 24 60 60))
                              (if *use-vsop* "on" "off")
                              "" ;;(if *normal-mapping-enabled* "NM on" "NM off")
                              "" ;;(if *tangent-function* " (tan)" "")
                              )))
        (setf fps-frames 0
              fps-start-time (get-internal-real-time))))))

(defun update-console ()
  #+nil
  (let ((d (distance *earth* *sun*)))
    (* d 0.0000000001))
  (multiple-value-bind (s min h d m y)
      (decode-universal-time
       (+ (encode-universal-time 0 0 0 1 1 2000)
          (round *epoch-time*)))
    (declare (ignore s min h))
    (setq *date-text* (format nil "~D/~2,'0D/~2,'0D" y m d)))
  (fps-display))

(defun %run-loop ()
  (continuable   
    (step-host)
    (update-repl-link)
    (unless *paused*
      (if *was-paused*
          (progn
            (tanstaafl-main-loop-step 0)
            (setq *was-paused* nil))
          (tanstaafl-main-loop-step)))
    (when *following*
      (camera-follow *following*))
    (update-positions)
    (step-gl)
    (%keyboard-callback nil nil)
    (update-console)
    (when (and *gl-pluto* (> (v3:length (box-pos (gl-planet-box *gl-pluto*))) 800))
      ;; too far away
      (when *vessel*
        (setf (slot-value *vessel* 'pos) (v! 0 0 0)))
      (setf (box-pos (gl-planet-box *gl-pluto*)) (v! 0 0 0))
      (setq *colliding* *sun*
            *hit-sun* t))
    (when *colliding*
      (cond
        (*hit-sun*
         (play-sound :hit-sun))
        (*hit-other*
         (play-sound :hit-other)))
      (unfollow)
      (reset-planets))
    (when (or *hit-sun* *hit-other*)
      (setq *scrolling-help* nil)
      (setq *funky-y* -1.0)
      (setq *show-help* t))
    (when *hide-help*
      (setq *show-help* nil))
    (unless *targeting*
      (setq *show-help* nil))
    (fps-limit-delay)))

(defun %window-callback (event timestamp)
  (declare (ignore timestamp))
  (format t "window event: ~S~%" event))

(defun window-callback (event timestamp)
  (%window-callback event timestamp))

(defun %quit-callback (event timestamp)
  (declare (ignore timestamp))
  (format t "quit event: ~S~%" event)
  (stop-loop))

(defun quit-callback (event timestamp)
  (%quit-callback event timestamp))

(let ((running t))
  (defun run-loop ()
    (setq *camera* (make-camera))
    (init-sphere)
    (init-console)
    (init-sky-data)
    (init-text)
    (init-font (merge-pathnames "fonts/FreeSans.ttf" *game-dir*))
    (enable-sky)
    (init-sim)
    (reset-camera)

    ;;(sdl2:hide-cursor)

    (start-targeting)
    (when *is-fullscreen*
      (unless *targeting*
        (sdl2:hide-cursor))
      (set-fullscreen-window-size)
      (sdl2:set-window-fullscreen jungl::*gl-window* :windowed))

    (fps-limit-init)
    
    (skitter:whilst-listening-to ((#'window-callback (skitter:window 0) :size)
                                  (#'quit-callback skitter:+system+ :quitting)
                                  (#'mouse-callback (skitter:mouse 0) :pos)
                                  (#'keyboard-callback (skitter:keyboard 0) :button))
      (loop :while (and running (not (shutting-down-p))) :do
         (%run-loop)))
    (cepl:quit))
  (defun stop-loop ()
    (setf running nil)))


(defun set-fullscreen-window-size ()
  (multiple-value-bind (format w h refresh-rate)
      (sdl2:get-current-display-mode 0)
    (declare (ignore format refresh-rate))
    (format t "width/height: ~S/~S~%" w h)
    (if (not (or (not (numberp w)) (not (numberp h))
                 (<= w 0) (<= h 0)))
        (sdl2:set-window-size jungl::*gl-window* w h))))

(defvar *original-width* 640)
(defvar *original-height* 480)
(defun set-original-window-size ()
  (format t "original width/height: ~S/~S~%" *original-width* *original-height*)
  (sdl2:set-window-size jungl::*gl-window* *original-width* *original-height*))

(defvar *loaded-ft* nil)

(defun start-game (&key game-dir (width 1920 width-p) (height 1080))
  (when game-dir
    (setq *game-dir* game-dir))
  (if *loaded-ft*
      (format t "Good. ft.lisp has already been loaded.~%")
      (load (merge-pathnames "ft.lisp" *game-dir*)))
  (unless width-p
    (sdl2:init :everything)
    (multiple-value-bind (format w h refresh-rate)
        (sdl2:get-current-display-mode 0)
      (declare (ignore format refresh-rate))
      (format t "width/height: ~S/~S~%" w h)
      (if (or (not (numberp w)) (not (numberp h))
              (<= w 0) (<= h 0))
          (setq width 640
                height 480)
          (setq width w
                height h))))
  (setq *original-width* width
        *original-height* height)
  (cepl:repl width height)
  (run-loop))

;; crash if we don't load cl-freetype2 at runtime
;; -> solved by calling make-freetype at runtime. see ft.lisp init-font.
;; 0: (SB-SYS:MEMORY-FAULT-ERROR)
;; 1: ("foreign function: call_into_lisp")
;; 2: ("foreign function: post_signal_tramp")
;; 3: ("foreign function: FT_Stream_New")
;; 4: ("foreign function: #x20015D9BF0")
;; 5: (CEPLER::INIT-FONT "/usr/share/fonts/opentype/ipafont-gothic/ipag.ttf")




(in-package :cepler)

(defvar *is-fullscreen* nil)

(defvar *targeting* nil)
(defvar *following* nil)
(defvar *follow-sun-lock* nil)

(defparameter *time-acceleration-factor* 2)

(defparameter *movement-scale* (* 1000 0.07))
(defparameter *mouse-sensitivity* 0.005)

;; Should be far enough away from the edges of the window
(defparameter *mouse-x-pos* 400)
(defparameter *mouse-y-pos* 300)

(defvar *was-paused* nil)
(defun pause ()
  (setq *overlay-changed* t)
  (when *paused*
    (setq *was-paused* t))
  (setq *paused* (not *paused*)))

(defclass gl-object ()
  ((pos :initform #.(v! 0 0 0)
        :initarg :pos
        :accessor pos)
   (rot :initform #.(q:identity)
        :initarg :rot
        :accessor rot)))

(defclass gl-planet (gl-object)
  ((name :initform ""
         :initarg :name
         :accessor gl-planet-name)
   (radius :initform 1
           :initarg :radius
           :accessor gl-planet-radius)
   (day :initform 1
        :initarg :day
        :accessor gl-planet-day)
   (data :initform nil
         :initarg :data
         :accessor :gl-planet-day)
   (index :initform nil
          :initarg :index
          :accessor gl-planet-index)
   (stream :initform nil
          :initarg :stream
          :accessor gl-planet-stream)
   (texture :initform nil
            :initarg :texture
            :accessor gl-planet-texture)
   (night-texture :initform nil
                  :initarg :night-texture
                  :accessor gl-planet-night-texture)
   (normal-texture :initform nil
                   :initarg :normal-texture
                   :accessor gl-planet-normal-texture)
   (bump-texture :initform nil
                 :initarg :bump-texture
                 :accessor gl-planet-bump-texture)
   (clouds-texture :initform nil
                   :initarg :clouds-texture
                   :accessor gl-planet-clouds-texture)))

(defvar *gl-planets* nil)

(defvar *gl-pluto* nil)

(defvar *sphere-data* nil)
(defvar *sphere-index* nil)
(defvar *sphere-stream* nil)

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
         (make-instance 'gl-planet
                        :name name
                        :radius (* 1000.0 radius) ;;XXX fix cloud clipping issue by making everything bigger
                        :day day
                        :pos pos
                        :data *sphere-data*
                        :index *sphere-index*
                        :stream *sphere-stream*
                        :texture (sample (load-texture texture)))))
    (push gl-planet *gl-planets*)
    gl-planet))

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
  (load-texture-set *texture-set*))

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
;;- - - - - - - - - - - - - - - - - -

(defun model->world (x)
  (m4:* (m4:translation (pos x)) (q:to-mat4 (rot x))))

(defun world->clip (c)
  (m4:* (cam->clip c) (world->cam c)))

(defun model->clip (m c)
  (m4:* (world->clip c) (model->world m)))

(defun pos->clip (pos c)
  (m4:* (world->clip c) (m4:translation pos)))

(defun model->world-norot (x)
  (m4:translation (pos x)))
(defun model->clip-norot (m c)
  (m4:* (world->clip c) (model->world-norot m)))

(defun cam-light-model->world (factor gl-planet)
  (let ((pos (pos gl-planet)))
    (m4:* (m4:* (q:to-mat4 (q:*
                            #.(q:from-axis-angle (v! -1 0 0) (coerce (/ pi 2) 'single-float))
                            (q:from-axis-angle (v! 0 0 -1) (* (gl-planet-day gl-planet)
                                                              (/ *time-acceleration* (* 24 60 60))
                                                              factor))))
                (m4:translation pos))
          (q:to-mat4 (rot gl-planet)))))

(defvar *camera* nil)
(defvar *rotation-factor* 0)
(defvar *follow-camera* nil)

;;;;;;;;;;; sphere without lighting ;;;;;;;;;;;;;;;;
;;; why is this called with a zero length pos?
(defun-g sphere-vert ((vert g-pnt) &uniform (model->clip :mat4) (radius :float))
  (values (if (= (length (pos vert)) 0)
              (v! 0.0 0.0 -1.0 0.0)
              (* model->clip (v! (pos vert) (/ 1 radius))))
          (norm vert)
          (tex vert)
          (pos vert)))

;; naiive texture mapped sphere
;;(defun-g sphere-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
;;  (v! (s~ (texture tex (* tc 1)) :xyz) fac))

;; prevent artifacts at seam
(defun-g sphere-frag ((norm :vec3) (tc :vec2) (vert :vec3) &uniform (tex :sampler-2d) (fac :float))
  (let* ((pos vert)
         (%tc (v! (/ (+ 3.1415927 (atan (v:x pos) (v:z pos))) (* 2 3.1415927))
                  (v:y tc)))
         (texel (texture tex %tc 1)))
    (v! (s~ texel :xyz) (* (v:w texel) fac))))

(def-g-> draw-sphere () #'sphere-vert #'sphere-frag)


(defvar *normal-mapping-enabled* t)
(defvar *lighting-enabled* t)

(defclass light ()
  ((position :initform (v! 0 0 0) :initarg :pos :accessor pos)
   (radius :initform 8000.0 :initarg :radius :accessor radius)))

(defvar *light* (make-instance 'light))

(defun-g nm-vert ((data g-pnt)
                  &uniform
                  (model->clip :mat4)
                  (radius :float))
  (values (if (= (length (pos data)) 0)
              (v! 0.0 0.0 -1.0 0.0)
              (* model->clip (v! (pos data) (/ 1 radius))))
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
                    (model-space-light-pos :vec3)
                    (light-intensity :vec4)
                    (ambient-intensity :vec4)
                    (tex :sampler-2d)
                    (night-tex :sampler-2d)
                    (clouds-tex :sampler-2d)
                    (norm-map :sampler-2d))
  (let* ((light-dir
          (normalize (- model-space-pos
                        model-space-light-pos)))
         (cos-ang-incidence
          (clamp (dot (normalize vertex-normal) light-dir)
                 0.0 1.0))
         (%tex-coord (v! (/ (+ 3.1415927 (atan (v:x model-space-pos) (v:z model-space-pos))) (* 2 3.1415927))
                         (v:y tex-coord)))
         (t-col (texture tex %tex-coord)))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))))

(defun-g nm-frag ((model-space-pos :vec3)
                  (vertex-normal :vec3)
                  (diffuse-color :vec4)
                  (tex-coord :vec2)
                  &uniform
                  (model->clip :mat4)
                  (model-space-light-pos :vec3)
                  (light-intensity :vec4)
                  (ambient-intensity :vec4)
                  (tex :sampler-2d)
                  (night-tex :sampler-2d)
                  (clouds-tex :sampler-2d)
                  (norm-map :sampler-2d))
  (let* ((%tex-coord (v! (/ (+ 3.1415927 (atan (v:x model-space-pos) (v:z model-space-pos))) (* 2 3.1415927))
                         (v:y tex-coord)))
         (full-tangent-norm (- (* (s~ (texture norm-map %tex-coord) :xyzw) 2)
                               (v! 1 1 1 1)))
         (tangent-normal (s~ full-tangent-norm :xyz))
         (binormal (* (cross vertex-normal tangent-normal) (v:w full-tangent-norm)))
         (temp (- model-space-pos
                  model-space-light-pos))
         (light-dir
          (normalize (v! (dot temp tangent-normal)
                         (dot temp binormal)
                         (dot temp vertex-normal))))
         (dummy-light-dir (* binormal -1.0))
         (dummy-cos-ang-incidence
          (clamp (dot (normalize tangent-normal) dummy-light-dir)
                 0.0 1.0))
         (cos-ang-incidence
          (clamp (dot (normalize tangent-normal) light-dir)
                 0.0 1.0))
         (t-col (texture tex %tex-coord)))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity dummy-cos-ang-incidence 0.1)
       (* t-col ambient-intensity))))

(defun-g bump-frag ((model-space-pos :vec3)
                    (vertex-normal :vec3)
                    (diffuse-color :vec4)
                    (tex-coord :vec2)
                    &uniform
                    (model->clip :mat4)
                    (model-space-light-pos :vec3)
                    (light-intensity :vec4)
                    (ambient-intensity :vec4)
                    (tex :sampler-2d)
                    (night-tex :sampler-2d)
                    (clouds-tex :sampler-2d)
                    (norm-map :sampler-2d))
  (let* ((bump-dimensions (texture-size norm-map 0))
         (%tex-coord (v! (/ (+ 3.1415927 (atan (v:x model-space-pos) (v:z model-space-pos))) (* 2 3.1415927))
                         (v:y tex-coord)))
         (temp (- model-space-pos
                  model-space-light-pos))
         (light-dir (normalize temp))
         (normal (normalize vertex-normal))
         (tangent (normalize (cross normal light-dir)))
         (binormal (normalize (cross normal tangent)))
         (bumpmap-strength (* 20.0 (/ (v:x bump-dimensions) 8192.0)))
         (bm0 (v:x (texture norm-map %tex-coord)))
         (bmup (v:x (texture norm-map (+ %tex-coord (v! 0 (/ 1.0 (v:y bump-dimensions)))))))
         (bmright (v:x (texture norm-map (+ %tex-coord (v! (/ 1.0 (v:x bump-dimensions)) 0)))))
         (bump-vector (* 1 (+ (* (- bmright bm0) binormal)
                              (* (- bmup bm0) tangent))))
         (bumped-normal (normalize (+ normal (* bumpmap-strength bump-vector))))
         (dummy-light-dir (* binormal -1.0))
         (dummy-cos-ang-incidence
          (clamp (dot bumped-normal dummy-light-dir)
                 0.0 1.0))
         (cos (dot bumped-normal light-dir))
         (cos-ang-incidence
          (clamp cos
                 0.0 1.0))
         (c-tc (v! (/ (+ 3.1415927 (atan (+ (* 0.013 (v:x light-dir)) (v:x model-space-pos))
                                         (+ (* 0.013 (v:z light-dir)) (v:z model-space-pos)))) (* 2 3.1415927))
                   (v:y tex-coord)))
         (c-col (texture clouds-tex c-tc))
         (t-col (texture tex %tex-coord))
         (n-t-col (texture night-tex %tex-coord))
         (angle (acos cos))
         (steepness 15)
         (lighted
          (* 0.5 (- (tanh (* steepness (+ angle (/ 3.1415927 2)))) (tanh (* steepness (- angle (/ 3.1415927 2))))))
           #+nil
           (* 0.5 (+ 1 (cos (/ (* angle angle) (* 1 3.1415927))))))
         (unlighted
          (- 1 lighted))
         (i (* 8
               (v:x n-t-col)
               (v:y n-t-col)
               (v:z n-t-col)))
         (w (v:w n-t-col))
         (col (+ (* t-col (max 0.5 lighted))
                 (* (v! i i i w)
                    unlighted))))
    (+ (* col light-intensity cos-ang-incidence)
       (* col ambient-intensity dummy-cos-ang-incidence)
       (* col ambient-intensity))))

(defun-g clouds-bump-frag ((model-space-pos :vec3)
                           (vertex-normal :vec3)
                           (diffuse-color :vec4)
                           (tex-coord :vec2)
                           &uniform
                           (model->clip :mat4)
                           (model-space-light-pos :vec3)
                           (light-intensity :vec4)
                           (ambient-intensity :vec4)
                           (tex :sampler-2d)
                           (night-tex :sampler-2d)
                           (clouds-tex :sampler-2d)
                           (norm-map :sampler-2d))
  (let* ((bump-dimensions (texture-size norm-map 0))
         (%tex-coord (v! (/ (+ 3.1415927 (atan (v:x model-space-pos) (v:z model-space-pos))) (* 2 3.1415927))
                         (v:y tex-coord)))
         (temp (- model-space-pos
                  model-space-light-pos))
         (light-dir (normalize temp))
         (normal (normalize vertex-normal))
         (tangent (normalize (cross normal light-dir)))
         (binormal (normalize (cross normal tangent)))
         (bumpmap-strength (* 20.0 (/ (v:x bump-dimensions) 8192.0)))
         (bm0 (v:x (texture norm-map %tex-coord)))
         (bmup (v:x (texture norm-map (+ %tex-coord (v! 0 (/ 1.0 (v:y bump-dimensions)))))))
         (bmright (v:x (texture norm-map (+ %tex-coord (v! (/ 1.0 (v:x bump-dimensions)) 0)))))
         (bump-vector (* 1 (+ (* (- bmright bm0) binormal)
                              (* (- bmup bm0) tangent))))
         (bumped-normal (normalize (+ normal (* bumpmap-strength bump-vector))))
         (dummy-light-dir (* binormal -1.0))
         (dummy-cos-ang-incidence
          (clamp (dot bumped-normal dummy-light-dir)
                 0.0 1.0))
         (cos (dot bumped-normal light-dir))
         (cos-ang-incidence
          (clamp cos
                 0.0 1.0))
         (c-tc (v! (/ (+ 3.1415927 (atan (+ (* 0.013 (v:x light-dir)) (v:x model-space-pos))
                                         (+ (* 0.013 (v:z light-dir)) (v:z model-space-pos)))) (* 2 3.1415927))
                   (v:y tex-coord)))
         (c-col (texture clouds-tex c-tc))
         (t-col (texture tex %tex-coord))
         (n-t-col (texture night-tex %tex-coord))
         (angle (acos cos))
         (steepness 15)
         (lighted
          (* 0.5 (- (tanh (* steepness (+ angle (/ 3.1415927 2)))) (tanh (* steepness (- angle (/ 3.1415927 2))))))
           #+nil
           (* 0.5 (+ 1 (cos (/ (* angle angle) (* 1 3.1415927))))))
         (unlighted
          (- 1 lighted))
         (i (* 7
               (v:x n-t-col)
               (v:y n-t-col)
               (v:z n-t-col)))
         (w (v:w n-t-col))
         (col (+ (* t-col (max 0.4 lighted))
                 (* (+ (v! i i i w)
                       n-t-col)
                    unlighted))))
    ;; cloud shadows
    (cond
      ((> (v:w c-col) 0.0)
       (let ((w (- 1 (v:w c-col))))
         (setf col (v! (* (v:x col) w)
                       (* (v:y col) w)
                       (* (v:z col) w)
                       (v:w col)))))
      (t
       (setf col col)))
    (+ (* col light-intensity cos-ang-incidence)
       (* col ambient-intensity dummy-cos-ang-incidence)
       (* col ambient-intensity))))

(def-g-> frag-point-light () #'nm-vert #'nm-frag)
(def-g-> frag-point-light-bump () #'nm-vert #'bump-frag)
(def-g-> frag-point-light-nonm () #'nm-vert #'nonm-frag)
(def-g-> frag-point-light-clouds-bump () #'nm-vert #'clouds-bump-frag)

(defun render-clouds (gl-planet factor)
  (let* ((clouds-texture (gl-planet-clouds-texture gl-planet))
         (speed 1)
         (radius (* (+ 1.0 (* *gl-scale* 7000.0 1000 0.01 1))
                    (gl-planet-radius gl-planet)))
         (obj (make-instance 'gl-object
                             :pos (pos gl-planet)
                             :rot (rot gl-planet))))
    (setq factor (* speed factor))
    (setf (rot obj) (q:* (q:from-axis-angle #.(v! 0 0 1) (* (gl-planet-day gl-planet) factor
                                                            (/ *time-acceleration* (* 24 60 60))))
                         ;; align with the texture
                         #.(q:from-axis-angle (v! 1 0 0) (coerce (/ pi 2) 'single-float))))
    (if *lighting-enabled*
        (let* ((norm-map nil)
               (bump-map nil)
               (to-model-space-transformation
                (m4:* (m4:* (q:to-mat4 (q:*
                                        #.(q:from-axis-angle (v! -1 0 0) (coerce (/ pi 2) 'single-float))
                                        (q:from-axis-angle #.(v! 0 0 -1) (* (gl-planet-day gl-planet)
                                                                            (/ *time-acceleration* (* 24 60 60))
                                                                            factor))))
                            (m4:translation (pos obj)))
                      (q:to-mat4 (rot obj))))
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
                 :model->clip (model->clip obj *camera*)
                 :radius radius
                 :model-space-light-pos (v:s~ cam-light-vec :xyz)
                 :light-intensity #.(v4:*s (v! 1 1 1 0) 1.5)
                 :ambient-intensity #.(v! 0.1 0.1 0.1 1.0)
                 :norm-map nil
                 :night-tex nil
                 :tex clouds-texture))
        (map-g #'draw-sphere (gl-planet-stream gl-planet)
               :model->clip (model->clip obj *camera*)
               :radius radius
               :fac 1
               :tex clouds-texture))))

(defun render-planet (gl-planet factor)
  (if *lighting-enabled*
      (let* ((norm-map (gl-planet-normal-texture gl-planet))
             (bump-map (gl-planet-bump-texture gl-planet))
             (clouds-map (gl-planet-clouds-texture gl-planet))
             (to-model-space-transformation (cam-light-model->world factor gl-planet))
             (cam-light-vec (m4:*v to-model-space-transformation
                                   (v! (pos *light*) 1.0))))
        (map-g (cond
                 ((and *normal-mapping-enabled* norm-map)
                  #'frag-point-light)
                 ((and *normal-mapping-enabled* bump-map clouds-map)
                  #'frag-point-light-clouds-bump)
                 ((and *normal-mapping-enabled* bump-map)
                  #'frag-point-light-bump)
                 (t
                  #'frag-point-light-nonm))
               (gl-planet-stream gl-planet)
               :model->clip (model->clip gl-planet *camera*)
               :radius (gl-planet-radius gl-planet)
               :model-space-light-pos (v:s~ cam-light-vec :xyz)
               :light-intensity #.(v4:*s (v! 1 1 1 0) 1.5)
               :ambient-intensity #.(v! 0.1 0.1 0.1 1.0)
               :norm-map (if bump-map bump-map norm-map)
               :night-tex (gl-planet-night-texture gl-planet)
               :clouds-tex (gl-planet-clouds-texture gl-planet)
               :tex (gl-planet-texture gl-planet)))
      (map-g #'draw-sphere (gl-planet-stream gl-planet)
             :model->clip (model->clip gl-planet *camera*)
             :radius (gl-planet-radius gl-planet)
             :fac 1
             :tex (gl-planet-texture gl-planet)))
  (when (gl-planet-clouds-texture gl-planet)
    (render-clouds gl-planet factor)))

(defvar *rings-data* nil)
(defvar *rings-index* nil)
(defvar *rings-stream* nil)
(defvar *rings-texture* nil)
(defvar *gl-saturn* nil)
(defun init-rings ()
  (let ((saturn (find-gl-planet "Saturn")))
    (setq *gl-saturn* saturn)
    (unless *rings-data*
      (destructuring-bind (d i) (dendrite.primitives:plain-data)
        (setf *rings-data* (make-gpu-array d :element-type 'g-pnt)
              *rings-index* (make-gpu-array i :element-type :ushort)
              *rings-stream* (make-buffer-stream *rings-data* :index-array *rings-index*)
              *rings-texture* (sample
                               (cepl.devil:load-image-to-texture
                                (merge-pathnames "rings-saturn.png"
                                                 (merge-pathnames "set2/" *game-dir*)))))))))

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

(def-g-> draw-rings () #'rings-vert #'rings-frag)

(defun render-rings (gl-planet factor)
  (declare (ignore factor))
  (let ((cam-light-vec (v3:- (pos gl-planet) (pos *light*))))
    (map-g #'draw-rings *rings-stream*
           :model->clip (m4:* (model->clip-norot gl-planet *camera*)
                              #.(q:to-mat4 (q:from-axis-angle (v! 0 0 1) (coerce (/ pi 1) 'single-float))))
           :model-space-light-pos (v:s~ cam-light-vec :xyz)
           :radius (* 1.3 (gl-planet-radius gl-planet))
           :fac 1
           :tex *rings-texture*)

    (setq cam-light-vec (v! (* -1.0 (v:x cam-light-vec)) (* 1.0 (v:y cam-light-vec)) (* -1.0 (v:z cam-light-vec))))

    (map-g #'draw-rings *rings-stream*
           :model->clip (m4:* (model->clip-norot gl-planet *camera*)
                              #.(q:to-mat4 (q:from-axis-angle (v! -1 0 0) (coerce (/ pi 1) 'single-float))))
           :model-space-light-pos (v:s~ cam-light-vec :xyz)
           :radius (* 1.3 (gl-planet-radius gl-planet))
           :fac 1
           :tex *rings-texture*)))

(defvar *cull-test-fov-cos* nil)

(defun step-gl ()
  ;;(format t "~f~%" *rotation-factor*)
  (unless *paused*
    (let ((new-factor (+ 0.01 *rotation-factor*)))      
      (setq *rotation-factor*
            (if (= *rotation-factor* new-factor)
                0.0
                new-factor))))

  (clear)

  (render-sky)

  ;; to disable backface culling
  ;;(gl:disable :cull-face)

  (with-blending *blending-params*
    (dolist (gl-planet *gl-planets*)
      (setf (rot gl-planet)
            (q:* (q:from-axis-angle (v! 0 0 1) (* (gl-planet-day gl-planet) *rotation-factor*
                                                  (/ *time-acceleration* (* 24 60 60))))
                 ;; align with the texture
                 #.(q:from-axis-angle (v! 1 0 0) (coerce (/ pi 2) 'single-float))))
      (if *cull-test-fov-cos*
          (let ((dot (v3:dot (v3:normalize (v3:- (pos gl-planet) (pos *camera*))) (dir *camera*))))
            (when (>= dot *cull-test-fov-cos*)
              (render-planet gl-planet *rotation-factor*)))
          (render-planet gl-planet *rotation-factor*)))
    (when *gl-saturn*
      (if *cull-test-fov-cos*
          (let ((dot (v3:dot (v3:normalize (v3:- (pos *gl-saturn*) (pos *camera*))) (dir *camera*))))
            (when (>= dot *cull-test-fov-cos*)
              (render-rings *gl-saturn* *rotation-factor*)))
          (render-rings *gl-saturn* *rotation-factor*))))

  (if *use-rtt*
      (when *overlay-changed*
        (render-overlay)
        (let ((res (cepl.viewports:viewport-resolution (cepl.viewports:current-viewport))))
          (gl:bind-framebuffer :framebuffer 0)
          (gl:viewport 0 0 (aref res 0) (aref res 1))))
      (render-overlay))

  (when *use-rtt*
    (gl:depth-func :lequal)
    (gl:disable :depth-test)
    (with-blending *blending-params*
      (map-g #'draw-rttconsole *console-stream*
             :model->clip #.(m4:translation (v! 0 0 0))
             :fac 0.95
             :tex *rtt-texture*)))

  (gl:enable :depth-test)
  (gl:depth-func :less)
  (swap))

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
          *console-texture* (sample
                             (cepl.devil:load-image-to-texture
                              (merge-pathnames "console.png" *game-dir*))))))

(defun controls-rolling-p ()
  (skitter:mouse-down-p 3))

(defun current-gl-window ()
  cepl.internals:*gl-window*)

(defun mouse-pov (event)
  (let ((d (skitter:xy-pos-vec event)))
    (let ((change (v2:*s (v! (- (v:x d) *mouse-x-pos*)
                             (* -1 (- (v:y d) *mouse-y-pos*)))
                         *mouse-sensitivity*)))
      (if (or (> (- (v:x d) *mouse-x-pos*) 100)
              (> (* -1 (- (v:y d) *mouse-y-pos*)) 100))
          (setq change (v! 0 0)))
      (sdl2:warp-mouse-in-window (current-gl-window) *mouse-x-pos* *mouse-y-pos*)
      (if (controls-rolling-p)
          ;; roll
          (let* ((up (world-up *camera*))
                 (forward (dir *camera*))
                 (rotx (m4:rotation-from-axis-angle forward (* 1 (v:x change))))
                 (side (v3:cross (dir *camera*) up))
                 (roty (m4:rotation-from-axis-angle side (v:y change))))
            (setf (world-up *camera*) (m4:*v3 rotx (world-up *camera*)))
            (setf (dir *camera*) (m4:*v3 roty (dir *camera*)))
            (setf (world-up *camera*) (m4:*v3 roty (world-up *camera*))))
          ;; normal movement
          (let* ((up (world-up *camera*))
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
      (let* ((rp (let ((x (/ (v:x p) *gl-scale*))
                       (y (/ (v:y p) *gl-scale*))
                       (z (/ (v:z p) *gl-scale*)))
                   (v! x y z)))
             (obj-pos (v3:+ rp (slot-value *sun* 'pos))))
        (unless *vessel*
          (when (> (v:z (pos *camera*)) 20000.0)
            (setf (v:z (pos *camera*)) 20000.0)))
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
          (unless *vessel*
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
    (text-setf *camera-text* (format nil "Following ~A~A"
                                     (slot-value *following* 'name)
                                     (if *follow-sun-lock* "-Sun" "")))
    (camera-follow-start *following*)))

(defun unfollow ()
  (when *following*
    (setq *last-followed* *following*)
    (setq *following* nil)
    (text-setf *camera-text* "")))

(defun camera-change (name &optional (up (v! 0.0 0.0 1.0)))
  (setf (fov *camera*) (coerce (/ pi 3) 'single-float))
  (setf (world-up *camera*) up)
  (unfollow)
  (text-setf *camera-text* name))

(defun camera-1 ()
  (setf (pos *camera*) (v! 0.0 50000.0 0.0)
        (dir *camera*) (v3:normalize (v! 0.0 -1.0 0.0)))
  (camera-change "CAM 1"))

(defun camera-2 ()
  (setf (pos *camera*) (v! 15000.0 5000.0 0.0))
  (let ((earth (find-gl-planet "Earth")))
    (when earth
      (setf (dir *camera*) (v3:normalize
                            (v3:- (pos earth)
                                  (pos *camera*))))))
  (camera-change "CAM 2"))

(defun reset-camera ()
  (camera-2))

(defun camera-3 ()
  (setf (pos *camera*) (v! 0.0 0.0 200000.0)
        (dir *camera*) (v3:normalize (v! 0.0 0.0 -1.0)))
  (camera-change "CAM 3" (v! 0.0 1.0 0.0)))

(defun camera-4 ()
  (setf (pos *camera*) (v! 0.0 0.0 10000.0)
        (dir *camera*) (v3:normalize (v! 0.0 0.0 -1.0)))
  (camera-change "CAM 4" (v! 0.0 1.0 0.0)))

(defun camera-5 ()
  (setf (pos *camera*) (v! 0.0 0.0 20000.0)
        (dir *camera*) (v3:normalize (v! 0.0 0.0 -1.0)))
  (camera-change "CAM 5" (v! 0.0 1.0 0.0)))

(defvar *follow-direction* -1.0)

(defun camera-follow (obj)
  (let* ((rel-pos (relative-position obj *sun*))
         (dir (if (and (eq *following* *vessel*) (not *follow-sun-lock*))
                  (v3:*s (dir *camera*) *follow-direction*)
                  (v3:*s (v3:normalize rel-pos) *follow-direction*)))
         (radius (slot-value obj 'radius))
         (gl-radius (coerce (* 152.0 (if *sun-actual-size* (/ 5.0 109) 1) *gl-scale* radius) 'single-float))
         (off (v3:*s (v3:+ (v! 0.0 0.0 (+ 2.0 (* (log gl-radius 2) 0.2)))
                           (v3:*s dir (* 5.0)))
                     (* gl-radius 2)))
         (gl-pos (v3:*s rel-pos *gl-scale*)))
    (when *follow-camera*
      (setq off (v3:+ off (pos *follow-camera*))))
    (when *follow-sun-lock*
      (setf (dir *camera*) (v3:*s dir -1.0))
      (setf (world-up *camera*) (v! 0.0 0.0 1.0)))
    (setf (pos *camera*) (v3:+ gl-pos off))))

(defun camera-follow-start (obj)
  (setq *follow-camera* (make-camera))
  (setq *last-followed* obj)
  (let* ((rel-pos (relative-position obj *sun*))
         (dir (v3:*s (v3:normalize rel-pos) *follow-direction*)))
    (setf (dir *camera*) (v3:*s dir -1.0))
    (setf (world-up *camera*) (v! 0.0 0.0 1.0))
    (camera-follow obj)))

(defun init-sim ()
  (setq *time-acceleration* (* 24 60 60))
  (setq *epoch-time* (- (get-universal-time) (encode-universal-time 0 0 0 1 1 2000)))
  (tanstaafl-main-loop-step 0)
  (update-positions))

(defun reset-planets ()
  (init-gl-planets)
  (init-planets)
  (update-positions)
  (setq *vessel* nil
        *gl-pluto* nil))

(defun reset-sim ()
  (setq *following* nil)
  (text-setf *camera-text* "")
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
  (setf (pos *camera*) (v! 0 0 (* 1000 260.0)))
  (setf (far *camera*) 1000000.0)
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
              (setq *overlay-changed* t)
              (decf (fuel-remaining *vessel*) (fps-multiplier 10))
              (when (<= (fuel-remaining *vessel*) 0)
                (setf (fuel-remaining *vessel*) +0.0))
              (incf (fuel *vessel*) (if (skitter:key-down-p key.lshift) 2 1))))
          (setf (reverse-thrust *vessel*) t)))
      (when (or (skitter:mouse-down-p 1) (skitter:mouse-down-p 2)) ;; left button is 1 on my system
        (when *vessel*
          (when (> (fuel-remaining *vessel*) 0)
            (setq *overlay-changed* t)
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
    (when (skitter:key-down-p key.backspace)
      (when (and *vessel* (< (fuel-remaining *vessel*) 0.01))
        (setq *cheated* t)
        (incf (fuel-remaining *vessel*) 200)))
    (when (skitter:key-down-p key.r)
      (let ((following *following*))
        (reset-sim)
        (when following
          (follow following))))
    (when (skitter:key-down-p key.j)
      (let ((following *following*))
        (setq *sun-actual-size* (not *sun-actual-size*))
        (init-gl-planets)
        (when *gl-pluto*
          (setq *gl-pluto* (add-gl-planet :name (gl-planet-name *gl-pluto*)
                                          :radius 0.18
                                          :pos (pos *gl-pluto*)
                                          :texture "plu0rss1.jpg"
                                          :day (gl-planet-day *gl-pluto*))))
        (when following
          (unfollow)
          (follow following))))
    (when (skitter:key-down-p key.v)
      (when *vsop-available*
        (setq *use-vsop* (not *use-vsop*))))

    (when (skitter:key-down-p key.h)
      (when *targeting*
        (setq *overlay-changed* t)
        (setq *scrolling-help* nil)
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
            (sdl2:set-window-fullscreen (current-gl-window) nil)
            (set-original-window-size))
          (progn
            (unless *targeting*
              (sdl2:hide-cursor))
            (set-fullscreen-window-size)
            (sdl2:set-window-fullscreen (current-gl-window) :windowed)))
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

    ;;#+nil
    (progn
      (when (skitter:key-down-p key.t)
        (setq *use-rtt* (not *use-rtt*)))
      (when (skitter:key-down-p key.m)
        (setq *normal-mapping-enabled* (not *normal-mapping-enabled*))))

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
          (text-setf *camera-text* (format nil "Following ~A~A"
                                           (slot-value *following* 'name)
                                           (if *follow-sun-lock* "-Sun" ""))))))
  (let ((movement-scale
         (* (if (skitter:key-down-p key.lshift) 10 1)
            (if *sun-actual-size* #.(/ 5.0 109.0) 1)
            *movement-scale*))
        (camera (if (and *following* *follow-camera*)
                    *follow-camera*
                    *camera*)))
    (if (and *following* (eq *following* *vessel*))
        (labels ((thrust ()
                   (setf (reverse-thrust *vessel*) nil)
                   (when (> (fuel-remaining *vessel*) 0)
                     (setq *overlay-changed* t)
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

(defun %run-loop ()
  (continuable   
    (setq *overlay-changed* *scrolling-help*)
    (step-host)
    (update-repl-link)
    (%keyboard-callback nil nil)
    (unless *paused*
      (if *was-paused*
          (progn
            (tanstaafl-main-loop-step 0)
            (setq *was-paused* nil))
          (tanstaafl-main-loop-step)))
    (when *following*
      (camera-follow *following*))
    (update-positions)
    (update-console)
    (when (and *gl-pluto* (> (v3:length (pos *gl-pluto*)) (* 1000 800)))
      ;; too far away
      (when *vessel*
        (setf (slot-value *vessel* 'pos) (slot-value *sun* 'pos)))
      (setf (pos *gl-pluto*) (v! 0 0 0))
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
    (when (or *hide-help* (not *targeting*))
      (setq *show-help* nil))
    (step-gl)
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
    (setq *cull-test-fov-cos* nil #+nil (cos (* 1.2 (fov *camera*))))
    (when *use-rtt*
      (init-render-to-texture))
    (init-sphere)
    (init-console)
    (init-sky-data)
    (init-text)
    (init-font (merge-pathnames "FreeSans.ttf" (merge-pathnames "fonts/" *game-dir*)))
    (enable-sky)
    (init-sim)
    (reset-camera)

    ;;(sdl2:hide-cursor)

    (start-targeting)
    (when *is-fullscreen*
      (unless *targeting*
        (sdl2:hide-cursor))
      (set-fullscreen-window-size)
      (sdl2:set-window-fullscreen (current-gl-window) :windowed))

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
        (sdl2:set-window-size (current-gl-window) w h))))

(defvar *original-width* 640)
(defvar *original-height* 480)
(defun set-original-window-size ()
  (format t "original width/height: ~S/~S~%" *original-width* *original-height*)
  (sdl2:set-window-size (current-gl-window) *original-width* *original-height*))

(defvar *loaded-ft* nil)

(defun start-game (&key game-dir (width 1920 width-p) (height 1080))
  (when game-dir
    (setq *game-dir* game-dir))
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
  (setq *rtt-width* width
        *rtt-height* height)
  (setq *mouse-x-pos* (floor (/ width 2))
        *mouse-y-pos* (floor (/ height 2)))
  (cepl:repl width height)
  (run-loop))

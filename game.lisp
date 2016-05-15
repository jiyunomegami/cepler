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

(defvar *use-rtt* t)

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

(defvar *overlay-changed* nil)

(defvar *paused* nil)
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

;; calling cepl.devil:load-image-to-texture too many times
;; results in OpenGL signalled (1285 . OUT-OF-MEMORY) from TEX-STORAGE-2D.
(defvar *texture-cache* (make-hash-table :test #'equal))
(defun load-planet-texture (texture &optional (dir "planets/"))
  (or (gethash texture *texture-cache*)
      (setf (gethash texture *texture-cache*)
            (cepl.devil:load-image-to-texture
             (merge-pathnames texture
                              (merge-pathnames dir *game-dir*))))))

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
                        :radius radius
                        :day day
                        :pos pos
                        :data *sphere-data*
                        :index *sphere-index*
                        :stream *sphere-stream*
                        :texture (sample (load-planet-texture texture)))))
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
  #-enable-clouds
  (list "earthmap1k.jpg"
        nil
        "earthbump1k.jpg")
  #+enable-clouds
  (list
   "e-8192.jpg"
   nil
   "eb-8192.jpg"
   "clouds.png")
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
                (bump-map (caddr tuple))
                (clouds-map (fourth tuple)))
            (setf (gl-planet-texture gl-planet) (sample (load-planet-texture texture dir))
                  (gl-planet-normal-texture gl-planet) (when normal-map (sample (load-planet-texture normal-map dir)))
                  (gl-planet-bump-texture gl-planet) (when bump-map (sample (load-planet-texture bump-map dir)))
                  (gl-planet-clouds-texture gl-planet) (when clouds-map (sample (load-planet-texture clouds-map dir))))))))))

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

(defvar *blending-params* (make-blending-params))
(defvar *camera* nil)
(defvar *rotation-factor* 0)
(defvar *follow-camera* nil)

;;;;;;;;;;;; console ;;;;;;;;;;;;;;;;;;;

(defun-g console-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values
   (* model->clip (v! (pos vert) 1))
   (norm vert)
   (tex vert)))

(defun-g console-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
  (* fac (v! (s~ (texture tex (* tc 1)) :xyzw))))

(def-g-> draw-console () #'console-vert #'console-frag)

(defun-g rttconsole-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values
   (* model->clip (v! (pos vert) 1))
   (norm vert)
   (tex vert)))
 
(defun-g rttconsole-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
  (* fac (v! (s~ (texture tex (v! (v:x tc) (- 1 (v:y tc)))) :xyzw))))

(def-g-> draw-rttconsole () #'rttconsole-vert #'rttconsole-frag)


;;;;;;;;;;;;;; text ;;;;;;;;;;;;;;;;;;;

(defun-g text-vert ((vert g-pnt) &uniform (model->clip :mat4) (size :float))
  (values
   (* model->clip (v! (pos vert) (/ 1 size))) 
   (norm vert)
   (tex vert)))

(defun-g normal-text-frag ((norm :vec3) (tc :vec2)
                    &uniform
                    (tex :sampler-2d)
                    (fac :float)
                    (col :vec3))
  (let* ((texel (texture tex tc))
         (w (v:x texel))
         (i (if (> w 0) 1.0 0.0)))
    (cond 
      ((> w 0.0)
       (setq i 1.0))
      (t (setq i 0.0)))
    (v! (* (v:x col) i)
        (* (v:y col) i)
        (* (v:z col) i)
        (* fac w))))

(defun-g rtt-text-frag ((norm :vec3) (tc :vec2)
                        &uniform 
                        (tex :sampler-2d)   
                        (fac :float)
                        (col :vec3))
  (let* ((texel (texture tex tc))
         ;;(i (v:x texel))
         ;;(w (v:w texel)))
         (w (v:x texel))
         (i (if (> w 0) 1.0 0.0)))
    (cond 
      ((> w 0.0)
       (setq i 1.0))
      (t (setq i 0.0)))
    (v! (* (v:x col) i)
        (* (v:y col) i)
        (* (v:z col) i)
        (clamp
         (* fac 1.2 (sqrt w))
         0.0 1.0))))

(def-g-> normal-draw-text () #'text-vert #'normal-text-frag)
(def-g-> rtt-draw-text () #'text-vert #'rtt-text-frag)

(defvar *text-data* nil)
(defvar *text-index* nil)
(defvar *text-stream* nil)
(defun init-text ()
  (destructuring-bind (d i) (dendrite.primitives:plain-data :width 0.05 :height 0.05)
    (setf *text-data* (make-gpu-array d :element-type 'g-pnt)
          *text-index* (make-gpu-array i :element-type :ushort)
          *text-stream* (make-buffer-stream *text-data* :index-array *text-index*))))


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


(defvar *lighting-enabled* t)

(defclass light ()
  ((position :initform (v! 0 0 0) :initarg :pos :accessor pos)
   (radius :initform 8.0 :initarg :radius :accessor radius)))

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
                    (norm-map :sampler-2d))
  (let* ((tex-size (texture-size tex 0))
         (bump-dimensions (texture-size norm-map 0))
         (%tex-coord (v! (/ (+ 3.1415927 (atan (v:x model-space-pos) (v:z model-space-pos))) (* 2 3.1415927))
                         (v:y tex-coord)))
         (temp (- model-space-pos
                  model-space-light-pos))
         (light-dir
          (normalize temp))
         (dir2 (* light-dir -1.0))
         (normal (normalize vertex-normal))
         (tangent (normalize (cross normal dir2)))
         (binormal (normalize (cross normal tangent)))
         (bumpmap-strength (* 20.0 (/ (v:x bump-dimensions) 8192.0)))
         (bm0 (v:x (texture norm-map %tex-coord)))
         (bmup (v:x (texture norm-map (+ %tex-coord (v! 0 (/ 1.0 (v:y bump-dimensions)))))))
         (bmright (v:x (texture norm-map (+ %tex-coord (v! (/ 1.0 (v:x bump-dimensions)) 0)))))
         (bump-vector (+ (* (- bmright bm0) binormal)
                         (* (- bmup bm0) tangent)))
         (bumped-normal (normalize (+ normal (* bumpmap-strength bump-vector))))
         (dummy-light-dir (* binormal -1.0))
         (dummy-cos-ang-incidence
          (clamp (dot bumped-normal dummy-light-dir)
                 0.0 1.0))
         (cos-ang-incidence
          (clamp (dot bumped-normal light-dir)
                 0.0 1.0))
         (t-col (texture tex %tex-coord)))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity dummy-cos-ang-incidence)
       (* t-col ambient-intensity))))

(def-g-> frag-point-light () #'nm-vert #'nm-frag)
(def-g-> frag-point-light-bump () #'nm-vert #'bump-frag)
(def-g-> frag-point-light-nonm () #'nm-vert #'nonm-frag)

(defvar *normal-mapping-enabled* t)

(defun render-clouds (gl-planet factor)
  (let* ((clouds-texture (gl-planet-clouds-texture gl-planet))
         (speed 2.5)
         (radius (* (+ 1.0 (* *gl-scale* 7000.0 1000 2))
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
                 :ambient-intensity #.(v! 0.2 0.2 0.2 1.0)
                 :norm-map nil
                 :tex clouds-texture))
        (map-g #'draw-sphere (gl-planet-stream gl-planet)
               :model->clip (model->clip obj *camera*)
               :radius radius
               :fac 1
               :tex clouds-texture))))

(defun render-planet (gl-planet factor)
  (when (gl-planet-clouds-texture gl-planet)
    (gl:disable :depth-test)
    (gl:enable :cull-face)
    #+nil
    (setf (near *camera*) 100.0))
  (if *lighting-enabled*
      (let* ((norm-map (gl-planet-normal-texture gl-planet))
             (bump-map (gl-planet-bump-texture gl-planet))
             (to-model-space-transformation (cam-light-model->world factor gl-planet))
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
               :model->clip (model->clip gl-planet *camera*)
               :radius (gl-planet-radius gl-planet)
               :model-space-light-pos (v:s~ cam-light-vec :xyz)
               :light-intensity #.(v4:*s (v! 1 1 1 0) 1.5)
               :ambient-intensity #.(v! 0.2 0.2 0.2 1.0)
               :norm-map (if bump-map bump-map norm-map)
               :tex (gl-planet-texture gl-planet)))
      (map-g #'draw-sphere (gl-planet-stream gl-planet)
             :model->clip (model->clip gl-planet *camera*)
             :radius (gl-planet-radius gl-planet)
             :fac 1
             :tex (gl-planet-texture gl-planet)))
  (when (gl-planet-clouds-texture gl-planet)
    (render-clouds gl-planet factor)
    (gl:enable :depth-test)
    #+nil
    (setf (near *camera*) 1.0)))

(defun calc-glyph-x-size (c glyph-size)
  (let ((x-size (car glyph-size)))
    (flet ((glyph-size (x)
             (* x (/ *base-glyph-array-size* *glyph-array-size*))))
      (if (= 0 x-size)
          (case c
            (#\Space (glyph-size 0.25))
            (t 0))
          ;; fix sizes of various glyphs
          (case c
            (#\w (glyph-size 0.45))
            (#\W (glyph-size 0.60))
            (#\V (glyph-size 0.38))
            (#\Y (glyph-size 0.40))
            (#\A (glyph-size 0.40))
            (#\O (glyph-size 0.50))
            (#\y (glyph-size 0.32))
            (#\1 (glyph-size 0.30))
            (#\I (glyph-size 0.15))
            (#\i (glyph-size 0.15))
            (#\. (glyph-size 0.20))
            (#\/ (glyph-size 0.18))
            (t x-size))))))

(defvar *scrolling-help* t)
(defvar *funky* nil)
(defvar *funky-y* -1.0)

(defun calc-text-width (str &key (size 1.0) (nowrap nil))
  (let ((pos-x -0.90)
        (pos-y 0.80)
        (text-width 0.0)
        (x-size (* (if *funky* 0.16 0.11) size))
        widths)
    (loop for c across str
       do
         (let* ((glyph-texture (get-glyph c))
                (glyph-size (get-glyph-size c))
                (dx (* x-size (calc-glyph-x-size c glyph-size)))
                (new-x (+ pos-x dx))
                (new-y pos-y))
           (declare (ignore glyph-texture))
           (when (or (and (> new-x 1.00) (not nowrap))
                     (char= c #\Newline))
             (push text-width widths)
             (setq text-width 0.0
                   new-x -0.90)
             (decf new-y (* 0.08 size (/ *base-glyph-array-size* *glyph-array-size*))))
           (setf pos-x new-x
                 pos-y new-y)
           (incf text-width dx)))
    (if widths
        (values (apply #'max widths) (nreverse (push text-width widths)))
        (values text-width (list text-width)))))

(defun render-text (text-string &key (x -0.90) (y 0.80) (color (v! 1 1 1)) (size 1.0) (transparency 0.8) (nowrap nil))
  (multiple-value-bind (maximum widths)
      (if (keywordp x)
          (calc-text-width text-string :nowrap nowrap)
          (values x (list x)))
    (let ((alignment (cond ((eq :right x) :right)
                           ((eq :center x) :center)
                           ((eq :left x) :left)
                           (t :left))))
      (labels ((start-x ()
                 (cond
                   ((eq :right alignment)
                    (- 1.00 maximum))
                   ((eq :center alignment)
                    (- 0.04 (* 0.5 (or (pop widths) 0.0))))
                   (t ;;(eq :left alignment)
                    (if (numberp x) x -0.90)))))
        (with-blending *blending-params*
          (let* ((pos-x (start-x))
                 (pos-y y)
                 (pos-vec (when *funky* (v! (v:x (pos *camera*)) (v:y (pos *camera*)) (- (v:z (pos *camera*)) 1))))
                 (model->clip (when *funky* (pos->clip pos-vec *camera*))))
            (loop for c across text-string
               do
                 (let* ((glyph-texture (get-glyph c))
                        (glyph-size (get-glyph-size c))
                        (this-x pos-x)
                        (this-y pos-y)
                        (dx (* (if *funky* 0.16 0.11) size (calc-glyph-x-size c glyph-size)))
                        (new-x (+ pos-x dx))
                        (new-y pos-y))
                   (when (or (and (> new-x 1.00) (not nowrap))
                             (char= c #\Newline))
                     (setf new-x (start-x))
                     (decf new-y (* (if *funky* 0.10 0.08) size (/ *base-glyph-array-size* *glyph-array-size*))))
                   (setf pos-x new-x
                         pos-y new-y)
                   (unless (or (char= c #\Newline) (char= c #\Space))
                     (when *funky*
                       (incf this-y *funky-y*))
                     (map-g (if *use-rtt* #'rtt-draw-text #'normal-draw-text) *text-stream*
                            :model->clip (if *funky*
                                             (m4:* model->clip
                                                   (m4:* #.(q:to-mat4 (q:from-axis-angle (v! -1 0 0) (coerce (/ pi 4) 'single-float)))
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
(defun render-text-faster (text-string &key (x -0.90) (y 0.80) (color (v! 1 1 1)) (size 1.0) (transparency 0.8) (funky *funky*) (nowrap nil))
  (with-blending *blending-params*
    (let* ((pos-x x)
           (pos-y y)
           (pos-vec (when funky (v! (v:x (pos *camera*)) (v:y (pos *camera*)) (- (v:z (pos *camera*)) 1))))
           (model->clip (when funky (pos->clip pos-vec *camera*))))
      (loop for c across text-string
         do
           (let* ((glyph-texture (get-glyph c))
                  (glyph-size (get-glyph-size c))
                  (this-x pos-x)
                  (this-y pos-y)
                  (dx (* (if funky 0.13 0.11) size (calc-glyph-x-size c glyph-size)))
                  (new-x (+ pos-x dx))
                  (new-y pos-y))
             (when (or (and (> new-x 1.00) (not nowrap))
                       (char= c #\Newline))
               (setf new-x x)
               (decf new-y (* (if funky 0.10 0.08) size (/ *base-glyph-array-size* *glyph-array-size*)))
               (setf pos-y new-y))
             (setf pos-x new-x)
             (unless (or (char= c #\Newline) (char= c #\Space))
               (when funky
                 (incf this-y *funky-y*))
               (map-g (if *use-rtt* #'rtt-draw-text #'normal-draw-text) *text-stream*
                      :model->clip (if funky
                                       (m4:* model->clip
                                             (m4:* #.(q:to-mat4 (q:from-axis-angle (v! -1 0 0) #.(coerce (/ pi 4) 'single-float)))
                                                   (m4:translation (v! this-x this-y 0))))
                                       (m4:translation (v! this-x this-y 0)))
                      :col color
                      :fac transparency
                      :size (if funky
                                (* size (+ 0.618 (* 0.618 (atan (/ (/ 1 (- this-y -1)) 1)))))
                                size)
                      :tex glyph-texture)))))))

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
            *rings-texture* (sample
                             (cepl.devil:load-image-to-texture
                              (merge-pathnames "rings-saturn.png"
                                               (merge-pathnames "set2/" *game-dir*))))))))

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

(defvar *rtt-framebuffer* nil)
(defvar *rtt-texture* nil)
(defvar *rtt-width* 1920)
(defvar *rtt-height* 1080)

(defun init-render-to-texture ()
  (let* ((framebuffer (first (gl:gen-framebuffers 1)))
         (cepl-texture (cepl.textures:make-texture
                        nil
                        :dimensions (list *rtt-width* *rtt-height*)
                        :element-type :rgba8))
         (gl-texture (cepl.textures::texture-id cepl-texture)))
    (gl:bind-framebuffer :framebuffer framebuffer)
    (gl:bind-texture :texture-2d gl-texture)
    (gl:framebuffer-texture-2d :framebuffer
                               :color-attachment0
                               :texture-2d
                               gl-texture
                               0)
    (gl:draw-buffers (list :color-attachment0))

    ;; validate framebuffer
    (let ((framebuffer-status (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= framebuffer-status :framebuffer-complete)
        (error "Framebuffer status: ~A." framebuffer-status)))

    (setq *rtt-framebuffer* framebuffer
          *rtt-texture* (sample cepl-texture))))

(defmacro text-setf (&rest rest)
  `(progn
     (setq *overlay-changed* t)
     (setf ,@rest)))

(defun render-overlay ()
  (when *use-rtt*
    (gl:bind-framebuffer :framebuffer *rtt-framebuffer*)
    (gl:viewport 0 0 *rtt-width* *rtt-height*)
    (gl:clear :color-buffer #+nil :depth-buffer))
  (gl:depth-func :lequal)
  (gl:disable :depth-test)
  (when *show-console*
    (with-blending *blending-params*
      (map-g #'draw-console *console-stream*
             :model->clip #.(m4:translation (v! 0 0 0))
             :fac (if *use-rtt* (/ 0.8 0.95) 0.8)
             :tex *console-texture*)))
  (when *console-text*
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
             (render-text "PLUTO STRIKES BACK"
                          :x :center
                          :y 0.5
                          :transparency transparency
                          :color color)
             (render-text #.(format nil "Lonely and no longer considered the ninth planet,~% PLUTO launches a daring plan using F = ma.")
                          ;;:x :left
                          :y 0.3
                          :transparency transparency
                          :nowrap t
                          :color color)))
         (render-text "CLICK MOUSE BUTTON TO PLAY"
                      :x :center
                      :y 0.0
                      :transparency transparency
                      :nowrap t
                      :color color)
         (render-text-faster "HOLD MOUSE BUTTON DOWN TO ACCELERATE TOWARDS CURSOR POSITION"
                             :size 0.6
                             :y -0.1
                             :nowrap t
                             :transparency transparency
                             :color color)
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
                                          "              v: toggle VSOP87    q/e: change speed (step)    r: reset   j: sun actual size toggle~%"
                                          "~%                         Other:~%"
                                          "              l: toggle lighting     z/c: change fov    enter: reset game~%"
                                          "              +/-: change visible size of pluto. Has no effect on collision detection.~%"
                                          ))
                             :size 0.6
                             :x -0.9
                             :y -0.2
                             :transparency transparency
                             :nowrap t
                             :color color))))
    (cond
      (*hit-sun*
       (render-text "GAME OVER"
                    :x :center :y 0.5 :color (v! 1 0 0)))
      (*hit-other*
       (render-text (format nil "YOU WIN~%Collided with ~A"
                            (slot-value *hit-other* 'name))
                    :x :center :y 0.5 :color (v! 0 1 0))
       (render-text (format nil "~%~%with remaining fuel ~,1f"
                            *fuel-remaining*)
                    :x :center :y 0.5 :color (if *cheated* (v! 0.5 0.5 0.5) (v! 0 1 0))))
      (*colliding*
       (render-text (format nil "Collided with ~A" (slot-value *colliding* 'name))
                    :x :center :y 0.5 :color (v! 0 1 0))))
    (when *paused*
      (render-text "PAUSED" :x :center :y 0.3 :color (v! 1 1 0))
      (render-text "PRESS SPACE TO CONTINUE" :x :center :y 0.2))))

(defvar *cull-test-fov-cos* nil)

(defun step-gl ()
  (unless *paused*
    (incf *rotation-factor* (* 0.01)))

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
  (setf (pos *camera*) (v! 0.0 50.0 0.0)
        (dir *camera*) (v3:normalize (v! 0.0 -1.0 0.0)))
  (camera-change "CAM 1"))

(defun camera-2 ()
  (setf (pos *camera*) (v! 15.0 5.0 0.0))
  (let ((earth (find-gl-planet "Earth")))
    (when earth
      (setf (dir *camera*) (v3:normalize
                            (v3:- (pos earth)
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
         (dir (if (and (eq *following* *vessel*) (not *follow-sun-lock*))
                  (v3:*s (dir *camera*) *follow-direction*)
                  (v3:*s (v3:normalize rel-pos) *follow-direction*)))
         (radius (slot-value obj 'radius))
         (gl-radius (coerce (* 1520 (if *sun-actual-size* (/ 5.0 109) 1) *gl-scale* radius) 'single-float))
         (off (v3:*s (v3:+ (v! 0.0 0.0 (* 1 (max (* gl-radius 0.05) 0.5)))
                           (v3:*s dir 1.0))
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
  (setq *epoch-time* (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 2000)))
  (tanstaafl-main-loop-step 0))

(defun reset-planets ()
  (init-gl-planets)
  (init-planets)
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
                  max-fps)))
      (when (< elapsed-time spf)
        (sdl2:delay (floor (- spf elapsed-time))))
      (setf fps-frame-start-time (get-internal-real-time))))
  (defun fps-display ()
    (incf fps-frames)
    (let* ((now (get-internal-real-time))
           (elapsed-time (/ (- now fps-start-time) internal-time-units-per-second)))
      (setq fps (if (= 0 elapsed-time) 1 (/ fps-frames elapsed-time)))
      (setq *fps* fps)
      (when (> elapsed-time update-interval)
        (text-setf *fps-text* (format nil "FPS: ~,1f" fps))
        (text-setf *console-text*
                   (format nil "~A ~A" *fps-text*
                           (format nil "~%step: ~,2f days/s~%VSOP87: ~A~%"
                                   (/ *time-acceleration* (* 24 60 60))
                                   (if *use-vsop* "on" "off")
                                   ;;(if *normal-mapping-enabled* "NM on" "NM off")
                                   )))
        (setf fps-frames 0
              fps-start-time (get-internal-real-time))))))

(defvar *prev-date-text* nil)
(defun update-console ()
  (multiple-value-bind (s min h d m y)
      (decode-universal-time
       (+ #.(encode-universal-time 0 0 0 1 1 2000)
          (round *epoch-time*)))
    (declare (ignore s min h))
    (let ((date-text (format nil "~D/~2,'0D/~2,'0D" y m d)))
      (when (or (null *prev-date-text*)
                (not (string= *prev-date-text* date-text)))
        (text-setf *date-text* (setq *prev-date-text* date-text)))))
  (fps-display))

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
    (when (and *gl-pluto* (> (v3:length (pos *gl-pluto*)) 800))
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
  (setq *rtt-width* width
        *rtt-height* height)
  (setq *mouse-x-pos* (floor (/ width 2))
        *mouse-y-pos* (floor (/ height 2)))
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




(in-package :cepler)

;; calling cepl.devil:load-image-to-texture too many times
;; results in OpenGL signalled (1285 . OUT-OF-MEMORY) from TEX-STORAGE-2D.
(defvar *texture-cache* (make-hash-table :test #'eq))
(defun load-texture (texture &optional (dir "planets/"))
  (or (gethash texture *texture-cache*)
      (setf (gethash texture *texture-cache*)
            (cepl.devil:load-image-to-texture
             (merge-pathnames texture
                              (merge-pathnames dir *game-dir*))))))

(defvar *planet-names* '("Sun" "Mercury" "Venus" "Earth" "Mars" "Jupiter" "Saturn" "Uranus" "Neptune" "Pluto"))
(defvar *texture-sets* (make-hash-table :test #'equalp))
(defvar *texture-set* (if (directory "set3") "set3" "set2"))

(defmacro deftexset (name dir &rest rest)
  (let ((varname (read-from-string (format nil "*texture-set-~A*" name)))
        (supersets (when (listp (car rest)) (car rest))))
    (when supersets
      (setq rest (cdr rest)))
    `(progn
       (defvar ,varname)
       (setq ,varname
             (list ,name ,dir (list ,@supersets)
                   (list ,@(loop for (key value) on rest
                              when (keywordp key)
                              collect (list 'cons key value)))))
       (setf (gethash ,name *texture-sets*) ,varname))))

;; SOLAR SYSTEM SIMULATOR: http://maps.jpl.nasa.gov/
;; Mercury: http://laps.noaa.gov/albers/sos/sos.html (mercury_rgb_cyl_www.jpg)
(deftexset "JPL" "planets/"
  :sun "sun.jpg"
  :mercury "mercury.jpg"
  :venus "ven0mss2.jpg"
  :earth "ear0xuu2.jpg"
  :mars "mar0kuu2.jpg"
  :jupiter "jup0vss1.jpg"
  :saturn "sat0fds1.jpg"
  :uranus "ura0fss1.jpg"
  :neptune "nep0fds1.jpg"
  :pluto "plu0rss1.jpg")

(deftexset "set2" "set2/"
  :sun "sun.jpg"
  ;; http://planetpixelemporium.com/mercury.html
  :mercury '("mercurymap.jpg"
             nil
             "mercurybump.jpg")
  ;; http://planetpixelemporium.com/venus.html
  :venus "venusmap.jpg" ;;"ven0mss2.jpg"
  ;; http://planetpixelemporium.com/earth.html
  :earth (list "earthmap1k.jpg"
               nil
               "earthbump1k.jpg")
  ;; http://planetpixelemporium.com/mars.html
  :mars (list "mars_1k_color.jpg"
              "mars_1k_normal.jpg")
  ;; http://planetpixelemporium.com/jupiter.html
  :jupiter "jupitermap.jpg"
  ;; http://planetpixelemporium.com/saturn.html
  :saturn "saturnmap.jpg"
  ;; http://planetpixelemporium.com/uranus.html
  :uranus "uranusmap.jpg"
  ;; http://planetpixelemporium.com/neptune.html
  :neptune "neptunemap.jpg"
  :pluto "plu0rss1.jpg")

(deftexset "set3" "set3/"
  ("set2")
  :earth (list
          "e-8192.jpg"
          nil
          "eb-8192.jpg"
          "clouds.png"
          "e-night.jpg"))

(defun load-texture-set (texture-set)
  (let* ((texture-set (if (stringp texture-set)
                          (gethash texture-set *texture-sets*)
                          texture-set))
         (dir (second texture-set))
         (supersets (third texture-set))
         (textures (fourth texture-set)))
    (map nil #'load-texture-set supersets)
    (dolist (gl-planet *gl-planets*)
      (let* ((name (if (eq *gl-pluto* gl-planet)
                       "Pluto"
                       (gl-planet-name gl-planet)))
             (key (intern (string-upcase name) :keyword))
             (tuple (let ((entry (cdr (assoc key textures))))
                      (if (listp entry)
                          entry
                          (list entry)))))
        (when tuple
          (let ((texture (car tuple))
                (night-texture (fifth tuple))
                (normal-map (cadr tuple))
                (bump-map (caddr tuple))
                (clouds-map (fourth tuple)))
            (setf (gl-planet-texture gl-planet) (sample (load-texture texture dir))
                  (gl-planet-night-texture gl-planet) (when night-texture (sample (load-texture night-texture dir)))
                  (gl-planet-normal-texture gl-planet) (when normal-map (sample (load-texture normal-map dir)))
                  (gl-planet-bump-texture gl-planet) (when bump-map (sample (load-texture bump-map dir)))
                  (gl-planet-clouds-texture gl-planet) (when clouds-map (sample (load-texture clouds-map dir))))))))))


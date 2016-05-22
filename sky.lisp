;;; from lark (https://github.com/cbaggers/lark)
(in-package :cepler)

(defvar *game-dir*
  (asdf:system-relative-pathname :cepler ""))

(defvar *camera* nil)
(defvar *skybox-camera* nil)
(defvar *sky-enabled* nil)
(defvar *skybox-stream* nil)
(defvar *sky-cube-texture* nil)

(defun enable-sky ()
  (setf *sky-enabled* t))

(defun disable-sky ()
  (setf *sky-enabled* nil))

(defun-g sky-vert ((vert :vec3) &uniform (to-cam-space :mat4))
  (let ((pos (* to-cam-space (v! vert 1.0))))
    (values (s~ pos :xyww)
	    vert)))

(defun-g sky-frag ((tc :vec3) &uniform (tex :sampler-cube))
  (texture tex tc))

(def-g-> draw-sky () #'sky-vert #'sky-frag)

(defun render-sky ()
  (when *sky-enabled*
    (gl:depth-func :lequal)
    (map-g #'draw-sky *skybox-stream*
           :tex *sky-cube-texture*
           :to-cam-space (m4:* (cam->clip *skybox-camera*) (world->cam *skybox-camera*)))
    (gl:depth-func :less)))

(defun make-cubemap-tex (&rest paths)
  (with-c-arrays (ca (mapcar (lambda (p)
                               (cepl.devil:load-image-to-c-array
                                (merge-pathnames p *game-dir*)))
                             paths))
    (sample (make-texture ca :element-type :rgb8 :cubes t))))

(defun init-sky-data ()
  (setq *skybox-camera* (make-camera))
  (let* ((bx (dendrite.primitives:cube-data
	      :size #.(* 1000 10 2010s0) :normals nil :tex-coords nil))
         (data (make-gpu-array (first bx) :element-type :vec3))
         (ind (make-gpu-array
	       (dendrite.primitives:swap-winding-order (second bx))
	       :element-type :ushort)))
    (setf *skybox-stream*
	  (make-buffer-stream data :index-array ind :retain-arrays t)))
  (setf *sky-cube-texture*
        (make-cubemap-tex
         "./stars/TychoSkymapII.t3_08192x04096_80_mx.jpg" ;; left
         "./stars/TychoSkymapII.t3_08192x04096_80_px.jpg" ;; right
         "./stars/vhflipped_my.jpg" ;; up
         "./stars/vhflipped_py.jpg" ;; down
         "./stars/TychoSkymapII.t3_08192x04096_80_mz.jpg" ;; front
         "./stars/TychoSkymapII.t3_08192x04096_80_pz.jpg" ;; back
         )))


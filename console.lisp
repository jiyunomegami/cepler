(in-package :cepler)

(defvar *blending-params* (make-blending-params))

(defvar *normal-mapping-enabled* t)
(defvar *lighting-enabled* t)

(defvar *paused* nil)
(defvar *show-console* nil)

(defvar *use-rtt* t)
(defvar *overlay-changed* nil)

(defvar *show-help* t)
(defvar *hide-help* nil)
(defvar *scrolling-help* t)
(defvar *funky* nil)
(defvar *funky-y* -1.0)

(defvar *fps-text* "")
(defvar *console-text* "")
(defvar *camera-text* "")
(defvar *date-text* "")

(defvar *console-data* nil)
(defvar *console-index* nil)
(defvar *console-stream* nil)
(defvar *console-texture* nil)

(defvar *rtt-framebuffer* nil)
(defvar *rtt-texture* nil)
(defvar *rtt-width* 1920)
(defvar *rtt-height* 1080)

;;;;;;;;;;;; console ;;;;;;;;;;;;;;;;;;;

(defun-g console-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values
   (* model->clip (v! (pos vert) 1))
   (norm vert)
   (tex vert)))

(defun-g console-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
  (* fac (v! (s~ (texture tex (* tc 1)) :xyzw))))

(def-g-> draw-console () (console-vert g-pnt) (console-frag :vec3 :vec2))

(defun-g rttconsole-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values
   (* model->clip (v! (pos vert) 1))
   (norm vert)
   (tex vert)))

(defun-g rttconsole-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
  (* fac (v! (s~ (texture tex (v! (v:x tc) (- 1 (v:y tc)))) :xyzw))))

(def-g-> draw-rttconsole () (rttconsole-vert g-pnt) (rttconsole-frag :vec3 :vec2))


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

(def-g-> normal-draw-text () (text-vert g-pnt) (normal-text-frag :vec3 :vec2))
(def-g-> rtt-draw-text () (text-vert g-pnt) (rtt-text-frag :vec3 :vec2))

(defvar *text-data* nil)
(defvar *text-index* nil)
(defvar *text-stream* nil)
(defun init-text ()
  (destructuring-bind (d i) (dendrite.primitives:plain-data :width 0.05 :height 0.05)
    (setf *text-data* (make-gpu-array d :element-type 'g-pnt)
          *text-index* (make-gpu-array i :element-type :ushort)
          *text-stream* (make-buffer-stream *text-data* :index-array *text-index*))))

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
        (format t" framebuffer status ~A is not :framebuffer-complete" framebuffer-status)
        (setq *use-rtt* nil)))

    (setq *rtt-framebuffer* framebuffer
          *rtt-texture* (sample cepl-texture))))

(defmacro text-setf (&rest rest)
  `(progn
     (setq *overlay-changed* t)
     (setf ,@rest)))

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
            (#\M (glyph-size 0.55))
            (#\V (glyph-size 0.38))
            (#\Y (glyph-size 0.40))
            (#\A (glyph-size 0.42))
            (#\O (glyph-size 0.50))
            (#\y (glyph-size 0.32))
            (#\1 (glyph-size 0.30))
            (#\I (glyph-size 0.15))
            (#\i (glyph-size 0.15))
            (#\. (glyph-size 0.20))
            (#\/ (glyph-size 0.18))
            (t x-size))))))

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

(defmacro drawing-settings (&body body)
  `(let ((%dsy 0.65)
         (%dsx -0.92)
         (%size 0.6))
     ,@body))

(defmacro draw-setting (var char)
  (let ((str (princ-to-string char)))
    `(progn
       (if ,var
           (render-text ,str :size %size :y %dsy :x %dsx :color (v! 0 1 0))
           (render-text ,str :size %size :y %dsy :x %dsx :color (v! 0.6 0.6 0.6)))
     (incf %dsx (calc-text-width ,str :size %size)))))

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
    (drawing-settings
      (draw-setting *use-vsop* "V")
      (draw-setting *use-elp* "E")
      (draw-setting *draw-to-scale* "S")
      (draw-setting *sun-actual-size* "A")
      (draw-setting *lighting-enabled* "L")
      (draw-setting *normal-mapping-enabled* "M")
      (draw-setting *use-rtt* "T"))
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
           (let ((multiplier (fps-multiplier 20)))
             (incf *funky-y* (* 0.005 multiplier))
             (when (> (v:z (pos *camera*)) (* 20 1000))
               (decf (v:z (pos *camera*)) (* (* 0.5 1000) multiplier))))
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
         (render-text "HOLD MOUSE BUTTON DOWN TO ACCELERATE TOWARDS CURSOR POSITION"
                      :size 0.6
                      :y -0.1
                      :nowrap t
                      :transparency transparency
                      :color color)
         (render-text #.(format nil
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

(defvar *prev-date-text* nil)
(defun update-console ()
  (multiple-value-bind (s min h d m y)
      (decode-universal-time
       (+ (encode-universal-time 0 0 0 1 1 2000)
          (round *epoch-time*)))
    (declare (ignore s min h))
    (let ((date-text (format nil "~D/~2,'0D/~2,'0D" y m d)))
      (when (or (null *prev-date-text*)
                (not (string= *prev-date-text* date-text)))
        (text-setf *date-text* (setq *prev-date-text* date-text)))))
  (fps-display))

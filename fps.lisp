(in-package :cepler)

(defvar *fps* 0)
(defun fps-multiplier (n)
  (if (> *fps* 0)
      (/ n *fps*)
      n))

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

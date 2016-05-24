(in-package :cepler)

(defvar *face* nil)
(defvar *glyphs* (make-hash-table :test #'eql))
(defvar *glyph-sizes* (make-hash-table :test #'eql))
(defparameter *base-glyph-array-size* 128)
(defvar *glyph-array-size* *base-glyph-array-size*)

(defun set-font-size (size)
  (let ((array-size (round (/ *base-glyph-array-size* size))))
    (when (< array-size 24)
      (setq array-size 24))
    (when (> array-size 256)
      (setq array-size 256))
    (setq *glyph-array-size* array-size))
  ;;(freetype2:set-char-size *face* (* 24 64) 0 72 72)
  (freetype2:set-char-size *face* (* 24 64 4) 0 72 72)
  (setq *glyphs* (make-hash-table :test #'equal)))

(defvar *loaded-freetype-library* nil)

(defun init-font (&optional
                    (font-file "/usr/share/fonts/opentype/ipafont-gothic/ipag.ttf"))
  (unless *loaded-freetype-library*
    (setf freetype2:*library* (freetype2:make-freetype))
    (setq *loaded-freetype-library* t)
    (format t "freetype2 library: ~S~%" freetype2:*library*)
    #+nil
    (multiple-value-bind (maj min pat)
        (freetype2:freetype-version)
      (when (and (eql 0 maj) (eql 0 min) (eql 0 pat))
        (setf freetype2:*library* (freetype2:make-freetype))
        (format t "new freetype2 library: ~S~%" freetype2:*library*))))
  (setq *face* (freetype2:new-face font-file))
  (set-font-size 1))

(defun orig-make-glyph-texture (width height buffer)
  (let* ((img-data (loop :for i :below width :collect
                      (loop :for j :below height :collect
                         (let ((val (aref buffer i j)))
                           (make-array 4
                                       :initial-contents
                                       (list (if (> val 0) 255 0)
                                             (if (> val 0) 255 0)
                                             (if (> val 0) 255 0)
                                             (if (> val 0) val 0)
                                             )))))))
    (with-c-array
        (temp (make-c-array img-data
                            :dimensions (list width height)
                            :element-type :uint8-vec4))
      (sample (make-texture temp)))))

(defun make-glyph-texture (width height buffer)
  (let* ((img-data (loop :for j :below height :collect
                      (loop :for i :below width :collect
                         (let ((val (aref buffer i j)))
                           (make-array 4
                                       :initial-contents
                                       (list val val val val)
                                       #+nil
                                       (list (if (> val 0) 255 0)
                                             (if (> val 0) 255 0)
                                             (if (> val 0) 255 0)
                                             (if (> val 0) val 0)
                                             )))))))
    (with-c-array
        (temp (make-c-array img-data
                            :dimensions (list width height)
                            :element-type :uint8-vec4))
      (let* ((texture (make-texture temp
                                    :element-type :rgba8))
             (sampler (sample texture)))
        sampler))))

(defun text-render (face text direction)
  (let* ((string (if (stringp text) text (princ-to-string text)))
         (flags (if (or (eq direction :up-down)
                        (eq direction :down-up))
                    '(:vertical-layout)
                    '(:default)))
         (height *glyph-array-size* #+nil (round (freetype2:string-pixel-height face string flags)))
         (width *glyph-array-size* #+nil (round (freetype2:string-pixel-width face string flags)))
         (array (make-array (list height width) :element-type 'unsigned-byte
                            :initial-element 0)))
    (declare (ignore flags))
    (freetype2:do-string-render (face string bitmap x y :direction direction)
      (let ((barray (freetype2:bitmap-to-array bitmap)))
        (let ((bh (freetype2::ft-bitmap-rows bitmap))
              (bw (freetype2::ft-bitmap-width bitmap)))
          (setf (gethash text *glyph-sizes*) (cons (/ bw width) (/ bh height)))
          (case direction
            (:left-right (freetype2::ablit array barray :x x :y y))
            (:right-left (freetype2::ablit array barray :x (+ width x) :y y))
            (:up-down    (freetype2::ablit array barray :x x :y y))
            (:down-up    (freetype2::ablit array barray :x x :y (+ height y))))
          (let ((texture (make-glyph-texture width height array)))
            (setf (gethash text *glyphs*) texture)))))))

(defun get-glyph (c)
  (or (gethash c *glyphs*)
      (when *face*
        (text-render *face* c :left-right))))

(defun get-glyph-size (c)
  (gethash c *glyph-sizes*))

(if *quicklisp-only*
    (ql:quickload "cl-freetype2")
    (asdf:operate 'asdf:load-op :cl-freetype2))

(in-package :cepler)

(setq *loaded-ft* t)

(defvar *face* nil)
(defvar *glyphs* (make-hash-table :test #'equal))
(defvar *glyph-sizes* (make-hash-table :test #'equal))
(defparameter *base-glyph-array-size* 32)
(defvar *glyph-array-size* *base-glyph-array-size*)

(defun set-font-size (size)
  (let ((array-size (round (/ *base-glyph-array-size* size))))
    (when (< array-size 24)
      (setq array-size 24))
    (when (> array-size 256)
      (setq array-size 256))
    (setq *glyph-array-size* array-size))
  (freetype2:set-char-size *face* (* 24 64) 0 72 72)
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
  (freetype2:set-char-size *face* (* 24 64) 0 72 72)
  (setq *glyphs* (make-hash-table :test #'equal)))

(defun make-glyph-texture (width height buffer)
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
                            :element-type :ubyte-vec4))
      (make-texture temp))))


(defun text-render (face string direction)
  (let* ((flags (if (or (eq direction :up-down)
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
        ;;(format t "char ~A  width: ~A height: ~A~%" string width height)
        ;;(describe bitmap)
        (let ((bh (freetype2::ft-bitmap-rows bitmap))
              (bw (freetype2::ft-bitmap-width bitmap)))
          (setf (gethash string *glyph-sizes*) (cons (/ bw width) (/ bh height)))
          ;;(format t "   bitmap  width: ~A height: ~A~%" bw bh))
          (case direction
            (:left-right (freetype2::ablit array barray :x x :y y))
            (:right-left (freetype2::ablit array barray :x (+ width x) :y y))
            (:up-down    (freetype2::ablit array barray :x x :y y))
            (:down-up    (freetype2::ablit array barray :x x :y (+ height y))))
          ;;(format t "array (~A): ~S~%" (array-dimensions array) array)
          ;;(format t "barray (~A): ~S~%" (array-dimensions barray) barray)
          ;;(setq height (car (array-dimensions array)) width (cadr (array-dimensions array)))
          (let ((texture (make-glyph-texture width height array)))
            ;;(describe texture)
            (setf (gethash string *glyphs*) texture)))))))

(defun text-test (string)
  (text-render *face* string :left-right))

(defun get-glyph (c)
  (let ((str (princ-to-string c)))
    (or (gethash str *glyphs*)
        (when *face*
          (text-render *face* (princ-to-string c)
                       ;;:up-down
                       :left-right
                       ;;:right-left
                       )))))

(defun get-glyph-size (c)
  (let ((str (princ-to-string c)))
    (gethash str *glyph-sizes*)))

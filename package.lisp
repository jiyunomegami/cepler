;;;; package.lisp

(defpackage #:cepler
  (:use #:cl #:cepl
        #:rtg-math #:varjo-lang #:livesupport
	#:skitter.sdl2.keys #:skitter.sdl2.mouse-buttons)
  (:export 
   :start-game))

(defvar *game-dir*
  (asdf:system-relative-pathname :cepler ""))

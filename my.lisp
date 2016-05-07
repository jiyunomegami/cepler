(defun my-getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defun my-command-line ()
  (or 
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

#+sbcl
(setf sb-impl::*default-external-format* :utf-8)
(load "quicklisp.lisp")
(load #p"~/quicklisp/setup.lisp")

#+(or sbcl allegro lispworks)
(load "asdf-recompile-invalid-fasl.lisp")

(ignore-errors (make-package :swank-loader))
(defun load-slime ()
  (let ((slime-dir #p"./slime/"))
    (when (directory slime-dir)
      (push slime-dir asdf:*central-registry*)))
  (asdf:operate 'asdf:load-op :swank))

(load-slime)
(format t "using slime in ~S~%" swank-loader:*source-directory*)

(defun load-openal ()
  (ql:quickload "bordeaux-threads")
  (ql:quickload "cl-openal")
  (ql:quickload "cl-alut")
  (ql:quickload "cl-alc"))

(defun load-stuff ()
  ;;(ql:quickload "cl-freetype2")
  (push #p"~/lgj/cl-freetype2/" asdf:*central-registry*)
  (push #p"~/lgj/documentation-utils/" asdf:*central-registry*)
  (push #p"~/lgj/temporal-functions/" asdf:*central-registry*)
  (push #p"~/lgj/cepl/" asdf:*central-registry*)
  (push #p"~/lgj/cepl.examples/" asdf:*central-registry*)
  (push #p"~/lgj/cepl.devil/" asdf:*central-registry*)
  (push #p"~/lgj/cepl.camera/" asdf:*central-registry*)
  (push #p"~/lgj/cepl.skitter/" asdf:*central-registry*)
  (push #p"~/lgj/cepl.sdl2/" asdf:*central-registry*)
  (push #p"~/lgj/cl-opengl/" asdf:*central-registry*)
  (push #p"~/lgj/cffi/" asdf:*central-registry*)
  ;; (asdf:operate 'asdf:load-op :cffi)
  ;; (asdf:operate 'asdf:load-op :cl-opengl)
  ;; (asdf:operate 'asdf:load-op :cepl)
  ;; (asdf:operate 'asdf:load-op :cepl.examples)
  ;; (asdf:operate 'asdf:load-op :cepl.devil)
  ;; (asdf:operate 'asdf:load-op :cepl.camera)
  ;; (asdf:operate 'asdf:load-op :cepl.sdl2)
  ;; (asdf:operate 'asdf:load-op :cepl.skitter.sdl2)
  )

(defun load-game ()
  (push #p"~/lgj/cepler/" asdf:*central-registry*)
  (asdf:operate 'asdf:load-op :cepler)
  (load (merge-pathnames "ft.lisp" *game-dir*)))

(defun load-using-quicklisp ()
  (format t "using packages from quicklisp only~%"))

(defvar *quicklisp-only* nil)

(defun load-all ()
  (let ((quicklisp-only (equal "1" (my-getenv "QUICKLISP_ONLY"))))
    (setq *quicklisp-only* quicklisp-only)
    (load-openal)
    (if quicklisp-only
        (load-using-quicklisp)
        (load-stuff))
    (load-game)))

(load-all)

(defparameter *swank-loopback-address* "127.0.0.1")
(defparameter *swank-port* 5005)
(defun swank-create-server (port)
  (swank:create-server :port port :dont-close t))

(defun swank-init (&optional port)
  (unless port (setq port *swank-port*))
  ;;(setf swank:*communication-style* :fd-handler)
  (let ((swank::*loopback-interface* *swank-loopback-address*))
    (swank-create-server port)))

#+sbcl
(defun my-toplevel ()
  (swank-init)
  ;;(cepler:start-game)
  (sb-ext:gc :full t)
  (let ((*package* (find-package :cepler)))
    (loop
      (handler-case
          (sb-impl::toplevel-repl nil)
        (sb-ext:timeout (x)
          (format t "repl timed out (~S)~%" x))))))

(defun exe-toplevel ()
  (let ((cmdline (my-command-line)))
    (format t "command line: ~S~%" cmdline)
    (cepler:start-game :game-dir (path:dirname (car cmdline)))))

(defun save-lisp (&key executable)
  #+sbcl
  (progn
    (sb-ext:gc :full t)
    (if executable
        (sb-ext:save-lisp-and-die
         "cepler"
         :toplevel #'exe-toplevel
         :executable t
         :compression 9
         :purify t)
        (sb-ext:save-lisp-and-die
         "my.core"
         ;;:compression 9
         ;;:purify t
         :toplevel #'my-toplevel))))

(save-lisp :executable (equal "1" (my-getenv "MAKE_EXECUTABLE")))

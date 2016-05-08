(in-package :cepler)

(defparameter *sound-lock* (bt:make-lock))
(defparameter *sound-condition* (bt:make-condition-variable))

(defvar *sound-queue* nil)
(defvar *stop-sound-queue* nil)

(defun play-sound-file (key file)
  (bt:with-lock-held (*sound-lock*)
    (setq *sound-queue* (append *sound-queue* (list (list key file))))
    (bt:condition-notify *sound-condition*)))

(defun stop-sound (key)
  (bt:with-lock-held (*sound-lock*)
    (setq *stop-sound-queue* (append *stop-sound-queue* (list key)))
    (bt:condition-notify *sound-condition*)))

(defvar *al-buffer-cache* (make-hash-table :test #'equal))

(defun load-sound-file (file)
  (let ((buffer (gethash file *al-buffer-cache*)))
    (if buffer
        buffer
        (setf (gethash file *al-buffer-cache*)
              (alut:create-buffer-from-file file)))))

(defvar *al-sources* 16)
(defvar *al-playing* (make-array *al-sources*
                                 :initial-contents (make-list *al-sources*)))

(defun sound-step (sources)
  (loop while *stop-sound-queue* do
       (let ((key (pop *stop-sound-queue*)))
         (dotimes (i (length *al-playing*))
           (let ((playing (aref *al-playing* i)))
             (when playing
               (when (eq playing key)
                 (let ((source (nth i sources)))
                   (al:source-stop source))))))))
  (let ((n 0))
    (dolist (source sources)
      (let ((state (cl-openal:get-source source :source-state)))
        ;;(format t "source ~A state: ~S~%" n state)
        ;; Possible values of state
        ;; AL_INITIAL
        ;; AL_STOPPED
        ;; AL_PLAYING
        ;; AL_PAUSED
        (when (or (eq state :initial) (eq state :stopped))
          (setf (aref *al-playing* n) nil)
          (let ((tuple (pop *sound-queue*)))
            (when tuple
              (let* ((key (first tuple))
                     (file (second tuple))
                     (buffer (load-sound-file file)))
                (when buffer
                  ;; Link source to buffer, and place
                  ;; source at (1 1 1).
                  (al:source source :buffer buffer)
                  (al:source source :position #(1 1 1))
                  (al:source source :velocity #(0 0 0))
                  ;; Place listener at (1 1 1), and have it
                  ;; face (0 0 0).
                  (al:listener :position #(1 1 1))
                  (al:listener :orientation #(0 0 0
                                              0 0 0))
                  ;; (al:source source :looping :true)
                  (al:source-play source)
                  (setf (aref *al-playing* n) key)))))
          #+nil
          (let ((new-state (cl-openal:get-source source :source-state)))
            (unless (eq new-state state)
              (format t "source ~A state change ~S -> ~S~%" n state new-state)))))
      (incf n))))

(defun sound-loop (sources)
  (loop
     (flet ((step-required ()
              (or *stop-sound-queue* *sound-queue*)))
       (bt:with-lock-held (*sound-lock*)
         (if (step-required)
             (sound-step sources)
             (bt:condition-wait *sound-condition* *sound-lock*))))))

(defun init-sound-state ()
  (setq *al-buffer-cache* (make-hash-table :test #'equal)
        *al-playing* (make-array *al-sources*
                                 :initial-contents (make-list *al-sources*))))

(defun init-sound ()
  (init-sound-state)
  (alut:with-init
    (alc:with-device (device)
      (when device
        (when (eq (cl-openal-alc:get-error device) :no-error)
          (alc:with-context (context device)
            (alc:make-context-current context)
            (al:with-sources (*al-sources* sources)
              (sound-loop sources))))))))

(defvar *sound-thread* nil)

(defun start-sound-thread ()
  (setq *sound-thread*
        (bt:make-thread
         #'(lambda ()
             (init-sound))
         :name "sound-thread")))

(defun stop-sound-thread ()
  (when (and *sound-thread* (bt:thread-alive-p *sound-thread*))
    (bt:destroy-thread *sound-thread*)
    (init-sound-state)))

(defvar *sound-sets* (make-hash-table :test #'equalp))
(defvar *sound-set* nil)

(defmacro defsoundset (name dir &rest sounds)
  (let ((varname (read-from-string (format nil "*sound-set-~A*" name))))
    `(progn
       (defvar ,varname)
       (setq ,varname
             (list ,name ,dir
                   (list ,@(loop for (key value) on sounds
                              when (keywordp key)
                              collect (list 'cons key value)))))
       (setf (gethash ,name *sound-sets*) ,varname))))

(defsoundset "sounds" "sounds/"
  :intro "intro.wav"
  :add-planet "space-blaster.wav"
  :hit-sun "deep-bass-rumble.wav"
  :hit-other "mortar-grenade-shell.wav")

(setq *sound-set* *sound-set-sounds*)

(defun play-sound (key)
  (when *sound-set*
    (let* ((filename (cdr (assoc key (third *sound-set*))))
           (pathname (merge-pathnames filename
                                      (merge-pathnames (second *sound-set*) *game-dir*))))
      (play-sound-file key pathname))))

;; sounds
;;   --- Filename ---          --- Changes ---         --- Attribution ---
;; mortar-grenade-shell.wav    converted to 16 bit     http://freesound.org/people/klankbeeld/
;; deep-bass-rumble.wav        converted w/sox         http://www.freesound.org/people/ERH/sounds/34187/
;; space-blaster.wav           converted to 16 bit     http://www.freesound.org/people/suonho/sounds/27568/
;; intro.wav                   converted w/sox         http://www.freesound.org/people/suonho/sounds/56364/

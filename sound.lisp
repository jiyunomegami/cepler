(in-package :cepler)

(defparameter *sound-lock* (bt:make-lock))
(defparameter *sound-condition* (bt:make-condition-variable))

(defvar *sound-queue* nil)
(defvar *stop-sound-queue* nil)

(defun play-sound-file (file)
  (bt:with-lock-held (*sound-lock*)
    (setq *sound-queue* (append *sound-queue* (list file)))
    (bt:condition-notify *sound-condition*)))

(defun stop-sound-file (file)
  (bt:with-lock-held (*sound-lock*)
    (setq *stop-sound-queue* (append *stop-sound-queue* (list file)))
    (bt:condition-notify *sound-condition*)))

(defvar *al-buffer-cache* (make-hash-table :test #'equal))

(defun load-sound-file (file)
  (let ((buffer (gethash file *al-buffer-cache*)))
    (if buffer
        buffer
        (setf (gethash file *al-buffer-cache*)
              (let ((buffer (alut:create-buffer-from-file
                             (merge-pathnames file
                                              (merge-pathnames "sounds/" *game-dir*)))))
                buffer)))))

(defvar *al-interval* (/ 1 40))
(defvar *al-sources* 8)
(defvar *al-playing* (make-array *al-sources*
                                 :initial-contents (make-list *al-sources*)))

(defun sound-step (sources)
  (let ((n 0))
    (when *stop-sound-queue*
      (let ((file (pop *stop-sound-queue*)))
        (dotimes (i (length *al-playing*))
          (let ((playing (aref *al-playing* i)))
            (when playing
              (when (string= playing file)
                (let ((source (nth i sources)))
                  (al:source-stop source))))))))
    (dolist (source sources)
      (let ((state (cl-openal:get-source source :source-state)))
        ;;(format t "source ~A ~S~%" n state)
        (when (and (not (eq state :playing))
                   (not (null (aref *al-playing* n))))
          (setf (aref *al-playing* n) nil))
        (unless (eq state :playing)
          (let ((file (pop *sound-queue*)))
            (when file
              (let ((buffer (load-sound-file file)))
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
                  (setf (aref *al-playing* n) file)
                  ))))
          #+nil
          (let ((new-state (cl-openal:get-source source :source-state)))
            (unless (eq new-state state)
              (format t "source ~A state change ~S -> ~S~%" n state new-state)))))
      (incf n))))

(defun init-sound ()
  (alut:with-init
    (alc:with-device (device)
      ;; Here it is appropriate to check so
      ;; device is actually opened. GET-ERROR
      ;; for example.
      (alc:with-context (context device)
        (alc:make-context-current context)
        (al:with-sources (*al-sources* sources)
          (loop
             (bt:with-lock-held (*sound-lock*)
               (sound-step sources)
               (bt:condition-wait *sound-condition* *sound-lock*)
               ;;(sleep *al-interval*)
               )))))))

(defvar *sound-thread* nil)

(defun start-sound-thread ()
  (setq *al-buffer-cache* (make-hash-table :test #'equal))
  (setq *sound-thread*
        (bt:make-thread
         #'(lambda ()
             (init-sound))
         :name "sound-thread")))

(defun stop-sound-thread ()
  (when (and *sound-thread* (bt:thread-alive-p *sound-thread*))
    (bt:destroy-thread *sound-thread*))
  (setq *al-buffer-cache* (make-hash-table :test #'equal)))

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
    (let ((file (cdr (assoc key (third *sound-set*)))))
      ;;(format t "  file: ~S~%" file)
      (play-sound-file file))))

(defun stop-sound (key)
  (when *sound-set*
    (let ((file (cdr (assoc key (third *sound-set*)))))
      (stop-sound-file file))))

;; sounds
;;   --- Filename ---          --- Changes ---         --- Attribution ---
;; mortar-grenade-shell.wav    converted to 16 bit     http://freesound.org/people/klankbeeld/
;; deep-bass-rumble.wav        converted w/sox         http://www.freesound.org/people/ERH/sounds/34187/
;; space-blaster.wav           converted to 16 bit     http://www.freesound.org/people/suonho/sounds/27568/
;; intro.wav                   converted w/sox         http://www.freesound.org/people/suonho/sounds/56364/

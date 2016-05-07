;;; -*- Mode: lisp; Syntax: common-lisp; indent-tabs-mode: nil -*-

;;; If the fasl was stale, try to recompile and load (once). Since only SBCL
;;; has a separate condition for bogus fasls we retry on any old error
;;; on other lisps. Actually, Allegro has a similar condition, but it's 
;;; unexported.  Works nicely for the ACL7 upgrade, though.
(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (#+sbcl sb-ext:invalid-fasl 
     #+allegro excl::file-incompatible-fasl-error
     #+lispworks conditions:fasl-error
     #-(or sbcl allegro lispworks) error ()
     (asdf:perform (make-instance 'asdf:compile-op) c)
     (call-next-method))))

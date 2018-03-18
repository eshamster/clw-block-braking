(defpackage clw-block-braking/src/game/life
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-life
           :get-rest-life
           :add-life-decrease-event)
  (:import-from :clw-block-braking/src/game/ball
                :add-ball-falling-event))
(in-package :clw-block-braking/src/game/life)

;; --- event --- ;;

(defvar.ps+ *life-decrease-event* (make-hash-table))

(defun.ps+ add-life-decrease-event (name func)
  (setf (gethash name *life-decrease-event*) func))

;; --- others --- ;;

(defvar.ps+ *rest-life* -1)
(defvar.ps+ *init-life* 2)

(defun.ps+ init-life ()
  (setf *rest-life* *init-life*)
  (add-ball-falling-event
   :decrease-life
   (lambda (ball)
     (declare (ignore ball))
     (decf *rest-life*)
     (maphash (lambda (name func)
                (declare (ignore name))
                (funcall func *rest-life*))
              *life-decrease-event*))))

(defun.ps+ get-rest-life ()
  *rest-life*)

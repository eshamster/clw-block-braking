(defpackage clw-block-braking/game/state/utils
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :register-state-maker
           :make-state
           :def-game-state

           :get-current-ball
           :reset-ball-on-field
           :delete-all-entities-in-next-frame)
  (:import-from :clw-block-braking/game/ball
                :reset-ball))
(in-package :clw-block-braking/game/state/utils)

;; --- state management utils --- ;;

(defvar.ps+ *state-maker-table* (make-hash-table))

(defun.ps+ make-state (kind &rest keys)
  (apply (gethash kind *state-maker-table*) keys))

(defun.ps+ register-state-maker (kind func)
  (setf (gethash kind *state-maker-table*) func))

;; --- (experimental macro) --- ;;

(defmacro.ps+ def-game-state (name (&rest params) &key start-process process end-process)
  `(progn
     (defstruct.ps+
         (,(intern (format nil "GAME-~A-STATE" name))
           (:include game-state
                     ,@(append (when start-process
                                 `((start-process ,start-process)))
                               (when process
                                 `((process ,process)))
                               (when end-process
                                 `((end-process ,end-process))))))
         ,@params)))

;; --- other utils --- ;;

(defun.ps+ get-current-ball ()
  (let ((current-ball (find-a-entity-by-tag :ball)))
    (assert current-ball)
    current-ball))

(defun.ps+ reset-ball-on-field ()
  (reset-ball (get-current-ball)))

(defun.ps+ delete-all-entities-in-next-frame ()
  (do-ecs-entities entity
    (unless (ecs-entity-parent entity)
      (register-next-frame-func
       (lambda () (delete-ecs-entity entity))))))

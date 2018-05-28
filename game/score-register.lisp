(defpackage clw-block-braking/game/score-register
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-score-register
           :register-score
           :get-score
           :score
           :score-time))
(in-package :clw-block-braking/game/score-register)

;; --- basic --- ;;

(defstruct.ps+ score (time 0))

(defun.ps+ find-score-register ()
  (find-a-entity-by-tag :score-register))

(defun.ps+ reset-score-register (&optional (score-register (find-score-register)))
  (check-entity-tags score-register :score-register)
  (set-entity-param score-register :frame-count 0))

(defun.ps+ register-score (&key (score-register (find-score-register))
                                stage time
                                (persist-p t))
  (check-entity-tags score-register :score-register)
  (assert stage)
  ;; Note: In JavaScript, 0 is interpreted as false.
  (assert (not (null time)))
  (setf (gethash stage (get-entity-param score-register :register))
        (make-score :time time))
  (when persist-p
    (make-current-score-persist stage score-register)))

(defun.ps+ get-score (stage &optional (score-register (find-score-register)))
  (check-entity-tags score-register :score-register)
  (let ((score (gethash stage (get-entity-param score-register :register))))
    (assert score)
    (score-time score)))

(defun.ps+ init-score-register ()
  (let ((score-register (make-ecs-entity)))
    (add-entity-tag score-register :score-register)
    (add-ecs-component-list
     score-register
     (init-entity-params :register (make-hash-table)))
    (add-ecs-entity score-register)))

;; --- persistence --- ;;

(defvar.ps+ *store-prefix* "clw-bb:score-")

(defun.ps+ make-current-score-persist (stage &optional (score-register (find-score-register)))
  (check-entity-tags score-register :score-register)
  (with-kvs-prefix (*store-prefix*)
    (let* ((new-key (+ "new-" stage))
           (old-key (+ "pre-max-" stage))
           (pre-new-value (read-kvs new-key))
           (pre-old-value (read-kvs old-key)))
      (unless (null pre-new-value)
        ;; Less is better
        (when (or (null pre-old-value)
                  (< pre-new-value pre-old-value))
          (store-kvs old-key pre-new-value)))
      (store-kvs new-key (get-score stage score-register)))))

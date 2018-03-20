(defpackage clw-block-braking/src/game/stage-generator
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-test-stage)
  (:import-from :clw-block-braking/src/game/block
                :make-rect-block))
(in-package :clw-block-braking/src/game/stage-generator)

(defun.ps+ make-test-stage (field)
  (interpret-stage *stage1* field))

(defstruct.ps+ block-info name width height)

;; Note: width and height is relative value to field size
(defvar.ps+ *block-list*
    (list (make-block-info :name :block1
                           :width 0.07
                           :height 0.025)))

;; Note: x and y is relative value to block size
(defvar.ps+
    *stage1*
    '(:blocks ((:info (:name :block1 :offset-x 0.9 :offset-y 25)
                :sequences ((:min (0 0) :max (11 1) :step (1 1))
                            (:min (0 4) :max (11 4) :step (3 1))
                            (:min (1 5) :max (11 5) :step (3 1))
                            (:min (0.5 8) :max (11.5 9) :step (2 1))
                            (:min (0 11) :max (11 11) :step (1 1)))))))

;; Note: x and y is relative value to block size
(defun.ps+ make-block-using-info (name x y field)
  (let ((info (find-if (lambda (info)
                         (eq (block-info-name info) name))
                       *block-list*)))
    (assert info)
    (let ((width (* (get-entity-param field :width)
                    (block-info-width info)))
          (height (* (get-entity-param field :height)
                     (block-info-height info))))
      (make-rect-block (* width x) (* height y) width height))))

(defun.ps+ interpret-stage (stage-info field)
  (dolist (cluster (getf stage-info :blocks))
    (let* ((info (getf cluster :info))
           (name (getf info :name))
           (offset-x (getf info :offset-x))
           (offset-y (getf info :offset-y)))
      (dolist (seq (getf cluster :sequences))
        (let ((min (getf seq :min))
              (max (getf seq :max))
              (step (getf seq :step)))
          (loop :for x = (car min) :then (+ x (car step)) :until (> x (car max)) :do
             (loop :for y = (cadr min) :then (+ y (cadr step)) :until (> y (cadr max)) :do
                (add-ecs-entity
                 (make-block-using-info
                  name (+ x offset-x) (+ y offset-y) field)
                 field))))))))

(defpackage clw-block-braking/game/stage-generator
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :generate-stage
           :get-max-stage-number)
  (:import-from :clw-block-braking/game/block
                :make-rect-block))
(in-package :clw-block-braking/game/stage-generator)

(defstruct.ps+ block-info name width height texture)

;; Note: width and height is relative value to field size
(defvar.ps+ *block-list*
    (list (make-block-info :name :block1
                           :width 0.07
                           :height 0.025
                           :texture "block")
          (make-block-info :name :super-wide-block
                           :width 0.9
                           :height 0.05
                           :texture "block")))

;; Note: x and y is relative value to block size
(defvar.ps+
    *stage1*
    '(:blocks ((:info (:name :block1 :offset-x 0.9 :offset-y 25)
                :sequences ((:min (0 0) :max (11 1) :step (1 1))
                            (:min (0 4) :max (11 4) :step (3 1))
                            (:min (1 5) :max (11 5) :step (3 1))
                            (:min (0.5 8) :max (11.5 9) :step (2 1))
                            (:min (0 11) :max (11 11) :step (1 1)))))))

(defvar.ps+
    *test-stage*
    '(:blocks ((:info (:name :super-wide-block :offset-x 0.05 :offset-y 10)
                :sequences ((:min (0 0) :max (0 0) :step (1 1)))))))

(defun.ps+ get-max-stage-number ()
  (labels ((rec (stage-number)
             (if (get-stage-info stage-number)
                 (rec (1+ stage-number))
                 (1- stage-number))))
    (rec 1)))

(defun.ps+ get-stage-info (stage-number)
  (case stage-number
    (1 *test-stage*)
    (2 *stage1*)
    (t nil)))

;; Note: x and y is relative value to block size
(defun.ps+ make-block-using-info (name x y field)
  (let ((info (find-if (lambda (info)
                         (eq (block-info-name info) name))
                       *block-list*)))
    (assert info)
    (let* ((width (* (get-entity-param field :width)
                     (block-info-width info)))
           (height (* (get-entity-param field :height)
                      (block-info-height info)))
           (result (make-rect-block (* width x) (* height y) width height))) 
      (frame-promise-then
       (make-texture-model-promise
        :width width :height height
        :texture-name (block-info-texture info))
       (lambda (model)
         (add-ecs-component-list
          result
          (make-model-2d :model model
                         :offset (make-point-2d :x (/ width -2)
                                                :y (/ height -2))))))
      result)))

(defun.ps+ generate-stage (stage-number field)
  (dolist (cluster (getf (get-stage-info stage-number) :blocks))
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

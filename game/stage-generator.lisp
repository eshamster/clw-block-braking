(defpackage clw-block-braking/game/stage-generator
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :generate-stage
           :get-max-stage-number)
  (:import-from :clw-block-braking/game/block
                :make-rect-block)
  (:import-from :clw-block-braking/game/parameter
                :get-param))
(in-package :clw-block-braking/game/stage-generator)

(defstruct.ps+ block-info name width height texture)

;; Note: width and height is relative value to field size
(defvar.ps+ *block-list*
    (list (make-block-info :name :mini-block
                           :width 0.05
                           :height 0.025
                           :texture "block")
          (make-block-info :name :big-block
                           :width 0.1
                           :height 0.02
                           :texture "block")
          (make-block-info :name :block2
                           :width 0.07
                           :height 0.025
                           :texture "block")
          (make-block-info :name :super-wide-block
                           :width 0.9
                           :height 0.05
                           :texture "block")))

;; Note: x and y is relative value to block size
(defvar.ps+
    *stage-sparse*
    '(:blocks ((:info (:name :mini-block :offset-x 2 :offset-y 3)
                :sequences ((:min (0 0) :max (15 6) :step (5 3)))))))

(defvar.ps+
    *stage-dense*
    '(:blocks ((:info (:name :big-block :offset-x 0 :offset-y 3)
                :sequences ((:min (0 0) :max (9 4) :step (1 1)))))))

(defvar.ps+
    *stage1*
    '(:blocks ((:info (:name :block2 :offset-x 0.9 :offset-y 3)
                :sequences ((:min (0 0) :max (11 0) :step (1 1))
                            (:min (0.5 3) :max (11.5 4) :step (2 1))
                            (:min (1 6) :max (11 6) :step (3 1))
                            (:min (0 7) :max (11 7) :step (3 1))
                            (:min (0 10) :max (11 11) :step (1 1)))))))

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
    (1 *stage-sparse*)
    (2 *stage-dense*)
    (3 *stage1*)
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
           (result (make-rect-block (* width x)
                                    (- (get-param :field :height) (* height (1+ y)))
                                    width height)))
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

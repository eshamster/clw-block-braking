(defpackage clw-block-braking/game/state/menu_stage-selector
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :generate-stage-list
           :init-stage-selector)
  (:import-from :clw-block-braking/game/stage-generator
                :get-max-stage-number))
(in-package :clw-block-braking/game/state/menu_stage-selector)

;; --- utils --- ;;

(defun.ps+ increment-selector (selector delta)
  (aset-entity-param selector :selected
                     (mod (+ it delta)
                          (length (get-entity-param selector :choices))))
  (funcall (get-entity-param selector :on-change)
           (get-entity-param selector :selected)))

(defun.ps+ generate-all-stage-list ()
  "Ex. (1 2 3)"
  (loop for i from 1 to (get-max-stage-number)
     collect i))

(defun.ps+ generate-candidate-to-select ()
  "Ex. (:all 1 2 3)"
  (let ((result (generate-all-stage-list)))
    (push :all result)
    result))

(defun.ps+ generate-stage-list (selector)
  "Ex (1 2 3) or (2)"
  (let ((target (nth (get-entity-param selector :selected)
                     (get-entity-param selector :choices))))
    (case target
      (:all (generate-all-stage-list))
      (t (list target)))))

;; --- UI to select stage  --- ;;

;; TODO: Hover animation
(defun.ps+ init-incrementor (&key selector increase-p offset)
  (frame-promise-then
   (make-text-model-promise (if increase-p ">" "<")
                            :size 20
                            :color #x00ffff)
   (lambda (text-mesh)
     (let* ((incrementor (make-ecs-entity))
            (size (get-mesh-size text-mesh))
            (width (getf size :width))
            (height (getf size :height)))
       (add-ecs-component-list
        incrementor
        offset
        (make-model-2d :model text-mesh :depth 100)
        (make-physic-polygon
         :pnt-list (list (make-point-2d :x 0 :y 0)
                         (make-point-2d :x width :y 0)
                         (make-point-2d :x width :y height)
                         (make-point-2d :x 0 :y height)))
        (make-ui-component
         :on-click-up
         (lambda ()
           (increment-selector selector (if increase-p 1 -1)))))
       (add-ecs-entity incrementor selector)))))

(defun.ps+ init-stage-display (&key selector offset first-choice)
  (let ((display (make-ecs-entity))
        (choices (generate-candidate-to-select))
        (model-list '()))
    (add-ecs-component-list display offset)
    (frame-promise-all
     (mapcar (lambda (item)
               (make-text-model-promise
                item :size 20 :color #xff0088))
             choices)
     (lambda (meshes)
       (setf model-list
             (mapcar (lambda (mesh)
                       (let ((model (make-model-2d :model mesh
                                                   :depth 100)))
                         (add-ecs-component-list display model)
                         model))
                     meshes))
       (flet ((change-model (selected)
                (disable-model-2d display)
                (let ((model (nth selected model-list)))
                  (assert model)
                  (enable-model-2d display :target-model-2d model))))
         (change-model first-choice)
         (aset-entity-param selector :on-change
                            (lambda (selected)
                              (change-model selected)
                              (funcall it selected))))))
    (add-ecs-entity display selector)))

;; --- body --- ;;

(defun.ps+ init-stage-selector (parent &key offset)
  (let ((selector (make-ecs-entity))
        (choices (generate-candidate-to-select)))
    (add-entity-tag selector :stage-selector)
    (add-ecs-component-list
     selector
     offset
     (make-script-2d
      :func (lambda (entity)
              (let ((wheel-delta (get-mouse-wheel-delta-y)))
                (cond ((> wheel-delta 0)
                       (increment-selector entity 1))
                      ((< wheel-delta 0)
                       (increment-selector entity -1))))))
     (init-entity-params :choices choices
                         :selected 0
                         :on-change (lambda (selected) (declare (ignore selected)))))
    (add-ecs-entity selector parent)
    (init-incrementor :increase-p nil
                      :offset (make-point-2d :x 0 :y 0)
                      :selector selector)
    (init-incrementor :increase-p t
                      :offset (make-point-2d :x 80 :y 0)
                      :selector selector)
    (init-stage-display :offset (make-point-2d :x 30 :y 0)
                        :first-choice 0
                        :selector selector)))

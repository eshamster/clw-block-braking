(defpackage clw-block-braking/game/score-register
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-score-register
           :register-score
           :get-score
           :get-total-score
           :score
           :score-time

           :get-best-score
           :update-best-record-p
           :clear-best-score))
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

(defvar.ps+ *truncate-scale* 10)

(defun.ps+ get-score (stage &optional (score-register (find-score-register)))
  (check-entity-tags score-register :score-register)
  (let ((score (gethash stage (get-entity-param score-register :register))))
    (assert score)
    (/ (floor (* (score-time score) *truncate-scale*))
       *truncate-scale*)))

(defun.ps+ get-total-score (&optional (score-register (find-score-register)))
  "To avoid round-up error, use this rather than sum values of \"get-score\" by yourself"
  (check-entity-tags score-register :score-register)
  (let ((scaled-result 0))
    (maphash (lambda (stage _)
               (declare (ignore _))
               ;; XXX: Adhoc solution to avoid from adding total score to itself...
               (unless (eq stage :all)
                 (let ((score (get-score stage score-register)))
                   (add-to-event-log (+ stage ": " score))
                   (incf scaled-result
                         (* (get-score stage score-register) *truncate-scale*)))))
             (get-entity-param score-register :register))
    (/ scaled-result *truncate-scale*)))

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

(defun.ps+ get-stage-record (stage)
  (with-kvs-prefix (*store-prefix*)
    (list :new (read-kvs (+ "new-" stage))
          :pre-max (read-kvs (+ "pre-max-" stage)))))

(defun.ps+ update-best-record-p (stage)
  (with-kvs-prefix (*store-prefix*)
    (let ((record (get-stage-record stage)))
      (or (null (getf record :pre-max))
          (< (getf record :new)
             (getf record :pre-max))))))

(defun.ps+ get-best-score (stage)
  (with-kvs-prefix (*store-prefix*)
    (let ((record (get-stage-record stage)))
      (if (null (getf record :pre-max))
          (getf record :new)
          (min (getf record :new)
               (getf record :pre-max))))))

;; TODO: Enable to use this from menu
(defun.ps+ clear-best-score ()
  (clear-kvs-all))

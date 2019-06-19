(in-package #:snake)

(defun mod-vec (this that)
  "MOD x and y of THIS by x and y of THAT."
  (gamekit:vec2 (mod (gamekit:x this) (gamekit:x that))
                (mod (gamekit:y this) (gamekit:y that))))

(defun centered-vec (width height)
  (gamekit:vec2 (/ width 2)
                (/ height 2)))

;; TODO(bsvercl): This doesn't work if the minimum is greater than maximum.
;; I haven't tested negative numbers yet, but I know RANDOM doesn't accept them.
(defun random-between (minimum maximum)
  (+ minimum (random (1+ maximum))))

(defun random-vec (end1 end2 &key (start1 0) (start2 0))
  (gamekit:vec2 (random-between start1 end1)
                (random-between start2 end2)))

(defun draw-segment (origin &key (fill-paint nil) (stroke-paint nil) (size +segment-size+))
  (gamekit:draw-rect (gamekit:mult origin size)
                     size size
                     :fill-paint fill-paint
                     :stroke-paint stroke-paint))

(defun direction-to-vec (direction)
  (case direction
    (:up (gamekit:vec2 0 1))
    (:down (gamekit:vec2 0 -1))
    (:left (gamekit:vec2 -1 0))
    (:right (gamekit:vec2 1 0))
    ;; We don't know that this is supposed to be.
    (t (gamekit:vec2))))

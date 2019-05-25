;;;; snake.lisp

(in-package #:snake)

(defparameter +segment-size+ 10.0)
(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)

;; TODO(bsvercl): Use this for wrapping snake position.
(defgeneric mod-vec (vec divisor))
(defmethod mod-vec ((vec gamekit:vec2) divisor)
  (gamekit:vec2 (mod (gamekit:x vec) divisor)
                (mod (gamekit:y vec) divisor)))

;; TODO(bsvercl): POSITION-OF does not seem to work correctly,
;; which is why WITH-SLOTS is used all over.
(defclass segment ()
  ((position :initarg :position :accessor position-of)))

(defun make-segment (position)
  (make-instance 'segment :position position))

(defclass snake ()
  ((segments :initarg :segments :accessor segments-of)
   (direction :initarg :direction :accessor direction-of)))

(defun make-snake (starting-position &optional (direction (gamekit:vec2)))
  (let* ((head (make-segment starting-position))
         (segments (make-array 1 :element-type 'segment
                                 :initial-element head
                                 :adjustable t
                                 :fill-pointer t)))
    (make-instance 'snake :segments segments :direction direction)))

(defun snake-head (snake)
  (aref (segments-of snake) 0))

(defun change-direction (snake direction)
  (setf (direction-of snake) direction))

(gamekit:defgame snake-game ()
  ((player :type snake :accessor player-of)
   (food-pos :initform nil :accessor food-pos-of))
  (:viewport-title "Snake")
  (:viewport-width +screen-width+)
  (:viewport-height +screen-height+))

(defmethod gamekit:post-initialize ((this snake-game))
  (with-slots (player food-pos) this
    (setf player (make-snake (gamekit:vec2)))
    ;; TODO(bsvercl): avoid selecting a spot occupied by the player.
    (flet ((random-location (size)
             (floor (/ +segment-size+ (random size)))))
      (let ((x-pos (random-location *screen-width*))
            (y-pos (random-location *screen-height*)))
        (setf food-pos (gamekit:vec2 x-pos y-pos))))
    (with-slots (direction) player
      ;; TODO(bsvercl): ??? I think we can go deeper.
      (macrolet ((binder (keycode &body body)
                   `(gamekit:bind-button ,keycode :pressed
                                         (lambda () ,@body))))
        (binder :w (setf direction (gamekit:vec2 0 1)))
        (binder :s (setf direction (gamekit:vec2 0 -1)))
        (binder :a (setf direction (gamekit:vec2 -1 0)))
        (binder :d (setf direction (gamekit:vec2 1 0)))))))

(defmethod gamekit:act ((this snake-game))
  ;; TODO(bsvercl): This is ugly.
  (with-slots (segments direction) (player-of this)
    (with-slots (position) (aref segments 0)
      (setf position (gamekit:add position direction)))))

(defmethod gamekit:draw ((this snake-game))
  (with-slots (segments) (player-of this)
    (loop for segment across segments
          do (with-slots (position) segment
               (gamekit:draw-rect position
                                  +segment-size+ +segment-size+
                                  :fill-paint (gamekit:vec4 1.0 0.75 0.5 1.0))))))

(defun play ()
  (gamekit:start 'snake-game :viewport-resizable nil))

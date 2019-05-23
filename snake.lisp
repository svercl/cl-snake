;;;; snake.lisp

(in-package #:snake)

(defparameter +segment-size+ 10.0)

(defclass segment ()
  ((position
    :initarg :position
    :initform (gamekit:vec2)
    :accessor segment-position)
   (direction ;; :up :down :left :right
    :initarg :direction
    :initform (error "Must supply :direction.")
    :accessor segment-direction)))

(defun make-segment (position direction)
  (make-instance 'segment :position position :direction direction))

(defclass snake ()
  ((segments
    :initarg :segments
    :initform nil
    :accessor snake-segments)))

(defmethod initialize-instance :after ((this snake) &key)
  (push (make-segment (gamekit:vec2) :up) (snake-segments this)))

(defun snake-head (snake)
  (first (snake-segments snake)))

(defun change-direction (snake dir)
  (let* ((segments (snake-segments snake))
         (segment (first segments)))
    (setf (segment-direction segment) dir)))

(gamekit:defgame snake-game ()
  ((player :type snake))
  (:viewport-title "Snake")
  (:viewport-width 800)
  (:viewport-height 600))

(defmethod gamekit:post-initialize ((this snake-game))
  (with-slots (player) this
    (setf player (make-instance 'snake)))
  (gamekit:bind-button :w :pressed
               (lambda ()
                 ())))

(defmethod gamekit:act ((this snake-game))
  ())

(defmethod gamekit:draw ((this snake-game))
  (gamekit:draw-text "Hello" (gamekit:vec2 10 10))
  (with-slots (player) this
    (let ((segments (snake-segments player)))
      (dolist (segment segments)
        (let ((pos (segment-position segment)))
          (gamekit:draw-rect pos +segment-size+ +segment-size+ :fill-paint (gamekit:vec4 1.0 0.75 0.5 1.0)))))))

(defun play ()
  (gamekit:start 'snake-game :viewport-resizable nil))

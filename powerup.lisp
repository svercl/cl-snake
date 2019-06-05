(in-package #:snake)

(defclass powerup ()
  ((position :initarg :position :reader position-of))
  (:documentation "A mutator."))

(defgeneric apply-powerup (powerup snake)
  (:documentation "Mutates SNAKE with POWERUP."))

(defclass color-changer (powerup)
  ((color :initarg :color))
  (:documentation "Changes color of SNAKE."))

(defun make-color-changer (position color)
  (make-instance 'color-changer :position position
                                :color color))

(defmethod apply-powerup ((this color-changer) snake)
  (with-slots (color) this
    (setf (color-of snake) color)))

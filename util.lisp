(in-package #:snake)

(defun mod-vec (this that)
  "MOD x and y of THIS by x and y of THAT."
  (gamekit:vec2 (mod (gamekit:x this) (gamekit:x that))
                (mod (gamekit:y this) (gamekit:y that))))

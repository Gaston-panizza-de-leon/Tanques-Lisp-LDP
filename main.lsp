(defun pinta ()
    (cls)
    (color 0 0 0)
    (rectangle 0 0 640 340)
    (color 0 0 0))

(defun rectangle (x y w h)
    (move x y)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h)))

(defun mida-muro ()
(pinta-campo(+ 20 (random 20)) (+ 15 (random 30))))

(defun pinta-campo-muro (x y))
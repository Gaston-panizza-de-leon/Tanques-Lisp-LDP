(defun inici ()
    "inici del programa (tecles)"
    (putprop 'escenari (+ 20 (random 21)) 'muramp)
    (putprop 'escenari (+ 100 (random 51)) 'muralt)

    (putprop 'escenari (+ (- (floor (- 640 (get 'escenari 'muramp)) 2) 20)
     (random 40)) 'camp1amp)

    (putprop 'escenari (- (- 640 (get 'escenari 'muramp)) 
    (get 'escenari 'camp1amp)) 'camp2amp)

    (putprop 'escenari (+ 15 (random 31)) 'camp1alt)
    (putprop 'escenari (+ 15 (random 31)) 'camp2alt)

    (putprop 'cano1 45 'angle)
    (putprop 'cano2 135 'angle)

    (putprop 'cano1 (+ (floor (get 'escenari 'camp1amp) 3) 
    (random (floor (get 'escenari 'camp1amp) 3)))'posicio)

    (putprop 'cano2 (+ (+ (+ (floor (get 'escenari 'camp2amp) 3) 
    (random (floor (get 'escenari 'camp2amp) 3))) (get 'escenari 
    'camp1amp)) (get 'escenari 'muramp)) 'posicio)

    (putprop 'cano1 20 'velocitat)
    (putprop 'cano2 20 'velocitat)

    (repeteix))

(defun inc-angle ()
    "incrementa l'angle"
    (putprop 'programa (+ (get 'programa 'angle) 1) ' angle))

(defun dec-angle ()
    "decrementa l'angle"
    (putprop 'programa (- (get 'programa 'angle) 1) ' angle))

(defun repeteix ()
    (pinta)
    (princ "Pitja fletxa cap amunt o cap avall o ESC.")
    (terpri)
    (cond ((equal (get-key) 328) ; fletxa cap amunt
           (inc-angle) (repeteix)) ; incrementa angle i repeteix
          ((equal (get-key) 336) ; fletxa cap avall
           (dec-angle) (repeteix)) ; decrementa angle i repeteix
          ((equal (get-key) 27)  ; ESC
           t)                      ; acaba recursió
          (t                 ; altrament
           (repeteix))))           ; repeteix

(defun pinta ()
    (cls)
    (color 0 0 0)
    (rectangle 0 0 639 339)
    (rectangle 0 0 (get 'escenari 'camp1amp) (get 'escenari 'camp1alt))

    (rectangle (get 'escenari 'camp1amp) 0 (get 'escenari 'muramp) 
    (get 'escenari 'muralt))

    (rectangle (+ (get 'escenari 'camp1amp) (get 'escenari 'muramp)) 0 
    (get 'escenari 'camp2amp) (get 'escenari 'camp2alt))

    (rectangle (get 'cano1 'posicio) (get 'escenari 'camp1alt) 20 10)

    (angle (+ (get 'cano1 'posicio) 10) (+ (get 'escenari 'camp1alt) 10)
    15 (get 'cano1 'angle))
 
    (rectangle (get 'cano2 'posicio) (get 'escenari 'camp2alt) 20 10)

    (angle (+ (get 'cano2 'posicio) 10) (+ (get 'escenari 'camp2alt) 10)
    15 (get 'cano2 'angle))
)


(defun angle (x y r angle)
    (move x y)
    (drawr (+ x (* r (cos (radians angle))))
           (+ y (* r (sin (radians angle))))))

(defun rectangle (x y w h)
    (move x y)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h)))

(defun mover (x y)
  "mou a les coordenades arrodonides"
  (move (round x) 
        (round y)))

(defun drawr (x y)
  "pinta a les coordenades arrodonides"
  (draw (round x) 
        (round y)))

(defun radians (graus)
  (/ (* graus (* 2 pi)) 360))

(defun inici ()
    "inici del programa (tecles)"
    (putprop 'escenari (+ 20 (random 21)) 'muramp)
    (putprop 'escenari (+ 100 (random 51)) 'muralt)
    (putprop 'escenari (+ (- (/ (- 640 (get 'escenari 'muramp)) 2) 20) (random 40)) 'camp1amp)
    (putprop 'escenari (+ 15 (random 31)) 'camp1alt)
    (putprop 'escenari (- (- 640 (get 'escenari 'muramp)) (get 'escenari 'camp1amp)) 'camp2amp)
    (putprop 'escenari (+ 15 (random 31)) 'camp2alt)
    (putprop 'cano1 45 'angle)
    (putprop 'cano2 45 'angle)
    (putprop 'cano1 (+ (/ (get 'escenari 'camp1amp) 3) (* (/ (get 'escenari 'camp1amp) 3) 2)) 'posicio)
    (putprop 'cano2 (+ (/ (get 'escenari 'camp2amp) 3) (* (/ (get 'escenari 'camp2amp) 3) 2)) 'posicio)
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
    (color 255 0 0)
    (rectangle 0 0 640 640)
    
    ;(color 0 0 0)
    ;(cercle 320 170 100 12)
    ;(color 255 0 0)
    ;(angle 320 170 100 (get 'programa 'angle))
    ;(color 0 0 0)
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

(defun cercle (x y radi segments)
    (mover (+ x radi) y)
    (cercle2 x y radi (/ 360 segments) 0))

(defun cercle2 (x y radi pas angle)
  (cond ((< angle 360)
         (drawr (+ x (* radi (cos (radians (+ angle pas)))))
                (+ y (* radi (sin (radians (+ angle pas))))))
         (cercle2 x y radi pas (+ angle pas)))
        (t t)))

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

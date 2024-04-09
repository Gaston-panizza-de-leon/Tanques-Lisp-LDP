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

(defun inc-angle-esq ()
    "incrementa l'angle"
    (cond 
    ((< 180 (get 'cano1 'angle)) (putprop 'cano1 180 ' angle))
    ((> 0 (get 'cano1 'angle)) (putprop 'cano1 0 ' angle))
    (t (putprop 'cano1 (+ (get 'cano1 'angle) 1) ' angle))))

(defun dec-angle-esq ()
    "decrementa l'angle"
    (cond 
    ((< 180 (get 'cano1 'angle)) (putprop 'cano1 180 ' angle))
    ((> 0 (get 'cano1 'angle)) (putprop 'cano1 0 ' angle))
    (t (putprop 'cano1 (- (get 'cano1 'angle) 1) ' angle))))

(defun inc-angle-dre ()
    "incrementa l'angle"
    (cond 
    ((< 180 (get 'cano2 'angle)) (putprop 'cano2 180 ' angle))
    ((> 0 (get 'cano2 'angle)) (putprop 'cano2 0 ' angle))
    (t (putprop 'cano2 (- (get 'cano2 'angle) 1) ' angle))))

(defun dec-angle-dre ()
    "decrementa l'angle"
    (cond 
    ((< 180 (get 'cano2 'angle)) (putprop 'cano2 180 ' angle))
    ((> 0 (get 'cano2 'angle)) (putprop 'cano2 0 ' angle))
    (t (putprop 'cano2 (+ (get 'cano2 'angle) 1) ' angle))))


(defun mou1-esq ()
    (cond 
    ((> 0 (get 'cano1 'posicio)) (putprop 'cano1 0 ' posicio))
    (t (putprop 'cano1 (- (get 'cano1 'posicio) 1) ' posicio))
))

(defun mou1-dre ()
    (cond
    ((< (- (get 'escenari 'camp1amp) 20) (get 'cano1 'posicio)) 
        (putprop 'cano1 (- (get 'escenari 'camp1amp) 20) ' posicio))
    (t (putprop 'cano1 (+ (get 'cano1 'posicio) 1) ' posicio))
))

(defun mou2-esq ()
    (cond 
    ((> (+ (get 'escenari 'camp1amp) (get 'escenari 'muramp)) 
    (get 'cano2 'posicio)) 
    (putprop 'cano2 (+ (get 'escenari 'camp1amp) 
    (get 'escenari 'muramp)) ' posicio))
    (t (putprop 'cano2 (- (get 'cano2 'posicio) 1) ' posicio))
))


(defun mou2-dre ()
    (cond
    ((< 620 (get 'cano2 'posicio)) 
        (putprop 'cano2 620 ' posicio))
    (t (putprop 'cano2 (+ (get 'cano2 'posicio) 1) ' posicio))
))

(defun repeteix ()
    (pinta)
    (princ "Pitja ESC per acabar.")
    (terpri)
    (cond ((equal (get-key) 119) ; w
           (inc-angle-esq) (repeteix)) ; puja canó esquerra
          ((equal (get-key) 115) ; s
           (dec-angle-esq) (repeteix)) ; baixa canó esquerra


          ((equal (get-key) 105) ; i
           (inc-angle-dre) (repeteix)) ; puja canó esquerra
          ((equal (get-key) 107) ; k
           (dec-angle-dre) (repeteix)) ; baixa canó esquerra

          ((equal (get-key) 97) ; a
           (mou1-esq) (repeteix)) ; mou tank1 esquerra
          ((equal (get-key) 100) ; d
           (mou1-dre) (repeteix)) ; mou tank1 dreta

          ((equal (get-key) 106) ; j
           (mou2-esq) (repeteix)) ; mou tank2 esquerra
          ((equal (get-key) 108) ; l
           (mou2-dre) (repeteix)) ; baixa tank2 dreta

          ((equal (get-key) 27)  ; ESC
           t)                      ; acaba recursió
          (t                 ; altrament
           (repeteix))))           ; repeteix



;q: 113 més potència
;e: 101 menys potència
;f: 102 dispara


;o: 111 més potència
;u: 117 menys potència
;h: 104 dispara

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

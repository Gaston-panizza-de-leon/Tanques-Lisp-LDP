;AUTORS: Gregori Serra Vinogradov, Lucas Gastón Panizza de León


(defun inicia ()
    "inici del programa (tecles)"
    (putprop 'escenari (+ 20 (random 21)) 'muramp)
    (putprop 'escenari (+ 100 (random 51)) 'muralt)

    (putprop 'escenari (+ (- (floor (- 640 (get 'escenari 'muramp)) 2) 20)
    (random 40)) 'camp1amp)

    (putprop 'escenari (- (- 640 (get 'escenari 'muramp)) 
    (get 'escenari 'camp1amp)) 'camp2amp)

    (putprop 'escenari (+ 15 (random 31)) 'camp1alt)
    (putprop 'escenari (+ 15 (random 31)) 'camp2alt)

    (putprop 'escenari 0 'gameover)

    (putprop 'cano1 45 'angle)
    (putprop 'cano2 135 'angle)

    (putprop 'cano1 (+ (floor (get 'escenari 'camp1amp) 3) 
    (random (floor (get 'escenari 'camp1amp) 3)))'posicio)

    (putprop 'cano1 (+ (get 'escenari 'camp1alt) 10) 'altura)

    (putprop 'cano2 (+ (+ (+ (floor (get 'escenari 'camp2amp) 3) 
    (random (floor (get 'escenari 'camp2amp) 3))) (get 'escenari 
    'camp1amp)) (get 'escenari 'muramp)) 'posicio)

    (putprop 'cano2 (+ (get 'escenari 'camp2alt) 10) 'altura)

    (putprop 'cano1 20 'velocitat)
    (putprop 'cano2 20 'velocitat)

    (repeteix))

(defun inc-angle-esq ()
    "incrementa l'angle"
    (cond 
    ((< 180 (get 'cano1 'angle)) (putprop 'cano1 180 ' angle))
    (t (putprop 'cano1 (+ (get 'cano1 'angle) 1) ' angle))))

(defun dec-angle-esq ()
    "decrementa l'angle"
    (cond 
    ((> 0 (get 'cano1 'angle)) (putprop 'cano1 0 ' angle))
    (t (putprop 'cano1 (- (get 'cano1 'angle) 1) ' angle))))

(defun inc-angle-dre ()
    "incrementa l'angle"
    (cond 
    ((> 0 (get 'cano2 'angle)) (putprop 'cano2 0 ' angle))
    (t (putprop 'cano2 (- (get 'cano2 'angle) 1) ' angle))))

(defun dec-angle-dre ()
    "decrementa l'angle"
    (cond 
    ((< 180 (get 'cano2 'angle)) (putprop 'cano2 180 ' angle))
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

(defun mespotencia-esq ()
    (cond 
    ((< 100 (get 'cano1 'velocitat)) (putprop 'cano1 100 ' velocitat))
    (t (putprop 'cano1 (+ (get 'cano1 'velocitat) 2) ' velocitat))
))

(defun menyspotencia-esq ()
    (cond 
    ((> 10 (get 'cano1 'velocitat)) (putprop 'cano1 10 ' velocitat))
    (t (putprop 'cano1 (- (get 'cano1 'velocitat) 2) ' velocitat))
))

(defun mespotencia-dre ()
    (cond 
    ((< 100 (get 'cano2 'velocitat)) (putprop 'cano2 100 ' velocitat))
    (t (putprop 'cano2 (+ (get 'cano2 'velocitat) 2) ' velocitat))
))

(defun menyspotencia-dre ()
    (cond 
    ((> 10 (get 'cano2 'velocitat)) (putprop 'cano2 10 ' velocitat))
    (t (putprop 'cano2 (- (get 'cano2 'velocitat) 2) ' velocitat))
))

;Dispara el canó de la esquerra
;Si impacta l'altre canó es termina la partida
;Si impacta altre objecte deixa de pintar
;Si no impacta res, es continua dibuixant la trajectòria
(defun dispara-esq (temps) (sleep 0.0001) (cond 

;Si impacta amb el primer camp = nil
((and (< (coordx 'cano1 (get 'cano1 'velocitat) temps) 
         (get 'escenari 'camp1amp))
      (< (coordy 'cano1 (get 'cano1 'velocitat) temps) 
         (get 'escenari 'camp1alt))) nil)

;Si impacta amb el mur = nil
((and (and (> (coordx 'cano1 (get 'cano1 'velocitat) temps) 
         (get 'escenari 'camp1amp))
      (< (coordx 'cano1 (get 'cano1 'velocitat) temps) 
         (+ (get 'escenari 'camp1amp) (get 'escenari 'muramp)))) 
        (< (coordy 'cano1 (get 'cano1 'velocitat) temps) 
        (get 'escenari 'muralt)))nil)

;Si impacta amb el segon camp = nil
((and (> (coordx 'cano1 (get 'cano1 'velocitat) temps) 
         (+ (get 'escenari 'camp1amp) (get 'escenari 'muramp)))
      (< (coordy 'cano1 (get 'cano1 'velocitat) temps) 
         (get 'escenari 'camp2alt))) nil)

;Si surt de la pantalla = nil
((< (coordx 'cano1 (get 'cano1 'velocitat) temps) 0) nil)
((> (coordx 'cano1 (get 'cano1 'velocitat) temps) 640) nil)
((> (coordy 'cano1 (get 'cano1 'velocitat) temps) 340) nil)

;Impacta canó, finalitza el joc
((and (and (< (get 'cano2 'posicio) 
    (coordx 'cano1 (get 'cano1 'velocitat) temps))
    (< (coordx 'cano1 (get 'cano1 'velocitat) temps) 
    (+ 20 (get 'cano2 'posicio))))

    (and (> (+ (get 'escenari 'camp2alt) 10) 
    (coordy 'cano1 (get 'cano1 'velocitat) temps)) 
    (> (coordy 'cano1 (get 'cano1 'velocitat) temps) 
    (get 'escenari 'camp2alt)))) 
    (putprop 'escenari 1 'gameover))

;No impacta segueix dibuixant 
(t (drawr (coordx 'cano1 (get 'cano1 'velocitat) temps) 
          (coordy 'cano1 (get 'cano1 'velocitat) temps))
          (dispara-esq  (+  0.05 temps)))))


;Dispara el canó de la dreta 
;Si impacta l'altre canó es termina la partida
;Si impacta altre objecte deixa de pintar
;Si no impacta res, es continua dibuixant la trajectòria
(defun dispara-dre (temps)(sleep 0.0001)(cond 

;Si impacta amb el primer camp = nil
((and (< (coordx 'cano2 (get 'cano2 'velocitat) temps) 
         (get 'escenari 'camp1amp))
      (< (coordy 'cano2 (get 'cano2 'velocitat) temps) 
         (get 'escenari 'camp1alt))) nil)

;Si impacta amb el mur = nil
((and (and (> (coordx 'cano2 (get 'cano2 'velocitat) temps) 
         (get 'escenari 'camp1amp))
      (< (coordx 'cano2 (get 'cano2 'velocitat) temps) 
         (+ (get 'escenari 'camp1amp) (get 'escenari 'muramp)))) 
        (< (coordy 'cano2 (get 'cano2 'velocitat) temps) 
        (get 'escenari 'muralt)))nil)

;Si impacta amb el segon camp = nil
((and (> (coordx 'cano2 (get 'cano2 'velocitat) temps) 
         (+ (get 'escenari 'camp1amp) (get 'escenari 'muramp)))
      (< (coordy 'cano2 (get 'cano2 'velocitat) temps) 
         (get 'escenari 'camp2alt))) nil)

;Si surt de la pantalla = nil
((< (coordx 'cano2 (get 'cano2 'velocitat) temps) 0) nil)
((> (coordx 'cano2 (get 'cano2 'velocitat) temps) 640) nil)
((> (coordy 'cano2 (get 'cano2 'velocitat) temps) 340) nil)


;Impacta canó, finalitza el joc
((and (and (< (get 'cano1 'posicio) 
    (coordx 'cano2 (get 'cano2 'velocitat) temps))
    (< (coordx 'cano2 (get 'cano2 'velocitat) temps) 
    (+ 20 (get 'cano1 'posicio))))

    (and (> (+ (get 'escenari 'camp1alt) 10) 
    (coordy 'cano2 (get 'cano2 'velocitat) temps)) 
    (> (coordy 'cano2 (get 'cano2 'velocitat) temps) 
    (get 'escenari 'camp1alt)))) 
    (putprop 'escenari 1 'gameover))
;No impacta segueix dibuixant 
(t (drawr (coordx 'cano2 (get 'cano2 'velocitat) temps)
          (coordy 'cano2 (get 'cano2 'velocitat) temps))
          (dispara-dre  (+  0.05 temps)))))

;Funcions coordinades
; X = (V*cosa)*T+x0
; Y = (V*sena)*T+1/2*a*t^2

(defun coordx (cano velocitat temps) 
    (+ (* (* velocitat (cos (radians (get cano 'angle)))) temps) 
    (+ (get cano 'posicio) 10)))
(defun coordy (cano velocitat temps)
    (+ (+ (* (* velocitat (sin (radians (get cano 'angle)))) temps)
    (* 0.5(* -9.8 (* temps temps)))) (get cano 'altura)))


(defun repeteix ()
    (pinta)
    (princ "Pitja ESC per acabar.")
    (terpri)
    (cond  ((equal (get 'escenari 'gameover) 1) 
            (color 255 0 0)
            "GAME OVER") 
    
          ((equal (get-key) 119) ; w
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

          ((equal (get-key) 101) ; e
           (mespotencia-esq) (repeteix)) ; més potència canó esquerra
          ((equal (get-key) 113) ; q
           (menyspotencia-esq) (repeteix)) ; menys potència canó esquerra

          ((equal (get-key) 117) ; u
           (mespotencia-dre) (repeteix)) ; més potència canó dreta
          ((equal (get-key) 111) ; o
           (menyspotencia-dre) (repeteix)) ; menys potència canó dreta

           ((equal (get-key) 102) ; f
           (move (+ 10 (get 'cano1 'posicio)) (get 'cano1 'altura))
           (dispara-esq 0) (repeteix)) ; dispara canó esquerra

          ((equal (get-key) 104) ; h
           (move (+ 10 (get 'cano2 'posicio)) (get 'cano2 'altura)) 
           (dispara-dre 0) (repeteix)) ; dispara canó dreta
           

          ((equal (get-key) 27)  ; ESC
           t)                      ; acaba recursió
          (t                 ; altrament
           (repeteix))))           ; repeteix



(defun pinta ()
    (cls)
    (color 0 0 0)
    (rectangle 0 0 639 339)

    ;CAMP1 CANO1
    (color 0 128 0)
    (rectangle 0 0 (get 'escenari 'camp1amp) (get 'escenari 'camp1alt))

    (rectangle (get 'cano1 'posicio) (get 'escenari 'camp1alt) 20 10)

    (angle (+ (get 'cano1 'posicio) 10) (+ (get 'escenari 'camp1alt) 10)
    (get 'cano1 'velocitat) (get 'cano1 'angle))

    ;CAMP2 CANO2
    (color 255 0 0)
    (rectangle (+ (get 'escenari 'camp1amp) (get 'escenari 'muramp)) 0 
    (get 'escenari 'camp2amp) (get 'escenari 'camp2alt))
 
    (rectangle (get 'cano2 'posicio) (get 'escenari 'camp2alt) 20 10)

    (angle (+ (get 'cano2 'posicio) 10) (+ (get 'escenari 'camp2alt) 10)
    (get 'cano2 'velocitat) (get 'cano2 'angle))

    ;MUR
    (color 0 0 0)
    (rectangle (get 'escenari 'camp1amp) 0 (get 'escenari 'muramp) 
    (get 'escenari 'muralt))
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

(defun drawr (x y)
  "pinta a les coordenades arrodonides"
  (draw (round x) 
        (round y)))

(defun radians (graus)
  (/ (* graus (* 2 pi)) 360))

(defun sleep (seconds)
    "Espera la quantitat indicada de segons"
    ; Això és un bucle iteratiu. NO PODEU FER-LO SERVIR ENLLOC MÉS
    (do ((endtime (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
        ((> (get-internal-real-time) endtime))))

(inicia)

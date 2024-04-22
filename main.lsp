;AUTORS: Gregori Serra Vinogradov, Lucas Gastón Panizza de León

;Iniciam les propietats dels símbols escenari cano1 i cano2
;i iniciam el bucle (repeteix)
(defun inicia ()
    ;Amplada i altura mur
    (putprop 'escenari (+ 20 (random 21)) 'muramp)
    (putprop 'escenari (+ 100 (random 51)) 'muralt)

    ;Amplada i altura camp1
    ;L'amplada del primer camp és igual a un valor random entre  
    ;amplada total-amplada del mur/2 -20 i +20.
    ;Això es fa així perquè la diferència màxima amb l'altre camp 
    ;ha de ser de 40 píxels.
    (putprop 'escenari (+ (- (floor (- 640 (get 'escenari 'muramp)) 2) 20)
    (random 40)) 'camp1amp)

    (putprop 'escenari (+ 15 (random 31)) 'camp1alt)

    ;Amplada i altura camp2
    ;L'amplada del camp2 és l'amplada restant
    (putprop 'escenari (- (- 640 (get 'escenari 'muramp)) 
    (get 'escenari 'camp1amp)) 'camp2amp)

    (putprop 'escenari (+ 15 (random 31)) 'camp2alt)

    ;Angle, posició, altura, velocitat i resitència cano1
    (putprop 'cano1 45 'angle)

    ;La posició horitzontal és un valor random dins del segon terç
    ;del primer camp
    (putprop 'cano1 (+ (floor (get 'escenari 'camp1amp) 3) 
    (random (floor (get 'escenari 'camp1amp) 3)))'posicio)

    (putprop 'cano1 (+ (get 'escenari 'camp1alt) 10) 'altura)

    (putprop 'cano1 20 'velocitat)

    (putprop 'cano1 3 'resistencia)

    ;Angle, posició, altura, velocitat i resistència cano2
    (putprop 'cano2 135 'angle) ;180-45

    ;La posició horitzontal és un valor random dins del segon terç
    ;del segon camp
    (putprop 'cano2 (+ (+ (+ (floor (get 'escenari 'camp2amp) 3) 
    (random (floor (get 'escenari 'camp2amp) 3))) (get 'escenari 
    'camp1amp)) (get 'escenari 'muramp)) 'posicio)

    (putprop 'cano2 (+ (get 'escenari 'camp2alt) 10) 'altura)

    (putprop 'cano2 20 'velocitat)
    
    (putprop 'cano2 3 'resistencia)

    ;Força i direcció vent
    (putprop 'escenari (- 5 (random 10)) 'vent)

    (repeteix))

;Incrementa angle canó esquerra i comprova si angle és major a 180
(defun inc-angle-esq ()
    (cond 
    ((< 180 (get 'cano1 'angle)) (putprop 'cano1 180 ' angle))
    (t (putprop 'cano1 (+ (get 'cano1 'angle) 1) ' angle))))

;Decrementa angle canó esquerra i comprova si angle és menor a 0
(defun dec-angle-esq ()
    (cond 
    ((> 0 (get 'cano1 'angle)) (putprop 'cano1 0 ' angle))
    (t (putprop 'cano1 (- (get 'cano1 'angle) 1) ' angle))))

;Incrementa angle canó dreta i comprova si angle és major a 180
(defun inc-angle-dre ()
    (cond 
    ((< 180 (get 'cano2 'angle)) (putprop 'cano2 180 ' angle))
    (t (putprop 'cano2 (+ (get 'cano2 'angle) 1) ' angle))))


;Decrementa angle canó dreta i comprova si angle és menor a 0
(defun dec-angle-dre ()
    (cond 
    ((> 0 (get 'cano2 'angle)) (putprop 'cano2 0 ' angle))
    (t (putprop 'cano2 (- (get 'cano2 'angle) 1) ' angle))))

;Mou 1 píxel a l'esquerra canó esquerra i comprova que no surti 
;del camp esquerra
(defun mou1-esq ()
    (cond 
    ((> 0 (get 'cano1 'posicio)) (putprop 'cano1 0 ' posicio))
    (t (putprop 'cano1 (- (get 'cano1 'posicio) 1) ' posicio))
))

;Mou 1 píxel a la dreta canó esquerra i comprova que no surti 
;del camp esquerra
(defun mou1-dre ()
    (cond
    ((< (- (get 'escenari 'camp1amp) 20) (get 'cano1 'posicio)) 
        (putprop 'cano1 (- (get 'escenari 'camp1amp) 20) ' posicio))
    (t (putprop 'cano1 (+ (get 'cano1 'posicio) 1) ' posicio))
))

;Mou 1 píxel a l'esquerra canó dreta i comprova que no surti 
;del camp dret
(defun mou2-esq ()
    (cond 
    ((> (+ (get 'escenari 'camp1amp) (get 'escenari 'muramp)) 
    (get 'cano2 'posicio)) 
    (putprop 'cano2 (+ (get 'escenari 'camp1amp) 
    (get 'escenari 'muramp)) ' posicio))
    (t (putprop 'cano2 (- (get 'cano2 'posicio) 1) ' posicio))
))

;Mou 1 píxel a la dreta canó dreta i comprova que no surti 
;del camp dret

(defun mou2-dre ()
    (cond
    ((< 620 (get 'cano2 'posicio)) 
        (putprop 'cano2 620 ' posicio))
    (t (putprop 'cano2 (+ (get 'cano2 'posicio) 1) ' posicio))
))

;Incrementa en 2 potència canó esquerra amb límit superior 100
(defun mespotencia-esq ()
    (cond 
    ((< 100 (get 'cano1 'velocitat)) (putprop 'cano1 100 ' velocitat))
    (t (putprop 'cano1 (+ (get 'cano1 'velocitat) 2) ' velocitat))
))

;Decrementa en 2 potència canó dreta amb límit inferior 10
(defun menyspotencia-esq ()
    (cond 
    ((> 10 (get 'cano1 'velocitat)) (putprop 'cano1 10 ' velocitat))
    (t (putprop 'cano1 (- (get 'cano1 'velocitat) 2) ' velocitat))
))

;Incrementa en 2 potència canó dreta amb límit superior 100
(defun mespotencia-dre ()
    (cond 
    ((< 100 (get 'cano2 'velocitat)) (putprop 'cano2 100 ' velocitat))
    (t (putprop 'cano2 (+ (get 'cano2 'velocitat) 2) ' velocitat))
))

;Decrementa en 2 potència canó dreta amb límit inferior 10
(defun menyspotencia-dre ()
    (cond 
    ((> 10 (get 'cano2 'velocitat)) (putprop 'cano2 10 ' velocitat))
    (t (putprop 'cano2 (- (get 'cano2 'velocitat) 2) ' velocitat))
))

;Dispara el canó de la esquerra
;Si impacta l'altre canó baixa resistència dreta
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
    (putprop 'cano2 (- (get 'cano2 'resistencia) 1) 'resistencia))

;No impacta segueix dibuixant 
(t (drawr (coordx 'cano1 (get 'cano1 'velocitat) temps) 
          (coordy 'cano1 (get 'cano1 'velocitat) temps))
          (dispara-esq  (+  0.05 temps)))))


;Dispara el canó de la dreta 
;Si impacta l'altre canó baixa resistència esquerra
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
    (putprop 'cano1 (- (get 'cano1 'resistencia) 1) 'resistencia))

;No impacta segueix dibuixant 
(t (drawr (coordx 'cano2 (get 'cano2 'velocitat) temps)
          (coordy 'cano2 (get 'cano2 'velocitat) temps))
          (dispara-dre  (+  0.05 temps)))))

;Funcions coordinades

; X = ((V*cosa)+(vent*T))*T+x0
(defun coordx (cano velocitat temps) 
    (+ (* (+ (* velocitat (cos (radians (get cano 'angle)))) 
    (* (get 'escenari 'vent) temps)) temps) 
    (+ (+ (get cano 'posicio) 10) 
    (* velocitat (cos (radians (get cano 'angle)))))))

; Y = (V*sena)*T+1/2*a*t^2+y0
(defun coordy (cano velocitat temps)
    (+ (+ (* (* velocitat (sin (radians (get cano 'angle)))) temps)
    (* 0.5(* -9.8 (* temps temps)))) 
    (+ (get cano 'altura)
    (* velocitat (sin (radians (get cano 'angle)))))))


;Bucle que pinta i escolta al teclat fins que s'acabi el joc
(defun repeteix ()
    (pinta)
    (princ "Pitja ESC per acabar.")
    (terpri)
    ;Si un dels és impactat 3 vegades, es termina la partida
    (cond ((equal (get 'cano2 'resistencia) 0) 
            (color 0 128 0)
            "GAME OVER. GUANYA CANO1") 
    
          ((equal (get 'cano1 'resistencia) 0) 
            (color 255 0 0)
            "GAME OVER. GUANYA CANO2") 
    
          ((equal (get-key) 119) ; w
           (inc-angle-esq) (repeteix)) ; puja canó esquerra
          ((equal (get-key) 115) ; s
           (dec-angle-esq) (repeteix)) ; baixa canó esquerra


          ((equal (get-key) 105) ; i
           (dec-angle-dre) (repeteix)) ; puja canó dreta
          ((equal (get-key) 107) ; k
           (inc-angle-dre) (repeteix)) ; baixa canó dreta

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
           ;Movem punter a la punta del canó esquerra
           (move (round (+ (+ (get 'cano1 'posicio) 10) 
           (* (get 'cano1 'velocitat) (cos (radians (get 'cano1 'angle))))))
           (round (+ (get 'cano1 'altura) (* (get 'cano1 'velocitat) 
           (sin (radians (get 'cano1 'angle)))))))
           (dispara-esq 0) (repeteix)) ; dispara canó esquerra

          ((equal (get-key) 104) ; h
          ;Movem punter a la punta del canó dret
           (move (round (+ (+ (get 'cano2 'posicio) 10) 
           (* (get 'cano2 'velocitat) (cos (radians (get 'cano2 'angle))))))
           (round (+ (get 'cano2 'altura) (* (get 'cano2 'velocitat) 
           (sin (radians (get 'cano2 'angle)))))))
           (dispara-dre 0) (repeteix)) ; dispara canó dreta
           

          ((equal (get-key) 27)  ; ESC
           t)                      ; acaba recursió
          (t                 ; altrament
           (repeteix))))           ; repeteix



;Cada vegada que es crida esborra la pantalla i torna a pintar tots els
;elements del joc
(defun pinta ()  (color 0 0 0 173 216 230)
    (cls)
    (color 0 0 0)
    (move 0 0)
    (rectanglebuit 639 339)

    ;CAMP1
    (color 0 128 0)
    (move 0 0)
    (rectangle (get 'escenari 'camp1amp) (get 'escenari 'camp1alt))

    ;TANK1
    (move (get 'cano1 'posicio) (get 'escenari 'camp1alt))
    (rectangle 20 10)

    ;CANO1
    (angle (+ (get 'cano1 'posicio) 10) (+ (get 'escenari 'camp1alt) 10)
    (get 'cano1 'velocitat) (get 'cano1 'angle))

    ;CAMP2
    (color 255 0 0)
    (move (+ (get 'escenari 'camp1amp) (get 'escenari 'muramp)) 0 )
    (rectangle (get 'escenari 'camp2amp) (get 'escenari 'camp2alt))
    
    ;TANK2
    (move (get 'cano2 'posicio) (get 'escenari 'camp2alt))
    (rectangle 20 10)

    ;CANO2
    (angle (+ (get 'cano2 'posicio) 10) (+ (get 'escenari 'camp2alt) 10)
    (get 'cano2 'velocitat) (get 'cano2 'angle))

    ;MUR
    (color 0 0 0)
    (move (get 'escenari 'camp1amp) 0)
    (rectangle (get 'escenari 'muramp) (get 'escenari 'muralt))

    ;BANDERA VENT
    (viento (get 'escenari 'vent))
)

;Dibuixa angle
(defun angle (x y r angle)
    (move x y)
    (drawr (+ x (* r (cos (radians angle))))
           (+ y (* r (sin (radians angle))))))

;Dibuixa rectangle complet
(defun rectangle (w h) (cond
    ((= h 0) nil)
    (t (drawrel w 0)
    (moverel (- 0 w) 1) 
    (rectangle w (- h 1)))
))

;Dibuixa rectangle buit
(defun rectanglebuit (w h)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h)))

;Dibuixa a les coordenades arredonides
(defun drawr (x y)
  (draw (round x) 
        (round y)))

;Passa a radians l'angle en graus
(defun radians (graus)
  (/ (* graus (* 2 pi)) 360))

;Al acabar de pintar el muro nos movemos al centro de la parte superior,
;dibujamos el palo de la bandera y dependiendo de la fuerza y dirección,
;dibujamos o no la bandera en la dirección que se indica
(defun viento (vent)
(moverel (round (/ (get 'escenari 'muramp) 2)) 0)
(drawrel 0 20)
(cond
((> vent 0) (rectanglebuit 20 10) (fillband vent))
((< vent 0) (rectanglebuit -20 10) (fillband vent))
(t nil)))


;Función que dibuja las lineas de la bandera dependiendo de la fuerza
;Con recursividad va añadiendo linea a linea
(defun fillband (vent) (cond 
((> vent 0) (drawrel 0 10) (moverel 4 -10) (fillband(- vent 1)))
((< vent 0) (drawrel 0 10) (moverel -4 -10) (fillband (+ 1 vent)))
(t nil)
))

(defun sleep (seconds)
    "Espera la quantitat indicada de segons"
    ; Això és un bucle iteratiu. NO PODEU FER-LO SERVIR ENLLOC MÉS
    (do ((endtime (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
        ((> (get-internal-real-time) endtime))))

(inicia)

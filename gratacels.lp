%AUTORS: Gregori Serra Vinogradov
         Lucas Gastón Panizza de León

%Funcio principal, donades 4 valors de vista per cada costat, resol el problema
%dels gratacels, on a cada quadrícula hi ha un edifici amb altura 1-4, però
%a la mateixa columna i fila no es pot repetir aquella altura. A mes les vistes
%han de complir-se des de les diferents posicions dels taulers nomes es poden
%veure una certa quantitat de gratacels. Si a qualque punt de la funció no es 
%compleix una condició es rebutja la opció i continua cercant dins el arbre

ciutats([E0,E1,E2,E3], [W0,W1,W2,W3], [N0,N1,N2,N3], [S0,S1,S2,S3], [F1,F2,F3,F4]) :-

    %Permutam les quatre files del tauler amb valors 1-4.
    permuta([1, 2, 3, 4],F1), permuta([1, 2, 3, 4],F2), permuta([1, 2, 3, 4],F3), permuta([1, 2, 3, 4],F4),

    %Obtenim el valor de cada quadrícula del tauler
    nth0(0,F1,F11),nth0(1,F1,F12),nth0(2,F1,F13),nth0(3,F1,F14),
	nth0(0,F2,F21),nth0(1,F2,F22),nth0(2,F2,F23),nth0(3,F2,F24),
	nth0(0,F3,F31),nth0(1,F3,F32),nth0(2,F3,F33),nth0(3,F3,F34),
	nth0(0,F4,F41),nth0(1,F4,F42),nth0(2,F4,F43),nth0(3,F4,F44),


    %Tenim les files, cream les columnes amb els valors de les quadrícules
    columns([F11, F21, F31, F41], C1),
    columns([F12, F22, F32, F42], C2),
    columns([F13, F23, F33, F43], C3),
    columns([F14, F24, F34, F44], C4),


    %Invertim les files i columnes per poder tractar les vistes esquerra a dreta i sud a nord
    invert(F1, F1I),
    invert(F2, F2I),
    invert(F3, F3I),
    invert(F4, F4I),

    invert(C1, C1I),
    invert(C2, C2I),
    invert(C3, C3I),
    invert(C4, C4I),

    %Cridam a la funcio vista que comprova que es compleixi el valor de vista amb la quantitat
    %de gratacels visibles

    vista(E0,F1), vista(E1,F2), vista(E2,F3), vista(E3,F4),
    vista(W0,F1I), vista(W1,F2I), vista(W2,F3I), vista(W3,F4I),
    vista(N0,C1), vista(N1,C2), vista(N2,C3), vista(N3,C4),
    vista(S0,C1I), vista(S1,C2I), vista(S2,C3I), vista(S3,C4I),

    %Comprovam que amb la solució obtenida es compleix la restricció de no altures repetides
    %dins la mateixa columna. Sabem que a les files no hi ha repetits per com es formen i permuten.
    F11\=F21,F11\=F31,F11\=F41,F21\=F31,F21\=F41,F31\=F41,	%Columna1
    F12\=F22,F12\=F32,F12\=F42,F22\=F32,F22\=F42,F32\=F42,	%Columna2
    F13\=F23,F13\=F33,F13\=F43,F23\=F33,F23\=F43,F33\=F43,	%Columna3
    F14\=F24,F14\=F34,F14\=F44,F24\=F34,F24\=F44,F34\=F44.	%Columna4


%Funcions permuta i insereix per obtenir totes les col·locacions dels nombres 1-4 dels vectors
permuta([],[]).
permuta([X|Y],Z) :- permuta(Y,L),
                    insereix(X,L,Z).

insereix(E,L,[E|L]).
insereix(E,[X|Y],[X|Z]) :- insereix(E,Y,Z).

%Donats 4 elements en una llista, retorna la llista formada
columns([A, B, C, D], [A, B, C, D]).

%Inverteix la llista passada per paràmetre
invert(List, Inverted) :-
    reverse(List, Inverted).


%Donat un valor de vista, comprova quantes vegades dins la llista actualitzam el valor maxim
%I comprova si son iguals. Si no ho son, el valor vista i el contador de actualizacions, es 
%rebutja la opció
vista(ValorVista, Lista) :-
    contar_max_aux(Lista, 0, 0, Contador),
    Contador = ValorVista.

%Cas base, la llista es buida i retorna el valor del contador
contar_max_aux([], _, Contador, Contador).

%Cas on el valor que esteim mirant, el primer del array, es major al maxim actual. 
%Aumentam comptador i actualitzam maxim
contar_max_aux([H|T], MaxActual, ContadorActual, ContadorFinal) :-
    H > MaxActual,
    NuevoContador is ContadorActual + 1,
    contar_max_aux(T, H, NuevoContador, ContadorFinal).

%Cas on el valor que esteim mirant, el primer del array, es menor al maxim actual. 
%No aumentam comptador i no actualitzam maxim 
contar_max_aux([H|T], MaxActual, ContadorActual, ContadorFinal) :-
    H =< MaxActual,
    contar_max_aux(T, MaxActual, ContadorActual, ContadorFinal).

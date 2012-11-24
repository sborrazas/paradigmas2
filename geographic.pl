:- module(geographic, [distance/3,inside/3,near/2,cross/5,corner/3]).

:- use_module(tramo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% distance((+X1, +Y1), (+X2, +Y2), ?D)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Este predicado calcula la distancia cartesiana D entre las coordenadas X e Y.
% Estas coordenadas son pares ordenados de reales correspondientes a la latitud y longitud.

distance((X1, Y1), (X2, Y2), D) :- DistX = (X1 - X2) ** 2, DistY = (Y1 - Y2) ** 2, D is sqrt(DistY + DistX).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inside(+X, +Y, +Z)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se cumple si la coordenada Z esta dentro del segmento definido por las coordenadas X e Y
% Considerar un error del 5 ∗ 10−14 debido al redondeo).
inside(P1, P2, P3) :- distance(P1, P2, D1), distance(P1, P3, D2), distance(P2, P3, D3), Diff = D2 + D3 - D1, Diff < (5 * (10 ** -14)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% near(+A, +B)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se cumple si las coordenadas X e Y se consideran cercanas, esto es, si estan a una distancia
% menor o igual a 5*10-14
near(P1, P2) :- distance(P1, P2, Dist), Dist =< (5 * (10 ** -14)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cross (+X1, +X2, +Y1, +Y2, ?Z)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se cumple si los segmentos determinados por las coordenadas X1,X2 e Y1,Y2 se intersectan en la coordenada Z.
cross((X1, _), (X1, _), (X3, Y3), (X4, Y4), (Zx, Zy)) :- X3 \= X4,
                                                         pendiente((X3, Y3), (X4, Y4), M2), % R1 paralela a eje y
                                                         Zx is X1,
                                                         Zy is M2 * Zx - M2 * X3 + Y3, !.
cross((X1, Y1), (X2, Y2), (X3, _), (X3, _), (Zx, Zy)) :- X1 \= X2,
                                                         pendiente((X1, Y1), (X2, Y2), M1), % R2 paralela a eje y
                                                         Zx is X3,
                                                         Zy is M1 * Zx - M1 * X1 + Y1, !.
cross((X1, Y1), (X2, Y2), (X3, Y3), (X4, Y4), (Zx, Zy)) :- X1 \= X2, X3 \= X4, % Ninguna paralela a eje y
                                                           pendiente((X1, Y1), (X2, Y2), M1),
                                                           pendiente((X3, Y3), (X4, Y4), M2),
                                                           M1 \= M2,
                                                           Zx is (M1 * X1 - Y1 - M2 * X3 + Y3) / (M1 - M2),
                                                           Zy is M1 * Zx - M1 * X1 + Y1.

pendiente((X1, Y1), (X2, Y2), M) :- M is (Y2 - Y1) / (X2 - X1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% corner (+C1, +C2, ?X)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se cumple si X es una coordenada cercana a la esquina formada por las calles C1 y C2.
% Esta condicion se cumple si se satisface alguna de las siguientes condiciones:
% - Existen dos tramos, correspondientes a las calles C1 y C2 respectivamente, que se intersectan en X
% - Existen dos extremos de tramos cercanos, correspondientes a las calles C1 y C2 respectivamente
corner(C1, C2, X) :- tramo(C1, P1, P2), tramo(C2, P3, P4), cross(P1, P2, P3, P4, X), !.
corner(C1, C2, X) :- tramo(C1, P1, _), tramo(C2, P3, _), near(P1, X), near(P3, X), !.
corner(C1, C2, X) :- tramo(C1, P1, _), tramo(C2, _, P4), near(P1, X), near(P4, X), !.
corner(C1, C2, X) :- tramo(C1, _, P2), tramo(C2, P3, _), near(P2, X), near(P3, X), !.
corner(C1, C2, X) :- tramo(C1, _, P2), tramo(C2, _, P4), near(P2, X), near(P4, X), !.

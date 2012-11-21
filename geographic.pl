:- module(geographic, [distance/3,inside/3,near/2,cross/5,corner/3]).

:-use_module(tramo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% distance((+X1, +Y1), (+X2, +Y2),?D)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Este predicado calcula la distancia cartesiana D entre las coordenadas X e Y.
% Estas coordenadas son pares ordenados de reales correspondientes a la latitud y longitud.

distance((X1, Y1), (X2, Y2), D) :- DistX = (X1 - X2) ** 2, DistY = (Y1 - Y2) ** 2, D is sqrt(DistY + DistX).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inside(+X,+Y,+Z)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se cumple si la coordenada Z esta dentro del segmento definido por las coordenadas X e Y
% Considerar un error del 5 ∗ 10−14 debido al redondeo).
inside(P1, P2, P3) :- distance(P1, P2, D1), distance(P1, P3, D2), distance(P2, P3, D3), Diff = D2 + D3 - D1, Diff < (5 * (10 ** -14)).

%near/2
near(A, B).

%cross/5
cross(A, B, C, D, E).

%corner/3
corner(A, B, C).

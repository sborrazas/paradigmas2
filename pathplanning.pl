:- module(pathplanning, [adyacentes/2,caminoHC/5,caminoAmp/5,caminoAAst/5]).

:- use_module(tramo).
:- use_module(geographic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adyacentes(+X, ?Y)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dada una coordenada X, este predicado se cumple si se satisface alguna de las siguientes condiciones:
% - Existe una coordenada Z que esta cerca de X, y hay un tramo de Y a Z o de Z a Y,
% - X esta adentro de un tramo de Y a Z o de Z a Y, y X no esta cerca de Y ni de Z.
adyacentes(X, Y) :- tramo(_, Y, Z), near(X, Z).
adyacentes(X, Y) :- tramo(_, Z, Y), near(X, Z).
adyacentes(X, Y) :- tramo(_, Y, Z), inside(Y, Z, X), not(near(X, Y)), not(near(X, Z)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminoHC(+C1, +C2, +C3, +C4, ?P)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se cumple si P es una lista de coordenadas con un camino desde la esquina formada por las calles C1 y C2,
% y la esquina formada por C3 y C4. Esta busqueda debera realizarse utilizando el metodo de 'Hill Climbing'
% visto en el teorico.

caminoHC(C1, C2, C3, C4, [E1|Recorrido]) :- corner(C1, C2, E1), corner(C3, C4, E2), conectados(E1, E2, [], Recorrido).

conectados(P1, P2, _, []) :- adyacentes(P1, P2).
conectados(P1, P2, PuntosVs, [NuevoPunto|Coords]) :-
  puntosValidosOrd(P1, NuevoPunto, P2), % Puntos a los que puedo ir desde P1
  not(member(NuevoPunto, PuntosVs)), % Se fija que aun no pase por ese punto
  conectados(NuevoPunto, P2, [NuevoPunto|PuntosVs], Coords). % Llama a conectados ahora para ir desde NuevoPunto a P2
                                                            % sin pasar por los puntos recorridos y cargando el resto
                                                            % de los puntos por los que se va a pasar en Coords

% movValOrd devuelve los movimientos en orden (segun el criterio deseado)
puntosValidosOrd(Desde, Punto, Destino) :-
  findall((Costo, NuevoPunto), (
    movVal(Desde, NuevoPunto),
    costo(NuevoPunto, Destino, Costo)
  ), Puntos),
  sort(Puntos, PuntosOrd),
  member((_, Punto), PuntosOrd).

% movVal = adyacentes
movVal(Desde, Hasta) :- adyacentes(Desde, Hasta).

% costo = distance
costo(Punto, Destino, Costo) :- distance(Punto, Destino, Costo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminoAmp/5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
caminoAmp(A, B, C, D, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminoAAst/5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
caminoAAst(A, B, C, D, E).

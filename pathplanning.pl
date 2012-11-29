:- module(pathplanning, [adyacentes/2,caminoHC/5,caminoAmp/5,caminoAAst/5]).

:- use_module(tramo).
:- use_module(geographic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adyacentes(+X, ?Y)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dada una coordenada X, este predicado se cumple si se satisface alguna de las siguientes condiciones:
% - Existe una coordenada Z que esta cerca de X, y hay un tramo de Y a Z o de Z a Y,
% - X esta adentro de un tramo de Y a Z o de Z a Y, y X no esta cerca de Y ni de Z.
adyacentes(P1, P2) :- tramo(P2, Z), near(P1, Z), !.
adyacentes(P1, P2) :- tramo(Z, P2), near(P1, Z), !.
adyacentes(P1, P2) :- tramo(P2, Z), inside(P2, Z, P1).
adyacentes(P1, P2) :- tramo(Z, P2), inside(P2, Z, P1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminoHC(+C1, +C2, +C3, +C4, ?P)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se cumple si P es una lista de coordenadas con un camino desde la esquina formada por las calles C1 y C2,
% y la esquina formada por C3 y C4. Esta busqueda debera realizarse utilizando el metodo de 'Hill Climbing'
% visto en el teorico.

% DEFINO EL TRAMO COMO ((Xdesde, Ydesde), (Xhasta, Yhasta))
% DEFINO EL PUNTO COMO (X, Y)

caminoHC(C1, C2, C3, C4, Recorrido) :- corner(C1, C2, E1), corner(C3, C4, E2), conectados(E1, E2, [], Recorrido).

conectados(P1, P2, _, []) :- near(P1, P2).
conectados(P1, P2, TramosVs, [NuevoPunto|Coords]) :-
  tramosValidosOrd(P1, NuevoTramo, P2), % Lista con las coordenadas a las que puedo ir desde P1
  nuevoPunto(P1, NuevoTramo, NuevoPunto), % Calcula el nuevo punto desde el que se sale
  not(member(NuevoTramo, TramosVs)), % Se fija que aun no pase por esa calle
  conectados(NuevoPunto, P2, [NuevoTramo|TramosVs], Coords). % Llama a conectados ahora para ir desde NuevoPunto a P2
                                                            % sin pasar por las calles recorridas y cargando el resto
                                                            % de los puntos por los que se va a pasar en Coords

% movValOrd debe devolver los movimientos en orden
% (segun el criterio deseado)
tramosValidosOrd(P, Tramo, Destino) :- findall((Costo, NuevoTramo), (movVal(P, NuevoTramo), costo(NuevoTramo, Destino, Costo)), Tramos),
                              sort(Tramos, TramosOrd),
                              member((_, Tramo), TramosOrd).

movVal(P, NuevoTramo) :- tramo(_, L1, L2), near(L1, P), NuevoTramo = (L1, L2).
movVal(P, NuevoTramo) :- tramo(_, L1, L2), near(L2, P), NuevoTramo = (L1, L2).

costo((Desde, Hasta), Destino, Costo) :- distance(Hasta, Destino, Costo).
nuevoPunto(_, (_, NuevoPunto), NuevoPunto). % Calcula el nuevo punto desde el que se sale

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminoAmp/5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
caminoAmp(A, B, C, D, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminoAAst/5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
caminoAAst(A, B, C, D, E).

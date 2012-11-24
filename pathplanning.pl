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
caminoHC(A, B, C, D, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%caminoAmp/5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
caminoAmp(A, B, C, D, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%caminoAAst/5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
caminoAAst(A, B, C, D, E).

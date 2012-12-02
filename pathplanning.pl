:- module(pathplanning, [adyacentes/2,caminoHC/5,caminoAmp/5,caminoAAst/5]).

:- use_module(tramo).
:- use_module(geographic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adyacentes(+X, ?Y)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dada una coordenada X, este predicado se cumple si se satisface alguna de las siguientes condiciones:
% - Existe una coordenada Z que esta cerca de X, y hay un tramo de Y a Z o de Z a Y,
% - X esta adentro de un tramo de Y a Z o de Z a Y, y X no esta cerca de Y ni de Z.
adyacentes(X, Y) :- tramo(_, Y, Z), inside(Y, Z, X), not(near(X, Y)), not(near(X, Z)).
adyacentes(X, Y) :- tramo(_, Y, Z), near(X, Z).
adyacentes(X, Y) :- tramo(_, Z, Y), near(X, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% movVal = adyacentes
movVal(Desde, Hasta) :- adyacentes(Desde, Hasta).

% lista con todos los nuevos puntos a los que puedo ir
movValidos(Desde, Puntos) :- findall(Hasta, movVal(Desde, Hasta), Puntos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminoHC(+C1, +C2, +C3, +C4, ?Recorrido)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se cumple si P es una lista de coordenadas con un camino desde la esquina formada por las calles C1 y C2,
% y la esquina formada por C3 y C4. Esta busqueda debera realizarse utilizando el metodo de 'Hill Climbing'
% visto en el teorico.
caminoHC(C1, C2, C3, C4, [E1|Recorrido]) :-
  corner(C1, C2, E1),
  corner(C3, C4, E2),
  conectadosHC(E1, E2, [], Recorrido).

conectadosHC(P1, P2, _, []) :- movVal(P1, P2).
conectadosHC(P1, P2, PuntosVs, [NuevoPunto|Coords]) :-
  puntoValidoOrd(P1, P2, NuevoPunto), % Puntos a los que puedo ir desde P1
  not(member(NuevoPunto, PuntosVs)), % Se fija que aun no pase por ese punto
  conectadosHC(NuevoPunto, P2, [NuevoPunto|PuntosVs], Coords). % Llama a conectadosHC ahora para ir desde NuevoPunto a P2
                                                            % sin pasar por los puntos recorridos y cargando el resto
                                                            % de los puntos por los que se va a pasar en Coords

% lista con todos los nuevos puntos ordenados por costo
puntoValidoOrd(Desde, DestinoFinal, Punto) :- findall((Costo, NuevoPunto), (
    movVal(Desde, NuevoPunto),
    costoHC(NuevoPunto, DestinoFinal, Costo)
  ), Puntos),
  sort(Puntos, PuntosOrd),
  member((_, Punto), PuntosOrd).

% costoHC = distancia al destino (criterio elegido para HC)
costoHC(Punto, Destino, Costo) :- distance(Punto, Destino, Costo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminoAmp(+C1, +C2, +C3, +C4, ?Recorrido)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado equivalente al anterior pero haciendo una busqueda en amplitud en el espacio de
% busqueda.
caminoAmp(C1, C2, C3, C4, Recorrido) :-
  corner(C1, C2, E1),
  corner(C3, C4, E2),
  conectadosAmp([(0, [E1])], E2, Recorrido).

conectadosAmp([(_, [UltimoPunto|RestoPuntos])|_], DestinoFinal, CaminoFinal) :-
  near(UltimoPunto, DestinoFinal),
  CaminoFinal = [DestinoFinal|[UltimoPunto|RestoPuntos]].
conectadosAmp([(Costo, [UltimoPunto|RestoPuntos])|RestoCaminos], DestinoFinal, Recorrido) :-
  movValidos(UltimoPunto, Hijos),
  subtract(Hijos, RestoPuntos, HijosSinRepetir), % Leve optimizacion para no volver para atras.
  expandir(Costo, [UltimoPunto|RestoPuntos], HijosSinRepetir, NCaminos),
  append(RestoCaminos, NCaminos, NEspacio),
  sort(NEspacio, NEspacioOrd), % Con variante de BEST FIRST
  conectadosAmp(NEspacioOrd, DestinoFinal, Recorrido).

% Crea una nueva lista de caminos con los caminos del primer parametro agregando cada elemento a la segunda lista
expandir(_, _, [], []).
expandir(CostoAntes, [UltimoPunto|RestoPuntos], [Hijo|RHijos], [(Costo, [Hijo|[UltimoPunto|RestoPuntos]])|R]) :-
  costoAmp(CostoAntes, UltimoPunto, Hijo, Costo),
  expandir(CostoAntes, [UltimoPunto|RestoPuntos], RHijos, R).

% costo = largo del camino recorrido
costoAmp(CostoAntes, UltimoPunto, NuevoPunto, Costo) :-
  distance(UltimoPunto, NuevoPunto, DistEntrePuntos),
  Costo is CostoAntes + DistEntrePuntos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminoAAst(+C1, +C2, +C3, +C4, ?Recorrido)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado equivalente al anterior pero donde la busqueda se realiza utilizando el algoritmo A*.
% Para la estimacion heuristica se debera usar la distancia en linea recta al destino.
caminoAAst(C1, C2, C3, C4, Recorrido) :-
  corner(C1, C2, E1),
  corner(C3, C4, E2),
  distance(E1, E2, Dist),
  conectadosAAst([(Dist, [E1])], E2, Recorrido).

conectadosAAst([(_, [UltimoPunto|RestoPuntos])|_], DestinoFinal, CaminoFinal) :-
  near(UltimoPunto, DestinoFinal),
  CaminoFinal = [DestinoFinal|[UltimoPunto|RestoPuntos]].

conectadosAAst([(_, [Ultimo|CaminoC])|RestoCaminos], DestinoFinal, CaminoFinal) :-
  movValidos(Ultimo, Hijos),
  expandirConCosto([Ultimo|CaminoC], Hijos, NCaminos, DestinoFinal),
  append(RestoCaminos, NCaminos, NEspacio),
  sort(NEspacio, NEspacioOrd),
  conectadosAAst(NEspacioOrd, DestinoFinal, CaminoFinal).

% Crea una nueva lista de caminos con los caminos del primer parametro agregando cada elemento a la segunda lista
expandirConCosto(_, [], [], _).
expandirConCosto([UltimoPunto|RCamino], [Hijo|RHijos], [(Costo, [Hijo|[UltimoPunto|RCamino]])|R], DestinoFinal) :-
  costoAAst([UltimoPunto|RCamino], Hijo, DestinoFinal, Costo),
  expandirConCosto([UltimoPunto|RCamino], RHijos, R, DestinoFinal).

% costo = costoRecorrido + costo de llegar al nuevo punto + costo de llegar al destino directamente
costoAAst([UltimoPunto|RCamino], NuevoPunto, DestinoFinal, Costo) :-
  costoRecorrido([UltimoPunto|RCamino], CostoRecorrido),
  distance(UltimoPunto, NuevoPunto, CostoNuevo),
  distance(NuevoPunto, DestinoFinal, CostoAlFinal),
  Costo is CostoRecorrido + CostoNuevo + CostoAlFinal.

costoRecorrido([_], 0).
costoRecorrido([X,Y|Resto], Costo) :- distance(X, Y, Dist), costoRecorrido([Y|Resto], Dist2), Costo is Dist + Dist2.

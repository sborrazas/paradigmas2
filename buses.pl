:- module(buses, [findBusCalles/9,findBusCoor/7]).

:-use_module(recorrido).
:-use_module(geographic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findBusCoor(+O, +D, ?N, ?V, ?Desc, ?PO, ?PD)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se satisface si el omnibus numero N con codigo de sublinea V y descripcion Desc
% tiene el recorrido que minimiza la suma de las distancias formadas por:
% - La coordenada origen O y la coordenada mas cercana a esta en la lista de coordenadas que
%   conforman el recorrido del omnibus
% - La coordenada destino D y la coordenada mas cercana a esta en el recorrido del omnibus
% Ademas PO es el indice de la coordenada en el recorrido mas cercana a O, y PD es el indice en el
% recorrido pero de la coordenada mas cercana a D. Dado que se desea ir desde O hasta D se tiene
% que cumplir que la coordenada mas cercana al origen O tiene que ocurrir antes en la lista
% que conforma el recorrido que la coordenada mas cercana al destino D (o sea, se debe cumplir PO < PD).
findBusCoor(O, D, N, V, Desc, PO, ParadaDestino) :-
  findall(Retorno, calcularRecorridos(O, D, Retorno), Retornos),
  sort(Retornos, [Solucion|_]),
  (_, N, V, Desc, PO, PD) = Solucion,
  ParadaDestino is PO + PD + 1.

calcularRecorridos(O, D, Retorno) :-
  recorrido(N, Desc, V, Paradas),
  paradaMasCercana(O, Paradas, PO),
  nth0(PO, Paradas, ParadaOrigen),
  restoLista(PO, Paradas, RParadas),
  paradaMasCercana(D, RParadas, PD),
  nth0(PD, RParadas, ParadaDestino),
  distance(O, ParadaOrigen, DistOrigen),
  distance(D, ParadaDestino, DistDestino),
  SumaDistancias is DistOrigen + DistDestino,
  Retorno = (SumaDistancias, N, V, Desc, PO, PD).

paradaMasCercana(_, [_], 0).
paradaMasCercana(Punto, [Parada|RParadas], Indice) :-
  distance(Punto, Parada, Dist),
  paradaMasCercana(Punto, RParadas, ProximoIndice),
  nth0(ProximoIndice, RParadas, ProximaMasCercana),
  distance(Punto, ProximaMasCercana, ProximaDistMasCercana),
  ProximoIndiceListaActual is ProximoIndice + 1,
  maxIndice(Dist, 0, ProximaDistMasCercana, ProximoIndiceListaActual, Indice).

maxIndice(Distancia1, Indice1, Distancia2, _, Indice1) :- Distancia1 < Distancia2.
maxIndice(Distancia1, _, Distancia2, Indice2, Indice2) :- Distancia1 >= Distancia2.

restoLista(0, [_|Resto], Resto).
restoLista(Indice, [_|Resto], NuevoResto) :-
  Indice > 0,
  NuevoIndice is Indice - 1,
  restoLista(NuevoIndice, Resto, NuevoResto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findBusCalles(+C1, +C2, +C3, +C4, ?N, ?V, ?Desc, ?PO, ?PD)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado analogo al anterior pero en vez de utilizar las coordenadas origen y destino,
% utiliza los nombres de las calles que conforman la esquina origen C1 y C2, y
% la esquina destino C3 y C4.
findBusCalles(C1, C2, C3, C4, N, V, Desc, Po, Pd) :-
  corner(C1, C2, E1),
  corner(C3, C4, E2),
  findBusCoor(E1, E2, N, V, Desc, Po, Pd).

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
findBusCoor(A, B, C, D, E, F, G).

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

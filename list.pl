:- module(list, [drop/3,sublist/4,sublist/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop(+N,+Xs,?Ys)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Este predicado se satisface si Ys es la lista resultante de seguir el siguiente procedimiento:
% dejar un elemento de la lista Xs y luego saltear los N proximos elementos y volver a repetir
% este proceso hasta que no haya mas elementos en Xs.
% Por ejemplo, drop(2,[1,2,3,4,5,6,7,8],Xs)? da como respuesta Xs = [1,4,7].
% Este predicado es usado en la interfaz grafica para reducir el detalle de recorridos muy
% largos debido a que Google Maps Image tiene limites en el largo de las URLs.

drop(N, Xs, Ys) :-  drop(N, Xs, Ys, N).

drop(_, [], [], _).
drop(N, [X|Xs], [X|Ys], N) :- drop(N, Xs, Ys, 0).
drop(N, [_|Xs], Ys, Cont) :- Cont < N, NuevoCont is Cont + 1, drop(N, Xs, Ys, NuevoCont).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sublist(+Xs, ?Ys, +S, +E)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se satisface si Ys es la lista formada por los elementos de la lista Xs que van desde la posicion S a la posicion E.
% Las posiciones se numeran desde cero, en caso que no alcancen los elementos de la lista, debe devolver los que pueda.
% Por ejemplo, sublist([1,2,3,4],Ys,2,3)? da como respuesta Ys=[3,4].
% Este predicado tambien se usa en la interfaz grafica para obtener parte del recorrido de un omnibus.

sublist(Xs, Ys, S, E) :- sublist(Xs, Ys, S, E, 0).

sublist([], [], _, E, Cont) :- Cont =< E.
sublist([X|Xs], [X|Ys], S, E, Cont) :- S =< Cont, Cont =< E, NuevoCont = Cont + 1, sublist(Xs, Ys, S, E, NuevoCont).
sublist([_|Xs], Ys, S, E, Cont) :- Cont < S, NuevoCont = Cont + 1, sublist(Xs, Ys, S, E, NuevoCont).
sublist(_, [], _, E, Cont) :- Cont > E.

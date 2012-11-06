% Autor:
% Fecha: 29/10/2012

%%%%%%%%%%%%%%%%%%%
%%% Ejercicio 3 %%%
%%%%%%%%%%%%%%%%%%%

ensena(juan, calculoI). % hecho
ensena(esposa(juan), disenoI). % hecho

materia(disenoI, programacion). % hecho
materia(calculoI, matematica). % hecho

inteligente(X) :- ensena(X, Y) , materia(Y, programacion). % regla

% Consultas
% inteligente(esposa(juan))

%%%%%%%%%%%%%%%%%%%
%%% Ejercicio 4 %%%
%%%%%%%%%%%%%%%%%%%

nacio(mary, australia).
nacio(peter, nueva_zelanda).

padre(peter, john).
madre(mary, john).


ciudadanoA(X) :- nacio(X, australia).
ciudadanoA(X) :- padre(Y, X), nacio(Y, australia).
ciudadanoA(X) :- madre(Y, X), nacio(Y, australia).


% Consultas
% ciudadanoA(john).
% ciudadanoA(mary).
% ciudadanoA(peter).

%%%%%%%%%%%%%%%%%%%
%%% Ejercicio 5 %%%
%%%%%%%%%%%%%%%%%%%

curso(pp, horario(lunes,11,13), prof(ernesto,copello), salon(facIng,404), carrera(id,m6a)).
curso(pp, horario(martes,11,13), prof(nora,szasz,), salon(palCafe,109), carrera(id,m6b)).

% Defina predicados y reglas que le permitan extraer la siguiente información
% a partir de estos hechos:

% a) docente(Prof,Cur) que indica que Prof es docente del curso Cur.

docente(Prof, Cur) :-
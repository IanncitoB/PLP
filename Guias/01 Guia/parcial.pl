subsecuenciaCreciente([],[]).
subsecuenciaCreciente([X|XS], [X|L]) :- subsecuenciaCreciente(XS,L), estrictaCreciente([X|L]).
subsecuenciaCreciente([_|XS], L) :- subsecuenciaCreciente(XS,L).

estrictaCreciente([]).
estrictaCreciente([_]).
estrictaCreciente([X,Y|XS]) :- X < Y, estrictaCreciente([Y|XS]).

subsecuenciaCrecienteMasLarga(L,S) :-   subsecuenciaCreciente(L,S),
                                        length(S,T),
                                        not(existeSubsecuenciaMasLarga(L,T)).

existeSubsecuenciaMasLarga(L,T) :-      subsecuenciaCreciente(L,S),
                                        length(S,TS),
                                        TS > T.

fibonacci(X) :- fibonacciDeADos(X,_).

fibonacciDeADos(1,1).
fibonacciDeADos(Y,S) :- fibonacciDeADos(X,Y), S is X+Y.


fibrev(X) :- var(X), fibonacci(X).
fibrev(X) :- nonvar(X),
             between(1,X,F2),
             F1 is X - F2,
             fibrevHasta(F1,F2).

fibrevHasta(0,1).
fibrevHasta(F1,F2) :- F1 > 0, F2 > 0, 
                      F3 is F2 - F1, 
                      fibrevHasta(F3,F1).

% p(b) :- !.
% p(a).

% q(b).
% p(X) :- not(not(q(X))), X = a.
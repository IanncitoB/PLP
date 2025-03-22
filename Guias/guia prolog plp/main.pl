% juntar(?Lista1, ?Lista2, ?Lista3) USAR APPEND
juntar([], L2, L2).
juntar([X|XS], L2, [X|L3]) :- juntar(XS,L2,L3).

% last(?L, ?U)
last(X,[X]).
last(X,[_|XS]) :- last(X,XS).

% reverse(+L, -L1)
reverse([],[]).
reverse([X|XS], L) :- reverse(XS,L1), append(L1,[X],L).

% prefijo(?P,+L)
prefijo(P,L) :- append(P,_,L).

% sufijo(?S,+L)
sufijo(S,L) :- append(_,S,L).

% sublista(?S, +L)
sublista([],_).
sublista([S|Ss],L) :- prefijo(P,L), sufijo([S|Ss],P).

% pertenece(?X, +L) USAR MEMBER
pertenece(E,[E|_]).
pertenece(E,[_|XS]) :- pertenece(E,XS).

% aplanar(+XS, -YS) USAR FLATTEN
aplanar([],[]).
aplanar([X|XS], YS) :- not(is_list(X)),            aplanar(XS,REC), append([X],REC,YS).
aplanar([X|XS], YS) :- is_list(X), aplanar(X,RES), aplanar(XS,REC), append(RES,REC,YS).

% interseccion(+L1, +L2, -L3) SI NO HAY REPETIDOS, ES EQUIVALENTE A INTERSECTION
interseccion([],_,[]).
interseccion([X|XS],L2,L3) :- not(member(X,L2)), interseccion(XS,L2,L3).
interseccion([X|XS],L2,L3) :- member(X,L2), borrar(XS,X,SINX), interseccion(SINX, L2, REC), append([X],REC,L3).

% partir(?N,+L,?L1,?L2)
partir(N,L,L1,L2) :-append(L1,L2,L), length(L1,N).

% borrar(+L1,+E,-L2)
borrar([],_,[]).
borrar([X|XS], X, L2) :- borrar(XS,X,L2).
borrar([X|XS], E, L2) :- E \= X, borrar(XS,E,REC), append([X],REC,L2).

% sacarDuplicados(+L1,-L2)
sacarDuplicados([],[]).
sacarDuplicados([X|XS], YS) :- borrar(XS,X,SINX), sacarDuplicados(SINX,REC), append([X],REC,YS).

% permutacion(+L1,?L2)
permutacion([],[]).
permutacion([X|XS],L2) :- permutacion(XS,REC),
                          length(REC,S), between(0,S,I),
                          partir(I,REC,REC1,REC2),
                          append(REC1,[X|REC2],L2).

% reparto(+L,+N, -LListas)
reparto(L,1,[L]).
reparto(L,N,[Y|YS]) :-  N >= 1, N1 is N-1, 
                        append(Y,L2,L),    % Y es prefijo de L, L2 es lo que sobra
                        reparto(L2,N1,YS). % reparto L2 en N-1 listas

% repartoSinVacias(+L,-LListas)
repartoSinVacias(L, RES) :- length(L,S), between(0,S,N), reparto(L,N,RES), sinVacias(RES).

sinVacias(L) :- not(member([],L)).

% parteQueSuma(+L,+S,-P)
parteQueSuma([],0,[]).
parteQueSuma([X|XS],S,P) :- S2 is S-X, parteQueSuma(XS,S2,REC), append([X],REC,P).
parteQueSuma([_|XS],S,P) :- parteQueSuma(XS,S,P).

% desde(+X,-Y)
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

% desdeReversible(+X,?Y)
desdeReversible(X,X).
desdeReversible(X,Y) :- nonvar(Y), X =< Y.
desdeReversible(X,Y) :- var(Y), N is X+1, desdeReversible(N,Y).

% intercalar(L1,L2,L3)
intercalar([],[],[]).
intercalar([X|XS],L2,[X|L3]) :- intercalar(XS,L2,L3).
intercalar(L1,[Y|YS],[Y|L3]) :- intercalar(L1,YS,L3).

% ARBOLES BINARIOS
vacio(nil).
raiz(bin(_,V,_), V).
altura(nil, 0).
altura(bin(I,_,D), A) :- altura(I,AI), altura(D,AD), A is 1 + max(AI,AD).
cantidadNodos(nil,0).
cantidadNodos(bin(I,_,D), C) :- cantidadNodos(I,CI), cantidadNodos(D,CD), C is 1+CI+CD.

inorder(nil, []).
inorder(bin(I,V,D), L) :- inorder(I,RECI), inorder(D,RECD), append(RECI,[V|RECD], L).

% coprimos(-X,-Y)
coprimos(X,Y) :- desde(1,S), paresQueSuman(S,X,Y), gcd(X,Y) =:= 1.

% paresQueSuman(+S,-A,-B)
paresQueSuman(S,A,B) :- between(0,S,A), B is S - A.

% cuadradoSemiMagico(+N,-XS)
cuadradoSemiMagico(0,[]).
cuadradoSemiMagico(N,XS) :- desde(0,S), length(XS,N), generarMatriz(S,N,N,XS).

% generarMatriz(+S,+N,+M,-XS) Matriz de N filas, M columnas todas filas suman S.
generarMatriz(_,_,0,[]).
generarMatriz(S,N,M,XS) :-  M > 0, M2 is M-1,
                            generarMatriz(S,N,M2,REC),
                            listaQueSumaSTamañoN(S,N,L),
                            append([[L]],REC,XS).

% listaQueSumaSTamañoN(+S,+N,-L)
listaQueSumaSTamañoN(_,0,[]).
listaQueSumaSTamañoN(S,N,L) :- N > 0, N2 is N-1, between(0,S,E),
                               S2 is S-E,
                               listaQueSumaSTamañoN(S2,N2,REC),
                               append([E],REC,L),
                               sum_list(L,S).

cuadradoMagico(N,XS) :- desde(0,S), generarMatriz(S,N,N,XS), columnasSuman(S,XS).

% columnasSuman(+S,+XS)
columnasSuman(0,[]).
columnasSuman(S,[X|XS]) :-  length(X,N), between(1,N,I),
                            sumaColumna(I,[X|XS],S).

% Esto lo estaba cocinando en el momento, no terminado ↓
noSumaColumna(_,[],S) :- S =\= 0.
noSumaColumna(I,[X|XS],S) :- nth1(I,X,E), sumaColumna(I,XS,REC), S =\= E+REC.

sumaColumna(_,[],0).
sumaColumna(I,[X|XS],S) :- nth1(I,X,E), sumaColumna(I,XS,REC), S is E+REC.

% columna(+I, +Fs, -Xs): instancia Xs en la columna I-ésima de la matriz Fs.
columna(_, [], []).
columna(I, [F|Fs], [X|Xs]) :- nth1(I, F, X), columna(I, Fs, Xs).

% listaQueSuma(+S,-L) todos los elementos >0
listaQueSuma(0,[]).
listaQueSuma(S,L) :- S > 0, between(1,S,E),
                     S1 is S-E, listaQueSuma(S1,L1), append([E],L1,L).

% Puede servir (pero es medio fea)
listaDeNaturales(L) :- desde(0,S), listaQueSuma(S,L1), restarle1(L1,L).
restarle1([],[]).
restarle1([X|XS], L) :- X2 is X-1, restarle1(XS,L2), append([X2],L2,L).

subsecuenciaCreciente([],[]).
subsecuenciaCreciente([X|XS],[X|REC]) :- subsecuenciaCreciente(XS,REC), estrictaCreciente([X|REC]).
subsecuenciaCreciente([_|XS],REC)     :- subsecuenciaCreciente(XS,REC).

estrictaCreciente([]).
estrictaCreciente([_]).
estrictaCreciente([X|[Y|XS]]) :- X < Y, estrictaCreciente([Y|XS]).

subsecuenciaCrecienteMasLarga(L,S) :- subsecuenciaCreciente(L,S), length(S,T), not(existeMasLarga(L,T)).

existeMasLarga(L,T) :- subsecuenciaCreciente(L,S), length(S,T2), T2 > T.


% Predicados: 
% sort 			(+List, -Sorted) sin duplicados
% msort 			(+List, -Sorted) con duplicados
% length			(?List, ?Length)
% nth1/3			(?Index, ?List, ?Elem)
% nth0/3			(?Index, ?List, ?Elem)
% nth1/4			(?N,?List,?Elem,?Rest)
% nth0/4			(?N,?List,?Elem,?Rest)
% member			(?Elem, ?List)
% append/3		(?L1,?L2,?L1++L2)
% append/2		(+ListOfLists, ?L)
% last			(?List, ?Last)
% between			(+Low,+High,-Value)
% is_list			(+Term)
% list_to_set		(+List,?Set)
% is_set			(@Set)
% union			(+Set1,+Set2,-Set3)
% intersection		(+Set1,+Set2,-Set3)
% subset			(+Subset,+Set)
% subtract		(+Set,+Delete,-Result)
% select			(?X,?XList,?Y,?YList)
% delete			(+List1,@Elem,-List2)
% reverse			(?List1,?List2)
% atom			(@Term)
% number			(@Term)
% numlist			(+Low,+High,-List)
% sum_list		(+List,-Sum)
% flatten			(+NestedList,-FlatList)

% Operaciones extra-lógicas: is, \=, ==, =:=, =\=, >, <, =<, >=, abs, max, min, mod, gcd, var, nonvar, ground, trace, notrace, make, halt

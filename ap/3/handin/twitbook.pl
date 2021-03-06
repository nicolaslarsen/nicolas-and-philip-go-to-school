mem(Y, [Y| _]).
mem(Y, [ _ | Tail]) :- mem(Y, Tail).
likes([person(X, Friends)|_], X, Y) :- mem(Y, Friends).
likes([_ | Tail], X, Y) :- likes(Tail, X, Y).

/**
* X likes Y, Y no likes X, Y in X
**/

different([ _ | Tail], X, Y) :- different(Tail, X, Y).
different([person(X, _) | Tail], X, Y) :- mem(person(Y, _), Tail).
different([person(Y, _) | Tail], X, Y) :- mem(person(X, _), Tail).

dislikes(G, X, Y) :- likes(G, Y, X), different(G, X, Y), notLikedBy(G, G, X, Y).

notLikedBy(G, [person(X, Friends) | _], X, Y) :- notInList(G, Friends, Y).
notLikedBy(G, [ _ | Tail], X, Y) :- notLikedBy(G, Tail, X, Y).
notLikedBy(G, [], X, _) :- different(G, X, X).

notInList(G, [Z | Tail], Y) :- notInList(G, Tail,  Y), different(G, Z, Y).
notInList(_, [], _).

popular(G, X) :- pop1(G, X, G).
pop1([person(X, Friends)|_], X, G):- allLike(G, X, Friends).
pop1([_ | Tail], X, G) :- pop1(Tail, X, G).
allLike(G, X, [Head | Tail]) :- likes(G, Head, X), allLike(G, X, Tail).
allLike(_, _, []).

outcast(G, X) :- out1(G, X, G).
out1([person(X, Friends)|_], X, G) :- allDisLike(G, X, Friends).
out1([_ | Tail], X, G) :- out1(Tail, X, G).
allDisLike(G, X, [Head | Tail]) :- dislikes(G, Head, X), allDisLike(G, X, Tail).
allDisLike(_, _, []). 


friendly(G, X) :- keepGraph(G, X, G), isMem(G, X).
keepGraph([person(Y, _) | Tail], X, G) :- chill(G, X, Y), keepGraph(Tail, X, G).

keepGraph([], _, _).
chill(G, X, Y) :- likes(G, X, Y), likes(G, Y, X).
chill(G, X, Y) :- notLikedBy(G, G, Y, X). 

hostile(G, X) :- keepGraphU(G, X, G), isMem(G, X).
keepGraphU([person(Y, _) | Tail], X, G) :- unchill(G, X, Y), keepGraphU(Tail, X, G).
keepGraphU([], _, _).
unchill(G, X, Y) :- dislikes(G, X, Y), likes(G, Y, X).
unchill(G, X, Y) :- notLikedBy(G, G, Y, X).


admires(G, X, Y) :- admires(G, X, Y, [X]).
admires(G, X, Y, V) :- different(G, X, Y), likes(G, X, N), notInList(G, V, N), admires(G, N, Y, [N | V]).
admires(G, X, Y, _) :- likes(G, X, Y).




isMem([person(X, _) | _], X).
isMem([ _ | Tail], X) :- isMem(Tail, X).

list([person(X, Friends)|_], X, Friends).
list([_ | Tail], X, Friends) :- list(Tail, X, Friends).



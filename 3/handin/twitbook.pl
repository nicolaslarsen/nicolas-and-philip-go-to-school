g1([person(kara, [barry, clark]),
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).

mem(Y, [Y| Tail]).
mem(Y, [Head | Tail]) :- mem(Y, Tail).
likes([person(X, Friends)|Tail], X, Y) :- mem(Y, Friends).
likes([Head | Tail], X, Y) :- likes(Tail, X, Y).

/**
* X likes Y, Y no likes X, Y in X
**/
same(X,X).
isFalse(X) :- same("this", "false").
dis([], X).
dis([A|Tail], X) :- dis(Tail, X), (X \= A).

%different([person(Z, Friends) | Tail], X, Y) :- different(Tail, X, Y).
different([person(X, Friends) | Tail], X, Y) :- mem(person(Y, _), Tail).
different([person(Y, Friends) | Tail], X, Y) :- mem(person(X, _), Tail).

dislikes(G, X, Y) :- likes(G, Y, X), different(G, X, Y), helper(G, X, Y).
helper([person(X, Friends)|Tail], X, Y) :- dis(Friends, Y).
helper([Head|Tail], X, Y) :- helper(Tail, X, Y).

popular(G, X) :- pop1(G, X, G).
pop1([person(X, Friends)|Tail], X, G):- allLike(G, X, Friends).
pop1([Head | Tail], X, G) :- pop1(Tail, X, G).
allLike(G, X, [Head | Tail]) :- likes(G, Head, X), allLike(G, X, Tail).
allLike(G, X, []).

outcast(G, X) :- out1(G, X, G).
out1([person(X, Friends)|Tail], X, G) :- allDisLike(G, X, Friends).
out1([Head | Tail], X, G) :- out1(Tail, X, G).
allDisLike(G, X, [Head | Tail]) :- dislikes(G, Head, X), allDisLike(G, X, Tail).
allDisLike(G, X, []). 


friendly(G, X) :- keepGraph(G, X, G).
keepGraph([person(Y, _) | Tail], X, G) :- chill(G, X, Y), keepGraph(Tail, X, G).
keepGraph([], _, _).
chill(G, X, Y) :- likes(G, X, Y), likes(G, Y, X).
chill(G, X, Y) :- helper(G, Y, X).

hostile(G, X) :- keepGraphU(G, X, G).
keepGraphU([person(Y, _) | Tail], X, G) :- unchill(G, X, Y), keepGraphU(Tail, X, G).
keepGraphU([], _, _).
unchill(G, X, Y) :- dislikes(G, X, Y), likes(G, Y, X).
unchill(G, X, Y) :- helper(G, Y, X).




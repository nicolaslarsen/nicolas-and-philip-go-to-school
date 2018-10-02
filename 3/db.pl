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
dislikes([person(X, Friends)|Tail], X, Y) :- likes([person(X, Friends)|Tail], Y, X), different(


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





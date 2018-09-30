g1([person(kara, [barry, clark]),
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).


likes([person(X, Friends)|Tail], X, Y) :- member(Y, Friends).
likes([Head | Tail], X, Y) :- likes(Tail, X, Y).

different(G, X, Y) :- member(person(X, _), G), member(person(Y, _), G), Y \= X.

dislikes(G, X, Y) :- different(G, X, Y), likes(G, Y, X), not (likes(G, X, Y)).

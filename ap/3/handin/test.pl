/* This is the testing suite for twitbook.pl */

:- begin_tests(twitbook).

g([person(kara, [barry, clark]),
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).

test(like, [nondet]) :- g(G), likes(G, kara, barry).
test(like, [nondet]) :- g(G), likes(G, kara, clark).
test(like, [fail]) :- g(G), likes(G, kara, oliver).

test(dislike, [nondet]) :- g(G), dislikes(G, kara, oliver).
test(dislike, [nondet]) :- g(G), dislikes(G, oliver, clark).
test(dislike, [fail]) :- g(G), dislikes(G, kara, bruce).

test(popular, [nondet]) :- g(G), popular(G, kara).
test(popular, [fail]) :- g(G), popular(G, oliver).
test(popular, [fail]) :- g(G), popular(G, clark).

test(outcast, [nondet]) :- g(G), outcast(G, bruce).
test(outcast, [fail]) :- g(G), outcast(G, kara).
test(outcast, [fail]) :- g(G), outcast(G, clark).

test(friendly, [nondet]) :- g(G), friendly(G, barry).
test(friendly, [nondet]) :- g(G), friendly(G, bruce).
test(friendly, [fail]) :- g(G), friendly(G, kara).

test(hostile, [nondet]) :- g(G), hostile(G, oliver).
test(hostile, [fail]) :- g(G), hostile(G, kara).
test(hostile, [fail]) :- g(G), hostile(G, clark).

test(admires, [nondet]) :- g(G), admires(G, kara, barry).
test(admires, [nondet]) :- g(G), admires(G, kara, oliver).
test(admires, [fail]) :- g(G), admires(G, kara, bruce).
test(admires, [nondet]) :- g(G), admires(G, bruce, kara).
test(admires, [nondet]) :- g(G),  admires(G, clark, barry).

:- end_tests(twitbook).

/* The following directive runs the tests. You can also give the goal
   run_tests on the command line (without the :- part). */
:-    run_tests.

-module(test_quizmaster).
-export([fib/1, starter/0, questions_test_/0, pre_pop/0, next_test_/0]).
-include_lib("eunit/include/eunit.hrl").

   fib(0) -> 1;
   fib(1) -> 1;
   fib(N) when N > 1 -> fib(N-1) + fib(N-2).

   fib_test_() ->
       [?_assert(fib(0) =:= 1),
        ?_assert(fib(1) =:= 1),
        ?_assert(fib(2) =:= 2),
        ?_assert(fib(3) =:= 3),
        ?_assert(fib(4) =:= 6),
        ?_assert(fib(5) =:= 8),
        ?_assertException(error, function_clause, fib(-1)),
        ?_assert(fib(31) =:= 2178309)
       ].


starter() ->
    {_, A} = quizmaster:start(),
    quizmaster:add_question(A, {"who is my partner", [{correct, "nicolas"}, "philip", "tom"]}),
    quizmaster:add_question(A, {"who is my cool", [{correct, "Jason"}, "sam,", "lily"]}),
    quizmaster:add_question(A, {"who is the prettiest", [{correct, "Philip"}, "tom", {correct, "Nicolas"}]}),
    quizmaster:get_questions(A).


pre_pop() ->
    {_, A} = quizmaster:start(),
    quizmaster:add_question(A, {"who is my partner", [{correct, "nicolas"}, "philip", "tom"]}),
    quizmaster:add_question(A, {"who is my cool", [{correct, "Jason"}, "sam,", "lily"]}),
    quizmaster:add_question(A, {"who is the prettiest", [{correct, "Philip"}, "tom", {correct, "Nicolas"}]}),
    A.

questions_test_() ->
    Result =  [{"who is my partner", [{correct, "nicolas"}, "philip", "tom"]},
               {"who is my cool", [{correct, "Jason"}, "sam,", "lily"]},
               {"who is the prettiest", [{correct, "Philip"}, "tom", {correct, "Nicolas"}]}],
    [?_assert(starter() =:= Result)].


next_test_() ->
    A = pre_pop(),
    quizmaster:play(A),
    [?_assert(quizmaster.next(A) =:= {"who is my partner",["nicolas","philip","tom"]}),
     ?_assert(quizmaster.next(A) =:= {"who is my cool", ["Jason", "sam,", "lily"]}),
     ?_assert(quizmasater.next(A) =:= {"who is the prettiest", ["Philip", "tom", "Nicolas"]})].



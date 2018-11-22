-module(test_quiz).

-export([starter/0, pre_pop/0]).
-include_lib("eunit/include/eunit.hrl").


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
    B = quizmaster:next(A),
    quizmaster:timesup(A),
    C = quizmaster:next(A),
    quizmaster:timesup(A),

    D = quizmaster:next(A),
    [?_assert(B =:= {ok, {"who is my partner",["nicolas","philip","tom"]}}),
     ?_assert(C =:= {ok, {"who is my cool", ["Jason", "sam,", "lily"]}}),
     ?_assert(D =:= {ok, {"who is the prettiest", ["Philip", "tom", "Nicolas"]}})].


rest_test_() ->
    Q = pre_pop(),
    {ok, Nick} = quizmaster:join(Q, "Nick"),
    {ok, Phil} = quizmaster:join(Q, "Phil"),
    R1 = quizmaster:play(Q),
    quizmaster:next(Q),
    {_,R2,R22, R222} = quizmaster:guess(Q, Nick, 2),
    {_, R3, R33, R333} = quizmaster:guess(Q, Phil, 1),
    {B1, B2, _, _, B5} = quizmaster:timesup(Q),
    quizmaster:next(Q),
    quizmaster:guess(Q, Nick, 2),
    quizmaster:guess(Q, Phil, 1),
    quizmaster:timesup(Q),
    F =quizmaster:leave(Q, Nick),
    quizmaster:next(Q),

    quizmaster:guess(Q, Phil, 3),
    quizmaster:timesup(Q),
    {N1, _} = quizmaster:join(Q, "Phil"),

    [?_assert(R1 =:= ok),
     ?_assert({guess, Nick, 2} =:=  {R2, R22, R222}),
     ?_assert({guess, Phil, 1} =:=  {R3, R33, R333}),
     ?_assert({ok, [1, 1, 0], false} =:= {B1, B2, B5}),
     ?_assert(ok =:= F),
     ?_assert(ok =:= N1)].




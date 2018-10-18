-module(quizmaster).

%% API exports.
-export([start/0, add_question/2, get_questions/1,
         play/1, next/1, timesup/1, loop/2,
         join/2, leave/2, guess/3]).

%Need to add a pattern match for case of failure
start() -> {ok, spawn(quizmaster, loop, [queue:new(), #{}])}.


add_question(Q, {Description, MarkedAnswers}) -> 
    Q ! {self(), {add_question, Description, MarkedAnswers}}.

get_questions(Q) ->
    Q ! {self(), get_questions},
    receive
      {Q, {questions, Questions}} -> queue:to_list(Questions)
    end.



loop(Questions, Players) ->
    Me = self(),
    receive
        {From, {add_question, Description, MarkedAnswers}} ->
            loop(queue:snoc(Questions, {Description, MarkedAnswers}), Players);
        {From, get_questions} ->
            From ! {Me, {questions, Questions}},
            loop(Questions, Players);
        _ -> "This failed"
    end.

play(Q) -> "not implemented".
next(Q) -> "not implemented".
timesup(Q) -> "not implemented".
join(Q, Player) -> "not implemented".
guess(Q, Ref, Index) -> "not implemented".
leave(Q, Player) -> "not implemented".

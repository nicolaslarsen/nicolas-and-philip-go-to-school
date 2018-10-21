-module(quizmaster).

%% API exports.
-export([start/0, add_question/2, get_questions/1,
         play/1, next/1, timesup/1, loop/3, playermap/1,
         join/2, leave/2, guess/3]).
loop(Questions, Players, Status) ->
    Me = self(),
    receive
        {From, {add_question, Description, MarkedAnswers}} ->
            if
              Status =:= editable ->
                From ! {Me, ok},
                loop(queue:snoc(Questions, {Description, MarkedAnswers}), Players, Status);
              true -> From ! {error, we_are_playing},
                      loop(Questions, Players, Status)
            end;
        {From, get_questions} ->
            From ! {Me, {questions, Questions}},
            loop(Questions, Players, Status);
        {From, play} ->
                    From ! {Me, ok},
                    loop(Questions, Players,
                         % I was thinking the status could be {conductor, state, score}
                         % but this is starting to feel funky.
                         % However, I would just leave status as "edible" until play(Q) is called
                         {From, {playing, between_questions}, playermap(Players)});
        {From, _} -> From ! {Me, {error, "Arguments are on the wrong form"}}
    end.

% We get the keys already defined in Players, then map each key to a {key, 0}.
% This way our scoreboard is initialized. Then we convert this new list to a map.
playermap(Players) ->
        Keys = maps:keys(Players),
        maps:from_list(lists:map(fun(X) -> {X, 0} end, Keys)).

%Need to add a pattern match for case of failure
start() -> {ok, spawn(quizmaster, loop, [queue:new(), #{}, editable])}.


add_question(Q, {Description, MarkedAnswers}) ->
    Q ! {self(), {add_question, Description, MarkedAnswers}},
    receive
      {Q, ok} -> ok;
      {Q, {error, Message}} -> {error, Message};
      _ -> {error, undefined_error}
    after 1000 ->
      {error, timed_out}
    end.


get_questions(Q) ->
    Q ! {self(), get_questions},
    receive
      {Q, {questions, Questions}} -> queue:to_list(Questions)
    end.

play(Q) ->
    case get_questions(Q) of
      [] -> {error, no_questions};
      _ ->
          Q ! {self(), play},
          receive
            {Q, ok} -> ok;
            {Q, Error} -> Error
          end
    end.

next(Q) -> "not implemented".
timesup(Q) -> "not implemented".
leave(Q, Player) -> "not implemented".
join(Q, Player) -> "not implemented".
guess(Q, Ref, Index) -> "not implemented".

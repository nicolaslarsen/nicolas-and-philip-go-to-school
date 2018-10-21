-module(quizmaster).

%% API exports.
-export([start/0, add_question/2, get_questions/1,
         play/1, next/1, timesup/1, loop/3,
         playerscore/1, hideAnswers/1, addToTotal/2,
         name_free/2, join/2, leave/2, guess/3]).
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
                    if
                      Status =:= editable ->
                        From ! {Me, ok},
                        loop(Questions, Players,
                          {From, playing_between_questions, playerscore(Players)});
                      true -> 
                        From ! {Me, {error, conductor_exists}},
                        loop(Questions, Players, Status)
                    end;
        {From, next} ->
                    case Status of
                      {Conductor, playing_between_questions, Scores} ->
                        if
                          From =:= Conductor ->
                            Question = hideAnswers(queue:get(Questions)),
                            From ! {Me, {ok, Question}},

                            % Sends next_question to player, to be used with map
                            SendToPlayer =
                              fun(Pid, {_, Ref}) ->
                                      Pid ! {next_question, Ref, Question}
                              end,

                            maps:map(SendToPlayer, Players),
                            loop(Questions, Players,
                                 {Conductor, playing_active_question, Scores});
                          true ->
                            From ! {error, who_are_you},
                            loop(Questions, Players, Status)
                        end;
                      {_, playing_active_question, _} ->
                          From ! {Me, {error, has_active_question}},
                          loop(Questions, Players, Status)
                    end;
        {From, times_up} ->
                    case Status of
                      {Conductor, playing_active_question, Scores} ->
                        if From =:= Conductor ->
                          LastQ = playerscore(Players), %standing for LastQ
                          NewTotal = addToTotal(maps:to_list(LastQ), Scores),
                          % We still need to remove placeholders
                          From ! {Me, {ok, dist_undefined, LastQ, NewTotal, false}},
                          loop(Questions, Players,
                               {Conductor, playing_between_questions, NewTotal});
                        true ->
                          From ! {Me, {error, nice_try}},
                          loop(Questions, Players, Status)
                        end;
                      {_, playing_between_questions, _} ->
                          From ! {Me, {error, no_question_asked}},
                          loop(Questions, Players, Status);
                      _ -> From ! {Me, {error, not_even_playing}},
                           loop(Questions, Players, Status)
                    end;
        {From, join, Player} ->
                    case Status of
                      {editable} ->
                            NameIsFree = name_free(Player, maps:values(Players)),
                            if
                              NameIsFree ->
                                    Ref = make_ref(),
                                    NewPlayers = maps:put(From, {Player, Ref}),
                                    From ! {Me, {ok, Ref}},
                                    loop(Questions, NewPlayers, editable);
                              true -> 
                                    From ! {Me, {error, is_taken}},
                                    loop(Questions, Players, Status)
                            end;
                      _ -> 
                          From ! {Me, {error, cant_join_yet}},
                          loop(Questions, Players, Status)
                    end;
        {From, _} -> From ! {Me, {error, "Arguments are on the wrong form"}},
                     loop(Questions, Players, Status)
    end.

% We get the keys already defined in Players, then map each key to a {key, 0}.
% This way our scoreboard is initialized. Then we convert this new list to a map.
playerscore(Players) ->
        Keys = maps:keys(Players),
        maps:from_list(lists:map(fun(X) -> {X, 0} end, Keys)).

hideAnswers({Description, Answers}) ->
        HideAnswer =
          fun(X) ->
              case X of
                      {_, Answer} -> Answer;
                      Answer            -> Answer
              end
          end,
        {Description, lists:map(HideAnswer, Answers)}.

% Increments the value for key Pid in Total by each score in a list of {Pid, Score}
addToTotal([], Total) -> Total;
addToTotal([{Pid, Score} | Scores], Total) ->
        CurrentScore = maps:get(Pid, Total, 0),
        NewTotal = maps:update(Pid, Score + CurrentScore, Total),
        addToTotal(Scores, NewTotal).

% Player name is not in use
name_free(_, []) -> true;
name_free(Player, [{Name, _} | Players]) ->
        if
          Player =:= Name -> false;
          true -> name_free(Player, Players)
        end.



%Need to add a pattern match for case of failure
start() -> {ok, spawn(quizmaster, loop, [queue:new(), #{}, editable])}.

add_question(Q, {Description, MarkedAnswers}) ->
    Q ! {self(), {add_question, Description, MarkedAnswers}},
    receive
      {Q, ok} -> ok;
      {Q, {error, Message}} -> {error, Message};
      Anything -> Anything
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

next(Q) ->
        Q ! {self(), next},
        receive
          {Q, {ok, Question}} -> Question;
          {Q, Message}        -> Message
        end.

timesup(Q) ->
        Q ! {self(), times_up},
        receive
          {Q, {error, nice_try}} -> nice_try;
          {Q, Response} -> Response
        end.

join(Q, Player) ->
        Q ! {self(), join, Player},
        receive
          {Q, {ok, Ref}} -> {ok, Ref};
          {Q, {error, Error}} -> {error, Error}
        end.

leave(Q, Player) -> "not implemented".
guess(Q, Ref, Index) -> "not implemented".

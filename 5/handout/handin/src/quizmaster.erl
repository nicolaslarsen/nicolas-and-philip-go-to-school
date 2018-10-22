-module(quizmaster).

%% API exports.
-export([start/0, add_question/2, get_questions/1,
         play/1, next/1, timesup/1, loop/3, get_dist/2,
         inc_nth/2, playerscore/1, hideAnswers/1, addToTotal/2,
         player_free/2, join/2, leave/2, guess/3]).

% This is the quiz loop. Questions is a queue, pretty self-explanatory.
% Players is a map, where we decided to store the players by their reference, for easy lookup.
% Status however, can either be the atom "editable", or some {Conductor, State, Score}
% Where state will also contain the guesses of the players, if a question is active
loop(Questions, Players, Status) ->
    Me = self(),
    receive
        {From, {add_question, Description, MarkedAnswers}} ->
            case Status of
              editable ->
                From ! {Me, ok},
                loop(queue:snoc(Questions, {Description, MarkedAnswers}), Players, Status);
              _ -> From ! {error, we_are_playing},
                      loop(Questions, Players, Status)
            end;
        {From, get_questions} ->
            From ! {Me, {questions, Questions}},
            loop(Questions, Players, Status);
        {From, play} ->
                    case Status of
                      editable ->
                        From ! {Me, ok},
                        loop(Questions, Players,
                          {From, playing_between_questions, playerscore(Players)});
                      _ ->
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
                              fun(Ref, {_, Pid}) ->
                                      Pid ! {next_question, Ref, Question}
                              end,

                            maps:map(SendToPlayer, Players),
                            % We store the guesses in this map
                            loop(Questions, Players,
                                 {Conductor, {playing_active_question, #{}}, Scores});
                          true ->
                            From ! {error, who_are_you},
                            loop(Questions, Players, Status)
                        end;
                      {_, {playing_active_question, _} , _} ->
                          From ! {Me, {error, has_active_question}},
                          loop(Questions, Players, Status);
                      _ ->
                          From ! {Me, {error, not_playing_yet}},
                          loop(Questions, Players, Status)
                    end;
        {From, times_up} ->
                    case Status of
                      {Conductor, {playing_active_question, Guesses}, Scores} ->
                        if From =:= Conductor ->
                          case queue:out(Questions) of
                            {{_, {_, Answers}}, RemQuestions} ->

                              % Give points for a single guess
                              GivePoints =
                                fun(_, Index) ->
                                    case lists:nth(Index, Answers) of
                                        {correct, _}  -> 1;
                                        _             -> 0
                                    end
                                end,

                              Dist = get_dist(Guesses, length(Answers)),
                              LastQ = maps:map(GivePoints, Guesses),
                              NewTotal = addToTotal(maps:to_list(LastQ), Scores),
                              Final = queue:is_empty(RemQuestions) =:= true,

                              % We still need to remove placeholders
                              From ! {Me, {ok, Dist, LastQ, NewTotal, Final}},

                              if
                                % game should end
                                Final ->
                                    % Sends a message to a player, to be used with map
                                    SendToPlayer =
                                      fun(_, {_, Pid}) ->
                                        Pid ! {Me, quiz_over}
                                      end,
                                    maps:map(SendToPlayer, Players),
                                    % Just to reinitialize the quiz
                                    loop(RemQuestions, #{}, editable);
                                true ->
                                    loop(RemQuestions, Players,
                                        {Conductor, playing_between_questions, NewTotal})
                              end;
                            _ -> From ! {Me, {error, question_wrong_format}}
                          end;
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
                      editable ->
                            NameIsFree = player_free(Player, maps:values(Players)),
                            if
                              NameIsFree ->
                                    Ref = make_ref(),
                                    NewPlayers = maps:put(Ref, {Player, From}, Players),
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
        % Players does not have to be deleted from the scoreboard,
        % so there's no reason to check the status
        {From, leave, Ref} ->
                    UserExists = maps:is_key(Ref, Players),
                    if
                      UserExists ->
                            From ! {Me, ok},
                            loop(Questions, maps:remove(Ref, Players), Status);
                      true ->
                            From ! {Me, {error, who_are_you}},
                            loop(Questions, Players, Status)
                    end;
        {_, guess, Ref, Index} ->
            case Status of
              {Conductor, {playing_active_question, Guesses}, Scores} ->
                PlayerExists = maps:is_key(Ref, Players),
                if
                  PlayerExists ->
                    loop(Questions, Players,
                         {Conductor,
                          {playing_active_question, maps:put(Ref, Index, Guesses)},
                           Scores});
                  true ->
                    loop(Questions, Players, Status) % We just ignore the guess
                end;
              _ -> loop(Questions, Players, Status) % We just ignore the guess
            end;
        {From, _} ->
                    From ! {Me, {error, "Arguments are on the wrong form"}},
                    loop(Questions, Players, Status)
    end.

% We get the keys already defined in Players, then map each key to a {key, 0}.
% This way our scoreboard is initialized. Then we convert this new list to a map.
playerscore(Players) ->
        Keys = maps:keys(Players),
        maps:from_list(lists:map(fun(X) -> {X, 0} end, Keys)).

% Check if player name is free
player_free(_, []) -> true;
player_free(Player, [{Name, _} | Players]) ->
        if
          Player =:= Name -> false;
          true -> player_free(Player, Players)
        end.

% Increment the nth element of a list
inc_nth(_, []) -> [];
inc_nth(N, [X | Xs]) ->
        if
          N =:= 1 -> [X + 1 | Xs];
          true -> [X | inc_nth(N-1, Xs)]
        end.

% Builds a distribution of answers
get_dist(Guesses, NumAnswers) ->
        Initial = lists:duplicate(NumAnswers, 0),
        Indexes = maps:values(Guesses),
        Fun = fun(X, List) -> inc_nth(X, List) end,
        lists:foldl(Fun, Initial, Indexes).

% Just removes the correct part of {correct, answer}
hideAnswers({Description, Answers}) ->
        HideAnswer =
          fun(X) ->
              case X of
                      {_, Answer} -> Answer;
                      Answer -> Answer
              end
          end,
        {Description, lists:map(HideAnswer, Answers)}.

% Increments the value for key Pid in Total by each score in a list of {Pid, Score}
addToTotal([], Total) -> Total;
addToTotal([{Ref, Score} | Scores], Total) ->
        CurrentScore = maps:get(Ref, Total, 0),
        NewTotal = maps:update(Ref, Score + CurrentScore, Total),
        addToTotal(Scores, NewTotal).

%Need to add a pattern match for case of failure
start() -> {ok, spawn(quizmaster, loop, [queue:new(), #{}, editable])}.

add_question(Q, {Description, MarkedAnswers}) ->
    Q ! {self(), {add_question, Description, MarkedAnswers}},
    receive
      {Q, ok} -> ok;
      {Q, Message} -> Message
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
          {Q, Message} -> Message
        end.

join(Q, Player) ->
        Q ! {self(), join, Player},
        receive
          {Q, {ok, Ref}} -> {ok, Ref};
          {Q, {error, Reason}} -> {error, Reason}
        end.

leave(Q, Ref) ->
        Q ! {self(), leave, Ref},
        receive
          {Q, ok} -> ok;
          {Q, {error, Reason}} -> {error, Reason}
        end.

guess(Q, Ref, Index) ->
        Q ! {self(), guess, Ref, Index}.

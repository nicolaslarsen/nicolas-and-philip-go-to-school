-module(flamingo).

-export([new/1, request/4, route/4, drop_group/2, request_reply/2, loop/2, new_mapping/4]).

request_reply(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.



new(_Global) -> {ok, spawn(flamingo, loop, [_Global, #{}])}.

request(_Flamingo, _Request, _From, _Ref) ->
    _Flamingo ! {_From, {request, _Request, _Ref}}.
    %receive
     %   {_Ref, Response} -> Response;
      %  _ -> "this failed epically"
    %end.

route(_Flamingo, _Path, _Fun, _Arg) ->
    _Flamingo ! {self(), {route, _Path, _Fun, _Arg}},
    receive
        {ok, _Flamingo} -> {ok, _Flamingo};
        _ -> "we super failed"
    end.

drop_group(_Flamingo, _Id) ->
    not_implemented.


loop(State, LocalState) ->
    Me = self(),
    receive
        {From, {request, {_Path, _Args}, _Ref}} ->
            case maps:find(_Path, LocalState) of
              {ok, Fun} -> 
                From ! {_Ref, {200, apply(Fun, [{_Path, _Args}, State, LocalState])}};
              {error} -> 
                From ! {_Ref, {404, "Path not found"}}
            end,
            loop(State, LocalState);
        {From, {route, Path, Fun, Args}} ->
            NewMap = new_mapping(Path, Fun, Args, LocalState),
            From ! {ok, Me},
            loop(State, NewMap);
        _ -> "we Failed"
    end.

new_mapping(Path, Fun, Args, Map) ->
    case (Path) of
        [H|T] -> new_mapping(T, Fun, Args, Map#{H => Fun});
        [] -> Map
    end.


-module(flamingo).

-export([new/1, request/4, route/4, drop_group/2, request_reply/2, loop/2, new_mapping/4]).

request_reply(Pid, Request) -> 
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.



new(_Global) -> {ok, spawn(flamingo, loop, [_Global, #{}])}.

request(_Flamingo, _Request, _From, _Ref) ->
    _Flamingo ! {_From, {request, _Request, _Ref}},
    receive
        {_Ref, Response} -> Response;
        _ -> "this failed epically"
    end.

route(_Flamingo, _Path, _Fun, _Arg) ->
    _Flamingo ! {self(), {route, _Path, _Fun, _Arg}},
    receive 
        {_Flamingo, B} -> B;
        _ -> "we super failed"
    end.

drop_group(_Flamingo, _Id) ->
    not_implemented.


loop(State, LocalState) -> 
    receive
        {From, {request, {_Path, _Args}, _Ref}} ->  
            From ! {_Ref, maps:get(_Path, LocalState)},
            loop(State, LocalState);
        {From, {route, Path, Fun, Args}} -> 
            NewMap = new_mapping(Path, Fun, Args, LocalState),
            %NewMap = LocalState#{Path => Fun},
            From ! {self(), "something may have worked"},
            loop(State, NewMap);
                                         
%Must figure out how to include local State in the lines above
        _ -> "we Failed"
    end.

new_mapping(Path, Fun, Args, Map) ->
    case (Path) of
        [H|T] -> new_mapping(T, Fun, Args, Map#{H => Fun});
        [] -> Map
    end.


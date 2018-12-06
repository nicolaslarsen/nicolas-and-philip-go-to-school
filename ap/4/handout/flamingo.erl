-module(flamingo).


-export([new/1, test/1, route/4, loop/3, getRoute/2, request/4]).



new(_Global) -> {ok, spawn(flamingo, loop, [_Global, #{}, #{}])}.

route(Flamingo, Prefixes, Action, Init) ->
  Flamingo ! {self(), {route, Prefixes, Action, Init}},
  receive 
    {ok, Ref} -> {ok, Ref}
  end.

test(Flamingo) -> 
  Flamingo ! {self(), {test}},
  receive
    {results, RouteMap, StateMap} -> {RouteMap, StateMap}
  end.

loop(State, RouteMap, StateMap) ->
  %Me = self(),
  receive
    {From, {route, Prefixes, Action, Init}} ->
      Ref = make_ref(),
      %Pairs = lists:zip(Prefixes, lists:duplicate(length(Prefixes), Ref)),
      Pairs = [{P, Ref} || P <- Prefixes],
      Fun = fun(K, V, AccIn) -> maps:put(K, V, AccIn) end,
      NewRouteMap = maps:fold(Fun, RouteMap, maps:from_list(Pairs)),
      NewStateMap = maps:put(Ref, {Action, Init}, StateMap),
      From ! {ok, Ref},
      loop(State, NewRouteMap, NewStateMap);
    {From, {test}} ->
      From ! {results, RouteMap, StateMap},
      loop(State, RouteMap, StateMap);
    {_, {request, {Path, Args}, From, Ref}} -> 
      % StateRef = maps:get(Path, getRoute(RouteMap),
      case getRoute(Path, lists:reverse(maps:keys(RouteMap))) of
        {error, 404} -> From ! {Ref, {error, 404}}, %No Matching routes
                        loop(State, RouteMap, StateMap);
        {ok, P} ->  
          R = maps:get(P, RouteMap),
          {F, LocalState} = maps:get(R, StateMap),
          Result = apply(F, [{P, Args}, State, LocalState]),
          case Result of
            {new_state, Content, NewState} -> 
              NewStateMap = maps:put(R, {F, NewState}, StateMap)
              From ! {
              
      
  end.

request(Flamingo, Request, From, Ref) -> 
  Flamingo ! {request, Request, From, Ref}.

%Routes must be in reverse order"
getRoute(_, []) -> {error, 404};
getRoute(Path, [H | T]) ->
  case  string:substr(Path, 1, length(H)) of
    H -> {ok, H};
    _ -> getRoute(Path, T)
  end.

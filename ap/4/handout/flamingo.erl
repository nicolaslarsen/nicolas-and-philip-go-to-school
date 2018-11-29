-module(flamingo).


-export([new/1, test/1, route/4, request/,loop/3]).



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
      Pairs = lists:zip(Prefixes, lists:duplicate(length(Prefixes), Ref)),
      Fun = fun(K, V, AccIn) -> maps:put(K, V, AccIn) end,
      NewRouteMap = maps:fold(Fun, RouteMap, maps:from_list(Pairs)),
      NewStateMap = maps:put(Ref, {Action, Init}, StateMap),
      From ! {ok, Ref},
      loop(State, NewRouteMap, NewStateMap);
    {From, {test}} ->
      From ! {results, RouteMap, StateMap},
      loop(State, RouteMap, StateMap)
  end.

request(Flamingo, Request, From, Ref) ->
  

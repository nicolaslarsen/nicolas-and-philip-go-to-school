-module(flamingo)


-export([new/1, request/4, route/4, drop_group/2])



new(_Global) -> {ok, spawn(flamingo, loop, [_Global, #{}])}

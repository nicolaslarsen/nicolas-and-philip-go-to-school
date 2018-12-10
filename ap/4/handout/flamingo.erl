-module(flamingo).

-export([new/1, request/4, route/4, drop_group/2]).

new(_Global) ->
    nope.

request(_Flamingo, _Request, _From, _Ref) ->
    nope.

route(_Flamingo, _Path, _Fun, _Arg) ->
    nope.

drop_group(_Flamingo, _Id) ->
    not_implemented.

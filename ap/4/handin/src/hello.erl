-module(hello).
-export([server/0]).

hello({_Path, _}, _, _) ->
    {no_change, "Hello my friend"}.

bye({_Path, _}, _, _) ->
    {no_change, "Sad to see you go."}.




server() ->
    {ok, F} = flamingo:new("Hi and Bye Server"),
    flamingo:route(F, ["/hello"], fun hello/3, none),
    flamingo:route(F, ["/goodbye"], fun bye/3, none),
    F.



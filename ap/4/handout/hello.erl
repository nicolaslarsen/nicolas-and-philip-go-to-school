-module(hello).
-export([server/0, try_hello/1, try_bye/1]).

hello({_Path, _}, _, _) ->
    {no_change, "Hello my friend"}.

bye({_Path, _}, _, _) ->
    {no_change, "Sad to see you go."}.




server() ->
    {ok, F} = flamingo:new("Hi and Bye Server"),
    flamingo:route(F, ["/hello"], fun hello/3, none),
    flamingo:route(F, ["/goodbye"], fun bye/3, none),
    F.

try_hello(Server) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/hello", []},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.

try_bye(Server) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/goodbye", []},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.



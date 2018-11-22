-module(greetings).
-export([server/0, try_it/1]).

greeter({_Path, [{"name", Name} | _ ]}, Server, _) ->
    {no_change,
     lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Server])}.

server() ->
    {ok, F} = flamingo:new("The Flamingo Server"),
    flamingo:route(F, ["/hello"], fun greeter/3, none),
    F.

try_it(Server) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/hello", [{"name", "Student"}]},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.

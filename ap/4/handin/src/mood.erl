-module(mood).
-export([server/0, moo/1, mood/1, mooHandler/3]).

mooHandler({_Path, _}, _, LocalState) ->
  case _Path of
    "/moo" -> Content = "That's funny",
              if
                LocalState -> {no_change, Content};
              true -> 
                {new_state, Content, true}
              end;
    "/mood" -> if
                 LocalState -> {no_change, "Happy!"};
               true -> 
                 {no_change, "Sad"}
               end
    end.

    
server() ->
    {ok, F} = flamingo:new("The Flamingo Server"),
    flamingo:route(F, ["/mood", "/moo"], fun mooHandler/3, false),
    F.

moo(Server) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/moo", []},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.


mood(Server) -> 
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/mood", []}, Me, Ref),
    receive
      {Ref, Reply} -> Reply
    end.


-module(mood).
-export([server/0]).

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


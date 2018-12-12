-module(counter).
-export([server/0, countHandler/3, argParser/1]).


argParser([]) -> 1;
argParser([{"x", Val} | Remainder]) -> 
                              %io:fwrite("matched with x"),
                              try 
                                Arg = list_to_integer(Val),
                                if 
                                  is_integer(Arg) and (Arg > 0) -> Arg;
                                true ->
                              %    io:fwrite("made it to the else branch"),
                                    argParser(Remainder)
                                end
                              catch error:badarg ->
                                argParser(Remainder)
                              end;

argParser([_ | Remainder]) -> argParser(Remainder).

countHandler({_Path, Array}, _, LocalState) ->
  Val = argParser(Array),
  case _Path of
    "/inc_with" -> {new_state, integer_to_list(Val + LocalState), Val + LocalState};
    "/dec_with" -> {new_state, integer_to_list(LocalState - Val), LocalState - Val}
  end.



    
server() ->
    {ok, F} = flamingo:new("The Counter Server"),
    flamingo:route(F, ["/inc_with", "/dec_with"], fun countHandler/3, 0),
    F.


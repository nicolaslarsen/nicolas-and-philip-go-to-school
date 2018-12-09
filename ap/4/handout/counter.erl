-module(counter).
-export([server/0, inc/1, inc/2, dec/1, dec/2, countHandler/3]).


argParser([]) -> 1.
argParser([{"x", Val} | Remainder]) -> if
                                  is_integer(Val) and Val > 0 -> Val;
                               true ->
                                  argParser(Remainder)
                               end.

argParser([_ | Remainder) -> argParser(Remainder).

countHandler({_Path, Array, LocalState) ->
  Val = argParser(Array)
  case _Path of
    "/inc_with" -> {new_state, integer_to_list(Val + LocalState), Val + LocalState};
    "/dec_with" -> {new_state, integer_to_list(LocalState - Val), LocalState - Val}
  end.



    
server() ->
    {ok, F} = flamingo:new("The Counter Server"),
    flamingo:route(F, ["/inc_with", "/dec_with"], fun countHandler/3, 0),
    F.

inc(Server) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/inc_with", []},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.


inc(Server, N) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/inc_with", [{"y", "temp"}, {"x", N}]},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.

dec(Server) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/dec_with", []},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.

dec(Server, N) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/dec_with", [{"y", "temp"}, {"x", N}]},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.



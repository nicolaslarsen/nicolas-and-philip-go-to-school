-module(test_server).

-export([test_hello/0, test_mood/0]).
-include_lib("eunit/include/eunit.hrl").


generator_test_() ->
        {inorder,
            [test_greetings(), test_hello(), test_mood(), test_counter()]
        }.

try_route(S, Route, Args) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(S, {Route, Args}, Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.


test_hello() ->
  S = hello:server(),
  [?_assert(try_route(S, "/hello", []) =:= {200, "Hello my friend"}),
  ?_assert(try_route(S, "/goodbye", []) =:= {200, "Sad to see you go."}), 
  ?_assert(try_route(S, "/badroute", []) =:= {404, "There are no matching routes"}), 
  ?_assert(try_route(S, "/hello", [{"x", "blah"}, {"y", "bleh"}]) =:= {200, "Hello my friend"})].

test_mood() ->
  S = mood:server(),
  [?_assert(try_route(S, "/mood", []) =:= {200, "Sad"}),
  ?_assert(try_route(S, "/moo", []) =:= {200, "That's funny"}),
  ?_assert(try_route(S, "/mood", []) =:= {200, "Happy!"}),
  ?_assert(try_route(S, "/moo", []) =:= {200, "That's funny"}),
  ?_assert(try_route(S, "/mood", []) =:= {200, "Happy!"})].

test_greetings() ->
  S = greetings:server(),
  [?_assert(try_route(S, "/hello", [{"name", "Philip"}])  =:= {200, "Greetings Philip\nYou have reached The Flamingo Server"}),
  ?_assert(try_route(S, "/hellooo", [{"name", "Philip"}]) =:= {200, "Greetings Philip\nYou have reached The Flamingo Server"}),
  ?_assert(try_route(S, "/hello", []) =:= {500, "Action failed on the flamingo server"}),
  ?_assert(try_route(S, "/bad", [{"name", "Philip"}]) =:= {404, "There are no matching routes"})].

test_counter() -> 
  S = counter:server(),
  [?_assert(try_route(S, "/inc_with", []) =:= {200, "1"}),
   ?_assert(try_route(S, "/inc_with", [{"x", "5"}]) =:= {200, "6"}),
   ?_assert(try_route(S, "/dec_with", []) =:= {200, "5"}),
   ?_assert(try_route(S, "/dec_with", [{"x", "-1"}]) =:= {200, "4"}),
   ?_assert(try_route(S, "/dec_with", [{"x", "3"}]) =:= {200, "1"})].


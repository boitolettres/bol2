-module( bol_logic_tests ).
-include_lib("eunit/include/eunit.hrl").

put_test_() ->
  B0 = bol_logic:new( {10, 10} ), 
  Position = {1, 1},
  {ok, B1} = bol_logic:put( B0, [{Position, "a"}] ), 
  [ ?_assertMatch( {error, {occupied, Position}},  bol_logic:put( B1, [{Position, "a"}] ) ),  
    ?_assertMatch( {error, {out_of_bounds, _}},  bol_logic:put( B1, [{{10, 1}, "a"}] ) ),
    ?_assertMatch( {error, {out_of_bounds, _}},  bol_logic:put( B1, [{{1, 10}, "a"}] ) ),
    ?_assertMatch( {error, {out_of_bounds, _}},  bol_logic:put( B1, [{{-1, 1}, "a"}] ) ),
    ?_assertMatch( {error, {out_of_bounds, _}},  bol_logic:put( B1, [{{1, -1}, "a"}] ) ) ].

tileAt_test_() ->
  B0 = bol_logic:new( {10, 10} ),
  Position = {1, 1},
  EmptyPosition = {2, 1},
  {ok, B1} = bol_logic:put( B0, [{Position, "a"}] ),
  [ ?_assertEqual( "a", bol_logic:tileAt( B1, Position ) ),
    ?_assertEqual( none, bol_logic:tileAt( B1, EmptyPosition ) )].


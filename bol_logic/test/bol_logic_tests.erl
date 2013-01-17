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

words_test_() ->
  B0 = bol_logic:new( {10, 10} ), 
  Tiles = [{{3, 2}, "d"}, {{3, 3}, "o"}, {{3, 4}, "t"}, {{2, 3}, "g"}, 
           {{4, 3}, "l"}, {{5, 3}, gray}, {{6, 3}, "h"}, {{7, 3}, "i"}],
  {ok, B1} = bol_logic:put( B0, Tiles ), 
  [ ?_assertMatch( [["g", "o", "l"], ["h", "i"]], bol_logic:words( B1, 3, horizontal ) ), 
    ?_assertMatch( [["d", "o", "t"]], bol_logic:words( B1, 3, vertical ) ), 
    ?_assertMatch( [["g", "o", "l"], ["h", "i"], ["d", "o", "t"]], bol_logic:words( B1, Tiles ) ) ]. 

connected_test_() ->
  B0 = bol_logic:new( {10, 10} ), 
  T1 = [{{3, 2}, "d"}, {{3, 3}, "o"}, {{3, 4}, "t"}, {{2, 3}, "g"}, 
        {{4, 3}, "l"}, {{5, 3}, gray}, {{6, 3}, "h"}, {{7, 3}, "i"}],
  T2 = [{{3, 2}, "d"}, {{3, 3}, "o"}, {{3, 4}, "t"}, {{2, 3}, "g"}, 
        {{4, 3}, "l"}, {{5, 3}, gray}],
  {ok, B1} = bol_logic:put( B0, T1 ), 
  {ok, B2} = bol_logic:put( B0, T2 ), 
  bol_logic:connected( B2 ),
  [ ?_assertEqual( false, bol_logic:connected( B1 ) ),
    ?_assertEqual( true, bol_logic:connected( B2 ) )].

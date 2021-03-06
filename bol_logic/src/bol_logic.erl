-module( bol_logic ).
-vsn( "1.0.0" ).
-export( [new/1, move/2, put/2, tileAt/2, size/1, words/3, words/2, connected/1] ).

new( {Width, Height} ) ->
  {{Width, Height}, dict:new()}.

size( {{Width, Height}, _} ) ->
  {Width, Height}.

put( {Bounds, Dict}, [{Position, Value} | Tail] ) ->
  case checkBounds( Bounds, Position ) of
    true ->
      case dict:is_key( Position, Dict ) of
        true  -> {error, {occupied, Position}};
        false -> bol_logic:put( {Bounds, dict:store( Position, Value, Dict )}, Tail )
      end; 
    false ->
      {error, {out_of_bounds, Position}}
  end;

put( Board, [] ) ->
  {ok, Board}.

tileAt( {Bounds, Dict}, Position ) ->
  case checkBounds( Bounds, Position ) of
    true ->
      case dict:find( Position, Dict ) of
        {ok, Value } -> Value;
        error -> none
      end;
    false ->
      {error, {out_of_bounds, Position}}
  end.
  
checkBounds( {Width, Height}, {X, Y} ) ->
  ( X >= 0 ) and ( X < Width ) and ( Y >= 0 ) and ( Y < Height ).

move( Board, Tiles ) ->
  case bol_logic:put( Board, Tiles ) of
    {ok, NewBoard} -> 
      begin
        true
        %move( NewBoard, Tiles, [], [], [] )
      end;
    Other -> Other
  end.

words( Board, Tiles ) -> 
  Horizontal = lists:usort( [X || {{X, _}, _} <- Tiles] ),
  Vertical = lists:usort( [Y || {{_, Y}, _} <- Tiles] ), 
  lists:flatmap( fun( Index ) -> words( Board, Index, horizontal ) end, Horizontal ) ++
  lists:flatmap( fun( Index ) -> words( Board, Index, vertical )  end, Vertical ).

words( Board, Index, Direction ) ->
  {Width, Height} = bol_logic:size( Board ),
  case Direction of
    horizontal ->
      lists:filter( fun( W ) -> length( W ) > 1 end,  words( Board, {Width - 1, Index}, {-1, 0}, [], [] ) );
    vertical ->
      lists:filter( fun( W ) -> length( W ) > 1 end, words( Board, {Index, Height - 1}, {0, -1}, [], [] ) )
  end.

words( Board, {X, Y}, {Dx, Dy}, Current, Result ) ->
  if
    X < 0; Y < 0 ->
      if 
        Current =:= [] -> Result;
        true -> [Current | Result]
      end;
    true ->
      Tile = tileAt( Board, {X, Y} ),
      if 
        Tile =:= none; Tile =:= gray ->
          if 
            Current =:= [] -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, [], Result );
            true -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, [], [Current | Result] )
          end;
        true -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, [Tile | Current], Result )
      end
  end.

connected( Board ) ->
  {_, Dict} = Board,
  [First | Tail] = dict:to_list( Dict ),
  connected( 
    Board, 
    sets:from_list( 
      lists:filter( 
        fun( {_, V} ) -> V =/= gray end, 
        [First | Tail] ) ), 
    sets:new(), [First] ).

connected( _, All, Visited, [] ) -> 
  sets:is_subset( All, Visited ) and sets:is_subset( Visited, All );

connected( Board, All, Visited, [Current | Tail] ) ->
  case sets:is_element( Current, Visited ) of
    true  -> connected( Board, All, Visited, Tail );
    false -> 
        begin
        io:format( "Current: ~p~nNeighbors: ~p~n", [Current, neighbors( Board, Current )]), 
      connected( 
        Board, All, sets:add_element( Current, Visited ), 
        neighbors( Board, Current ) ++ Tail )
       end
  end.

neighbors( Board, {{X, Y}, _} ) ->
  lists:filter( 
    fun( Entry ) -> 
      { _, Val } = Entry,
      case Val of
        none -> false;
        {out_of_bounds, _} -> false;
        gray -> false;
        _ -> true
      end
    end,
    [ {{X - 1, Y}, bol_logic:tileAt( Board, {X - 1, Y} )},
      {{X + 1, Y}, bol_logic:tileAt( Board, {X + 1, Y} )},
      {{X, Y - 1}, bol_logic:tileAt( Board, {X, Y - 1} )},
      {{X, Y + 1}, bol_logic:tileAt( Board, {X, Y + 1} )}]
  ).
  

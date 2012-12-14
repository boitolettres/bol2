-module( bol_logic ).
-vsn( "1.0.0" ).
-export( [new/1, move/2, put/2, tileAt/2, size/1, words/3, words/2] ).

new( {Width, Height} ) ->
  {{Width, Height}, dict:new()}.

size( {{Width, Height}, _} ) ->
  {Width, Height}.

put( {Bounds, Dict}, [{Position, Value} | Tail] ) ->
  io:format( "Value= ~p~n", [Value] ),
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
  case Direction of
    horizontal ->
      lists:filter( fun( W ) -> length( W ) > 1 end,  words( Board, {0, Index}, {1, 0}, [], [] ) );
    vertical ->
      lists:filter( fun( W ) -> length( W ) > 1 end, words( Board, {Index, 0}, {0, 1}, [], [] ) )
  end.

words( Board, {X, Y}, {Dx, Dy}, Current, Result ) ->
  {Width, Height} = bol_logic:size( Board ),
  if
    X >= Width; Y >= Height ->
      if 
        Current =:= [] -> Result;
        true -> Result ++ [Current]
      end;
    true ->
      Tile = tileAt( Board, {X, Y} ),
      if 
        Tile =:= none; Tile =:= gray ->
          if 
            Current =:= [] -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, [], Result );
            true -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, [], Result ++ [Current] )
          end;
        true -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, Current ++ [Tile], Result )
      end
  end.

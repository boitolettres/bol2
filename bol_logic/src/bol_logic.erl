-module( bol_logic ).
-vsn( "1.0.0" ).
-export( [new/1, move/2, put/2, tileAt/2] ).

new( {Width, Height} ) ->
  {{Width, Height}, dict:new()}.

size( {{Width, Height} Dict} ) ->
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

words( Board, Index, Horizontal ) ->
  case Horizontal of
    true ->
      words( Board, {Index, 0}, {1, 0}, [], [] );
    false ->
      words( Board, {0, Index}, {0, 1}, [], [] )
  end.

words( Board, {X, Y}, {Dx, Dy}, [], Result ) ->
  {Width, Height} = size( Board ),
  if
    X >= Dx; Y >= Dy ->
      Result;
    true ->
      case tileAt( Board, {X, Y} ) of
        none -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, [], Result );
        Tile -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, [Tile], Result )
      end
  end;

words( Board, {X, Y}, {Dx, Dy}, Current, Result ) ->
  {Width, Height} = size( Board ),
  if
    X >= Dx; Y >= Dy ->
      [Current | Result]
    true ->
      case tileAt( Board, {X, Y} ) of
        none -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, [], [Current | Result] );
        Tile -> words( Board, {X + Dx, Y + Dy}, {Dx, Dy}, [Tile | Current], Result )
      end
  end.

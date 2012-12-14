-module( board ).
-export( [new/1, put/2, tileAt/2] ).

new( {Width, Height} ) ->
  {{Width, Height}, dict:new()}.

put( {Bounds, Dict}, [{Position, Value} | Tail] ) ->
  io:format( "Value= ~p~n", [Value] ),
  case checkBounds( Bounds, Position ) of
    true ->
      case dict:is_key( Position, Dict ) of
        true  -> {error, {occupied, Position}};
        false -> board:put( {Bounds, dict:store( Position, Value, Dict )}, Tail )
      end; 
    false ->
      {error, {out_of_bounds, Position}}
  end;

put( Board, [] ) ->
  {ok, Board}.

tileAt( {Bounds, Board}, Position ) ->
  case checkBounds( Bounds, Position ) of
    true ->
      case dict:find( Position, Board ) of
        {ok, Value } -> Value;
        error -> none
      end;
    false ->
      {error, {out_of_bounds, Position}}
  end.
  
checkBounds( {Width, Height}, {X, Y} ) ->
  ( X >= 0 ) and ( X < Width ) and ( Y >= 0 ) and ( Y < Height ).


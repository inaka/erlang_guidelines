-module(no_if).

-export([bad/1, better/1, good/1]).

bad(Connection) ->
  {Transport, Version} = other_place:get_http_params(),
  if
    Transport =/= cowboy_spdy, Version =:= 'HTTP/1.1' ->
      [{<<"connection">>, utils:atom_to_connection(Connection)}];
    true ->
      []
  end.


better(Connection) ->
  {Transport, Version} = other_place:get_http_params(),
  case {Transport, Version} of
    {cowboy_spdy, 'HTTP/1.1'} ->
      [{<<"connection">>, utils:atom_to_connection(Connection)}];
    {_, _} ->
      []
  end.
 

good(Connection) ->
  {Transport, Version} = other_place:get_http_params(),
  connection_headers(Transport, Version, Connection).
  
connection_headers(cowboy_spdy, 'HTTP/1.1', Connection) ->
    [{<<"connection">>, utils:atom_to_connection(Connection)}];
connection_headers(_, _, _) ->
    [].

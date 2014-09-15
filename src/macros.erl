-module(macros).

-define(OTHER_MODULE, other_module).
-define(LOG_ERROR(Error),
        error_logger:error_msg(
          "~p:~p >> Error: ~p~n\tStack: ~p",
          [?MODULE, ?LINE, Error, erlang:get_stacktrace()])).

-define(HTTP_CREATED, 201).

-export([bad/0, good/0]).

bad() ->
  try
    ?OTHER_MODULE:some_function(that, may, fail, 201)
  catch
    _:Error ->
      ?LOG_ERROR(Error)
  end.

good() ->
  try
    other_module:some_function(that, may, fail, ?HTTP_CREATED)
  catch
    _:Error ->
      log_error(?LINE, Error)
  end.

log_error(Line, Error) ->
  error_logger:error_msg(
    "~p:~p >> Error: ~p~n\tStack: ~p",
    [?MODULE, Line, Error, erlang:get_stacktrace()]).

-module(test_SUITE).

-export([bad/1, good1/1, good2/1, good3/1]).

bad(_Config) ->
  ct:comment("When input is 0, it should return 0"),
  0 = should:return(0),
  ct:comment("When input is positive, it should return 1"),
  1 = should:return(2),
  ct:comment("When input is negative, it should return -1"),
  -1 = should:return(-100),
  {comment, ""}.

good1(_Config) ->
  ct:comment("When input is 0, it should return 0"),
  0 = should:return(0),
  {comment, ""}.

good2(_Config) ->
  ct:comment("When input is positive, it should return 1"),
  1 = should:return(2),
  {comment, ""}.

good3(_Config) ->
  ct:comment("When input is negative, it should return -1"),
  -1 = should:return(-100),
  {comment, ""}.

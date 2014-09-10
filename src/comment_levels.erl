% this comment is badbad
%%% @doc This comment is good
-module(comment_levels).

-export([bad/0, good/0]).

% @doc This comment is bad
%%% @doc This comment is also bad
bad() ->
  R = 1 + 2, %%% This comment is not good
  R. %% This comment is bad again

%% @doc I like this comment
good() ->
  % This comment is approved by the International Commenting Association
  % and Chuck Norris
  R = 1 + 2,
  R. % This comment (megusta)

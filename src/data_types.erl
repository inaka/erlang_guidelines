-module(data_types).

-export([bad/1, good/1]).

-type your_type() :: {integer(), string()}.
-opaque my_type() :: {binary(), binary()}.
-export_type([your_type/0, my_type/0]).

-spec good(your_type()) -> {ok, my_type()}.
good({I, S}) -> {ok, {integer_to_binary(I), list_to_binary(S)}}.

-spec bad({integer(), string()}) -> {ok, {binary(), binary()}}.
bad({I, S}) -> {ok, {integer_to_binary(I), list_to_binary(S)}}.

-module(record_sharing).

-include("record_sharing.hrl").

-export([bad/0, good/0, good_field/1, good_field/2]).

-record(good, {good_field :: string()}).
-opaque good() :: #good{}.
-export_type([good/0]).

-spec good() -> good().
good() -> #good{}.

-spec good_field(good()) -> string().
good_field(#good{} = Good) -> Good#good.good_field.

-spec good_field(good(), string()) -> good().
good_field(#good{} = Good, Value) -> Good#good{good_field = Value}.

-spec bad() -> #bad{}.
bad() -> #bad{}.

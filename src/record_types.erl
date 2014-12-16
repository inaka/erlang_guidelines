-module(record_types).

-export([records/0]).

-record(bad, {no_type}).

-record(good, {with_type :: string()}).

records() -> [#bad{}, #good{}].

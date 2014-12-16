-module(record_placement).

-export([good/0, bad/0]).

-record(good, {this, record, appears, before, the_functions}).

good() -> [#good{}].

-record(bad, {this, record, appears, below, a_function}).

bad() -> [#bad{}].

-module(record_placement).

-export([good/0, bad/0]).

-record(good, { this_record   :: any()
              , appears       :: any()
              , before        :: any()
              , the_functions :: any()}).

good() -> [#good{}].

-record(bad,  { this_record :: any()
              , appears     :: any()
              , below       :: any()
              , a_function  :: any()}).

bad() -> [#bad{}].

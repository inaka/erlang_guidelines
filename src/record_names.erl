-module(record_names).

-export([records/0]).

-record(badName, {}).
-record(bad_field_name, {badFieldName :: any()}).
-record('UPPERCASE', {'THIS_IS_BAD' :: any()}).

-record(good_name, {good_field_name :: any()}).

records() -> [#badName{}, #bad_field_name{}, #'UPPERCASE'{}, #good_name{}].

-module(record_names).

-export([records/0]).

-record(badName, {}).
-record(bad_field_name, {badFieldName}).
-record('UPPERCASE', {'THIS_IS_BAD'}).

-record(good_name, {good_field_name}).

records() -> [#badName{}, #bad_field_name{}, #'UPPERCASE'{}, #good_name{}].

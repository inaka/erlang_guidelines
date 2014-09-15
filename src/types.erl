-module(types).

-incldue("bad_types.hrl").

-type id() :: pos_integer().

-record(type, {id :: id(), name :: binary()}).
-opaque type() :: #type{}.

-export_type([id/0, type/0]).
%% If you later want to use these types on your specs you DO NOT have to include
%% any file and you just write -spec my_function(types:id()) -> types:type().

-type type_id() :: pos_integer().
-type type() :: proplists:proplist().

%% If you later want to use these types on your specs you have to include this
%% file and write stuff like -spec my_function(type_id()) -> type().

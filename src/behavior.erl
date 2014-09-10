-module(behavior).

-type element() :: binary().
-type id() :: pos_integer().

-export_type([element/0, id/0]).

-callback store(element()) -> id().
-callback retrieve(id()) -> notfound | element().
-callback delete(id()) -> ok.
-callback count() -> non_neg_integer().

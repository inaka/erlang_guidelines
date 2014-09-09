-module(consistency).

-export([bad/1, good/1]).

bad(UserId) -> internal_bad(UserId).

internal_bad(User_Id) -> internal_bad2(User_Id).

internal_bad2(Usr) -> db:get_by_id(Usr).


good(UserId) -> internal_good(UserId).

internal_good(UserId) -> internal_good2(UserId).

internal_good2(UserId) -> db:get_by_id(UserId).

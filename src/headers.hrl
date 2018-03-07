% Bad
-record(nasty_non_encapsulated_record, {
    dont_use_me_directly,
    me_neither
    }).

-type who_knows_what_this_is() :: binary().

cool_function_everyone_uses(Thingy) -> 
    Thingy.

% OK
-define(?COOKIE, <<""Cookie: ">>).

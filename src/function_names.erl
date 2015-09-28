-module(function_names).

-export([badFunction/0, 'BAD_FUNCTION'/0, good_function/0, base64_encode/0]).

badFunction() -> {not_allowed, camel_case}.

'BAD_FUNCTION'() -> {not_allowed, upper_case}.

good_function() -> ok.

base64_encode() -> ok.

-module(atoms).

-export([bad/0, good/0]).

bad() -> ['BAD', alsoBad, bad_AS_well].

good() -> [good, also_good, 'good@its.mail'].

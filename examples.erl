-module(examples).

%%% BAD
-exports([ function1
         , i_like
         , preceding_commas, i_dont, like_them
         ]).

%%% GOOD
-exports([ function1
         , i_like
         , preceding_commas
         , i_dont
         , like_them
         , but_i_respect_your_style
         ]).

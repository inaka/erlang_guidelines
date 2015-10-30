-module(export_all).

-compile(export_all). % Avoid, better to do
% -export([real_fun/0, other_fun/0]).

real_fun()  -> does_something.
other_fun() -> does_something_else.

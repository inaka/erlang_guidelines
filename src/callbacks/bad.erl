-module(bad).

-export([behavior_info/1]).

behavior_info(callbacks) -> [{function1, 2}].

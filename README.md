Erlang Coding Standards & Guidelines
====================================

Suggested reading material: http://www.erlang.se/doc/programming_rules.shtml

***

Table of Contents:
* [Rules & Conventions](#conventions)
  * [Source Code Layout](#source-code-layout)
    * [Spaces over tabs](#spaces-over-tabs)
    * [Use your spacebar](#use-your-spacebar)
    * [80-90 column per line](#80-90-column-per-line)
    * [Maintain existing style](#maintain-existing-style)
    * [Avoid deep nesting](#avoid-deep-nesting)
    * [Group functions logically](#group-functions-logically)
    * [No God modules](#no-god-modules)
    * [Simple unit tests](#simple-unit-tests)
    * [Honor DRY](#honor-dry)
    * [Avoid dynamic calls](#avoid-dynamic-calls)
  * [Naming](#naming)
    * [Be consistent when naming](#be-consistent-when-naming)
  * [Comments](#comments)
  * [Exceptions](#exceptions)
  * [Strings](#strings)
  * [Macros](#macros)
    * [Uppercase Macros](#uppercase-macros)
    * [No module or function name macros](#no-module-or-function-name-macros)
  * [Misc](#misc)
    * [Write function specs](#write-function-specs)
    * [Lock your dependencies](#lock-your-dependencies)
  * [Tools](#tools)
* [Suggestions & Great Ideas](#great-ideas)

## Conventions & Rules

"Things that may be used as reason to reject a Pull Request."

### Syntax & Source Code Layout

Erlang syntax is horrible amirite? So you might as well make the best of it, right? _Right_?

##### rule Spaces over tabs
>  Spaces over tabs, 2 space indentation.

```erlang
% bad (inconsistent)
handle_info(timeout, State=#state{}) ->
  try
  EpisodeOnAir = State#state.on_air,
  OrgId = State#state.org#org.id,
  NextEpisode = conflux_db:episode_on_air(OrgId),
  {ok, StateEpisode, NextStopTime, NewEpiName} =
    case NextEpisode of
      EpisodeOnAir -> {
        ok,
        EpisodeOnAir,
        State#state.next_stop_time,
        State#state.on_air_name
      };
      undefined ->
        % No next episode, so let's see if we should keep this one on the air.
        Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
        if
          Now >= State#state.next_stop_time ->
            handle_cast({stop_on_air}, State),
            {ok, undefined, 0, undefined};
          true ->
            lager:debug(
              "Keeping current episode for another: ~p seconds",
              [State#state.next_stop_time - Now]
            ),

% better but still bad (consistent but 4 spaces)
handle_info({json, InitEvent}, State = #state{status = wait_for_init}) ->
    lager:debug("New init message"),
    NewVersion = State#state.version + 1,
    VersionBin = conflux_utils:integer_to_binary(NewVersion),
    VersionSuffix = integer_to_list(NewVersion) ++ ".json",
    ChangesBin =
        lists:foldr(
            fun({json, Json}, _Acc) -> Json; %%NOTE: Init message renders everything before it invalid
               (Change, Acc) ->
                    try conflux_utils:json_encode(Change) of
                        Json ->
                            case Acc of
                                <<>> -> Json;
                                Acc -> <<Acc/binary, ",\n\t", Json/binary>>
                            end
                    catch
                        _:Error ->
                            lager:error("Comet: Invalid Json: ~p / Error: ~p", [Change, Error]),
                            <<>>
                    end
            end, <<>>, State#state.changes),
    spawn_backup(State),

% good
stop(EpisodeName) ->
  case on_air(EpisodeName) of
    undefined -> ok;
    Pid ->
      gen_server:cast(Pid, stop),
      catch erlang:unregister(EpisodeName),
      ok
  end.
```

*Reasoning*:
  This is *not* intended to allow deep nesting levels in the code. 2 spaces are enough if the code is clean enough, and the code looks more concise, allowing more characters in the same line.

##### Use your spacebar
>  Surround operators and commas with spaces.

```erlang
% bad
my_function(My,Space,Bar)->[is,not,working].

% good
my_function(Hey, Now, It) -> ["works" ++ "again" | [hooray]]].
```

*Reasoning*:
  Again, easier to find / read / etc.

##### 80-90 column per line
>  Stick to 80-90 chars per line, some of us still have to use vi sometimes, specially when editing code via ssh. Also, it allows showing more than one file simultaneously on a wide screen or laptop monitor.

```erlang
% bad
function1([], Arg) ->
  do_something(Arg).
function1(Foo = #rec{field1 = FF1, field2 = FF2, field3 = FF3}, Bar = #rec{field1 = BF1, field2 = BF2, field3 = BF3} | Rest], Arg2) ->
  do_something(FF1, FF2, FF3, BF1, BF2, BF3, function1(Rest, Arg2)).

% good
function1([], Arg) ->
  do_something(Arg).
function1([Foo, Bar | Rest], Arg2) ->
  Foo = #rec{field1 = FF1, field2 = FF2, field3 = FF3},
  Bar = #rec{field1 = BF1, field2 = BF2, field3 = BF3},
  do_something(FF1, FF2, FF3, BF1, BF2, BF3, function1(Rest, Arg2)).
```

##### Maintain existing style
>  When editing a module written by someone else, stick to the style in which it was written. If a project has an overall style, stick to that when writing new modules as well.

```erlang
% bad
-exports([ function1
         , i_like
         , preceding_commas, i_dont, like_them
         ]).

% good
-exports([ function1
         , i_like
         , preceding_commas
         , i_dont
         , like_them
         , but_i_respect_your_style
         ]).
```

*Reasoning*:
  It's better to keep a module that just looks ugly to you than to have a module that looks half ugly to you, half ugly to somebody else.

##### Avoid deep nesting
>  Try not to nest more than 3 levels deep.

```erlang
% bad
function1(Foo) ->
  case Foo of
    true ->
      Bar = do_something(Foo),
      case Bar of
        {ok, WhatIReallyWanted} ->
          try
            InterimValue1 = this_might_blow_up(WhatIReallyWanted),
            InterimValue2 = compute(InterimValue1),
            get_final_value(InterimValue2)
          catch
            _:_ ->
              % dont do this either, let it fail
              io:format("something bad happened")
          end;
        undefined ->
          error(my_application_error)
      end;
    false ->
      io:format("Oh noes!")
  end.

% good
function1(true) ->
  Bar = do_something(Foo),
  really_descriptive_name(Bar);
function1(false) ->
  io:format("Oh noes!").

really_descriptive_name({ok, WhatIReallyWanted}) ->
  try
    InterimValue1 = this_might_blow_up(WhatIReallyWanted),
    InterimValue2 = compute(InterimValue1),
    get_final_value(InterimValue2)
  catch
    _:_ ->
    % dont do this either, let it fail
    io:format("something bad happened")
    end;
really_descriptive_name(undefined) ->
  error(my_application_error).
```

*Reasoning*:
  Nested levels indicate deep logic in a function, too many decisions taken or things done in a single function. This hinders not only readability, but also maintainability (making changes) and debugging, and writing unit tests.

##### Group functions logically
>  Try to always separate **PRIVATE** and **PUBLIC** functions in groups, with the public ones first, unless it helps readability and code discovery.

```erlang
% bad
-module(my_module).
-exports([public1/0, public2/0]).

public1() -> private3(atom1).

private1() -> atom2.

public2() -> private2(private1()).

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.

% not that bad
-module(my_module).
-exports([public1/0, public2/0]).

public1() ->
  case application:get_env(atom_for_public_1) of
    {ok, X} -> public1(X);
    _ -> throw(cant_do)
  end.
public1(X) -> private3(X). % This is a private function but it's obviously related just to the one before

public2() -> private2(private1()).

private1() -> atom2.

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.

% good
-module(my_module).
-exports([public1/0, public2/0]).

public1() ->
  case application:get_env(atom_for_public_1) of
    {ok, X} -> private3(X);
    _ -> throw(cant_do)
  end.

public2() -> private2(private1()).

%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
private1() -> atom2.

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.
```

*Reasoning*:
  Well structured code is easier to read/understand/modify.

##### No God modules
>  Don't design your system using **god**  modules (modules that have a huge number of functions and/or deal with very unrelated things)

##### Simple unit tests
>  Single responsibility applies to tests as well. When writing **unit** tests, keep them short and don't pur more than 1 or 2 asserts per test

##### Honor DRY

This convention is specifically put in this list (instead of treat it as a [great idea](#great-ideas)) so that reviewers can reject PRs that include the same code several times or PRs that re-implement something that they know it's already done somewhere else.

##### Avoid dynamic calls
>  If there is no specific need for it, don't use dynamic function calling.

### Naming

##### Be consistent when naming concepts
>  Use the same variable name for the same concept everywhere (even in different modules).

```erlang
% bad
…
my_function(OrganizationID) -> …
…
my_other_function(OrgID) -> …
…

% good
…
my_function(OrganizationID) -> …
…
my_other_function(OrganizationID) -> …
…
```

*Reasoning*:
  When trying to figure out all the places where an ``OrgID`` is needed (e.g. if we want to change it from ``string`` to ``binary``), it's way easier if we can just grep for ``OrgID`` instead of having to check all possible names.

##### 
>  Stick to one convention when naming modules (i.e: tt_something vs ttsomething vs something).

### Comments

### Exceptions

### Strings

### Macros

##### Uppercase macros
>  Macros should be named in ALL_UPPER_CASE:

```erlang
% bad
-define(?my_macro, '...').
-define(?MYMACRO, '...').
-define(?My_Macro, '...').
-define(?_mY_L33t_M@Cr0, '...').

% good
-define(?MY_MACRO, '...').
-define(?YOUR_MACRO, '...').
```

*Reasoning*:
  It makes it easier not to duplicate macro names, to find them through grep, etc.

##### No module or function name macros
>  Don't use macros for module or function names

```erlang
% bad
function() ->
  ?SM:function(Args).

% good
function() ->
  some_module:function(Args).
```

*Reasoning*:
  Copying lines of code to the console for debugging (something that happens *a lot*) becomes a really hard task if we need to manually replace all the macros.

### Misc

##### Write function specs
>  Write the **-spec**'s for your public fun's, and for private fun's when it adds real value for documentation purposes. Define as many types as needed.

```erlang
-type option_id():: atom().
-type option_count():: non_neg_integer().
-type option_percnt():: non_neg_integer().
-type option():: {option_id(), option_count()}.
-type result():: {option_id(), option_percnt()}.

-spec calc([option()]) -> [result()].
calc(Options) ->
  TotalCounts = [ X || {_,X} <- Options],
  calc(Options, lists:sum(TotalCounts)).er
```

*Reasoning*:
  Dialyzer output is complicated as is, and is improved with good type names. In general, having semantically loaded type names for arguments makes reasoning about possible type failures easier, as well as the function's purpose.

##### Lock your dependencies
>  In your rebar.config or Erlang.mk, specify a tag or commit, but not master.

### Tools

## Suggestions & Great Ideas

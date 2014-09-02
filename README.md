Erlang Coding Standards & Guidelines
====================================

Suggested reading material: http://www.erlang.se/doc/programming_rules.shtml

***

Table of Contents:
* [Rules & Conventions](#rules-conventions)
  * [Source Code Layout](#source-code-layout)
    * [Spaces over tabs](#spaces-over-tabs)
    * [Use your spacebar](#use-your-spacebar)
    * [80-90 column per line](#80-90-column-per-line)
    * [Maintain existing style](#maintain-existing-style)
    * [Avoid deep nesting](#avoid-deep-nesting)
    * [More, smaller functions over case expressions](#more-smaller-functions-over-case-expressions)
    * [Group functions logically](#group-functions-logically)
    * [No God modules](#no-god-modules)
    * [Simple unit tests](#simple-unit-tests)
    * [Honor DRY](#honor-dry)
    * [Avoid dynamic calls](#avoid-dynamic-calls)
    * [Group modules in subdirectories by functionality](#group-modules-in-subdirectories-by-functionality)
    * [Don't write spaghetti code](#dnt-write-spaghetti-code)
  * [Syntax](#syntax)
    * [Avoid if expressions](#avoid-if-expressions)
  * [Naming](#naming)
    * [Be consistent when naming](#be-consistent-when-naming)
    * [Explicit state should be explicitly named](#explicit-state-should-be-explicitly-named)
    * [Don't use _Ignored variables](#dont-use-ignored-variables)
    * [Avoid boolean parameters](#avoid-boolean-parameters)
    * [Stick to one convention for naming modules](#stick-to-one-convention-for-naming-modules)
  * [Design](#design)
    * [Separate your concerns](#separate-your-concerns)
  * [Strings](#strings)
    * [IOLists over string concatenation](#iolists-over-string-concatenation)
  * [Macros](#macros)
    * [Uppercase Macros](#uppercase-macros)
    * [No module or function name macros](#no-module-or-function-name-macros)
  * [Misc](#misc)
    * [Write function specs](#write-function-specs)
    * [Avoid records in specs](#avoid-records-in-specs)
    * [Use -callback attributes over behaviour_info/1]()
    * [Lock your dependencies](#lock-your-dependencies)
  * [Tools](#tools)
    * [Loud errors](#loud-errors)
    * [Properly use logging levels](#properly-use-logging-levels)
* [Suggestions & Great Ideas](#great-ideas)
  * [CamelCase over Under_Score](#camelcase-over-under-score)
  * [Prefer shorter (but still meaningful) variable names](#prefer-shorter-but-still-meaningful-variable-names)
  * [Comment levels](#comment-levels)
  * [Keep functions small](#keep-functions-small)
  * [Keep modules small](#keep-modules-small)
  * [Honor KISS](#honor-kiss)
  * [Help your tools help you](#help-your-tools-help-you)
  * [Commits that remove code are beautiful](#commits-that-remove-code-are-beautiful)
  * [Control header inclusion](#control-header-inclusion)
  * [Use behaviours.](#use-behaviours)
  * [When programming defensively, do so on client side](#when-programming-defensively-do-so-on-client-side)

## Conventions & Rules

"Things that may be used as reason to reject a Pull Request."

### Source Code Layout

***
##### Spaces over tabs
> Spaces over tabs, 2 space indentation.

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

*Reasoning*: This is *not* intended to allow deep nesting levels in the code. 2 spaces are enough if the code is clean enough, and the code looks more concise, allowing more characters in the same line.

***
##### Use your spacebar
> Surround operators and commas with spaces.

```erlang
% bad
my_function(My,Space,Bar)->[is,not,working].

% good
my_function(Hey, Now, It) -> ["works" ++ "again" | [hooray]]].
```

*Reasoning*: Again, easier to find / read / etc.

***
##### 80-90 column per line
> Stick to 80-90 chars per line, some of us still have to use vi sometimes, specially when editing code via ssh. Also, it allows showing more than one file simultaneously on a wide screen or laptop monitor.

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

***
##### Maintain existing style
> When editing a module written by someone else, stick to the style in which it was written. If a project has an overall style, stick to that when writing new modules as well.

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

*Reasoning*: It's better to keep a module that just looks ugly to you than to have a module that looks half ugly to you, half ugly to somebody else.

***

##### Avoid deep nesting
> Try not to nest more than 3 levels deep.

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

*Reasoning*: Nested levels indicate deep logic in a function, too many decisions taken or things done in a single function. This hinders not only readability, but also maintainability (making changes) and debugging, and writing unit tests.

***
##### More, smaller functions over case expressions
> Use pattern-maching in clause functions rather than case's. Specially important if the case is:
> - the top-level expression of the function
> - huge

```erlang
% bad
my_fun(Arg) ->
  case Arg of
    option1 -> process1();
    option2 -> process2()
  end.

my_other_fun(Arg) ->
  …
  case Something of
    option1 ->
      …multiple lines of code…;
    option2 ->
      _multiple lines of code…;
    …many other options…
  end,
  ….

% good
my_fun(option1) -> process1();
my_fun(option2) -> process2().

my_other_fun(Arg) ->
  …
  something_to_do_with(Something),
  ….
```

***
##### Group functions logically
> Try to always separate **PRIVATE** and **PUBLIC** functions in groups, with the public ones first, unless it helps readability and code discovery.

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

*Reasoning*: Well structured code is easier to read/understand/modify.

***
##### No God modules
> Don't design your system using **god**  modules (modules that have a huge number of functions and/or deal with very unrelated things)

***
##### Simple unit tests
> Single responsibility applies to tests as well. When writing **unit** tests, keep them short and don't pur more than 1 or 2 asserts per test

***
##### Honor DRY
> This convention is specifically put in this list (instead of treat it as a [great idea](#great-ideas)) so that reviewers can reject PRs that include the same code several times or PRs that re-implement something that they know it's already done somewhere else.

***
##### Avoid dynamic calls
> If there is no specific need for it, don't use dynamic function calling.

***
##### Group modules in subdirectories by functionality
> When having lots of modules, use subdirectories for them, named with a nice descriptive name for what that "package" does.

Remember to properly configure your ``Emakefile`` and ``rebar.config`` to handle that

***
##### Don't write spaghetti code
> Don't write spaghetti code (A list comprehension with a case inside, or blocks with begin/end, and nested stuff)

```
% bad
Organizations =
    [binary_to_list(Org)
     || Org <- autocomplete_db:smembers(
                case Product of
                    consumer -> <<"organizations">>;
                    spot -> <<"product:", (?SPOTMD_KEY)/binary, ":organizations">>;
                    whisper -> <<"product:", (?WHISPER_KEY)/binary, ":organizations">>
                end)],

% good
Organizations =
  [binary_to_list(Org) || <- Org <- product_organizations(Product)],
```

### Syntax

Erlang syntax is horrible amirite? So you might as well make the best of it, right? _Right_?

***
##### Avoid if expressions
> Don't use `if`.

```erlang
% bad
if
  SomethingIsTrue -> do_something();
  true -> default()
end

% good
case Something of
  an_appropriately_named_thing -> good(not_a_boolean);
  _ -> default()
end
```

*Reasoning*: Clarity of intention and using the right tool for the job.

### Naming

***
##### Be consistent when naming concepts
> Use the same variable name for the same concept everywhere (even in different modules).

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

*Reasoning*: When trying to figure out all the places where an ``OrgID`` is needed (e.g. if we want to change it from ``string`` to ``binary``), it's way easier if we can just grep for ``OrgID`` instead of having to check all possible names.

***
##### Explicit state should be explicitly named
> Name your state records ``#state`` and use ``-type state():: #state{}`` in all your OTP modules.

***
##### Don't use _Ignored variables
> Variables beginning with _ are still variables, and are matched and bound, the _ just keeps the compiler from warning when you don't use them. If you add the _ to a variable's name, don't use it. Say what you mean, mean what you say.

```erlang
% bad
function(_Var) ->
  …
  other_function(_Var),
  …
```

***
##### Avoid boolean parameters
> Don't use boolean parameters (i.e. `true` and `false`) to control clause selection.

```erlang
% bad
square:draw(EdgeLength, true).
square:draw(EdgeLength, false).

% good
square:draw(EdgeLength, full).
square:draw(EdgeLength, empty).
```

*Reasoning*: Clarity of intention and not requiring the reader to check the function definition.

***
##### Stick to one convention for naming modules
> Stick to one convention when naming modules (i.e: tt_something vs ttsomething vs something).

### Design

***
##### Separate your concerns
> Functions should have one clearly enunciated concern.

```
% very bad
process(Path, #request{method='POST', headers=Headers, q=Params}) ->
    ?DEBUG_MSG("Signup params: ~p", [Params]),
    %% required params
    Reqd_Props = ["password", "email"],
    [Password, Emails]
        = Reqd_Values = [proplists:get_value(P, Params) || P <- Reqd_Props],
    Missing_Params = [V || V <- Reqd_Values, V =:= undefined],
    case length(Missing_Params) of
        Missing_Count when Missing_Count > 0 ->
            ?WARN_MSG("Received a signup request with missing params.  Params=~p", [Params]),
            {?HTTP_BAD_REQUEST, ?JSON_HDR, ""};
        _All_Reqd_Params_Present ->
            Opt_Props  = ["phone_number", "first_name", "last_name", "birthday", "gender"],
            [Phones, First, Last, Birth, Gender] = [proplists:get_value(P, Params) || P <- Opt_Props],

            Phone_Numbers = case Phones of undefined -> []; Phones -> string:tokens(Phones, ",") end,
            Email_Addresses = string:tokens(Emails, ","), %% This one is required, can't be undefined

            case account:validate_for_create(?SPOTMD_KEY, Email_Addresses, Phone_Numbers, "") of
                {error, Error_Message} -> {?HTTP_CONFLICT, ?JSON_HDR, "{\"error\": \"" ++ Error_Message ++ "\"}"};
                ok                     ->
                    %% TODO: add the right client record via similar call to regular acct signup
                    case account:create(?SPOTMD_KEY, Email_Addresses, Phone_Numbers, true, ?SIGNUP_SOURCE_WEB_CONSOLE, #client{}) of
                        {created, Token, Xmpp_Password, Resource} ->
                            ?TRACE_HTTP_REQUEST(Token, Path, Headers, Params),
                            Account_Details = #account{token =           Token,
                                                       consumer_key =    ?SPOTMD_KEY,
                                                       consumer_name =   "Spot.MD",
                                                       first_name =      First,
                                                       last_name =       Last,
                                                       birthday  = Birth,
                                                       gender = Gender,
                                                       created_on =      support:now_for_timestamp(),
                                                       unverified_phone_numbers = Phone_Numbers,
                                                       unverified_email_addresses = Email_Addresses,
                                                       time_to_live =    ?DEFAULT_TIME_TO_LIVE,
                                                       account_id =      "0",
                                                       hashed_password = sha2:hexdigest256(Password ++ "0"),
                                                       delete_on_read =  ?DEFAULT_DELETE_ON_READ},
                            account:set_account_details_on_create(Account_Details),
                            account:auto_join_organizations(Account_Details),
                            autocomplete:index_add(Token, account, ?SPOTMD_KEY),
                            Response_Json = "{\"result\": {"
                                "\"token\": \"" ++ Token ++ "\", "
                                "\"resource\": \"" ++ Resource ++ "\", "
                                "\"xmpp_password\": \"" ++ Xmpp_Password ++ "\"}}",
                            ?TRACE_HTTP_RESPONSE(Token, Path, ?HTTP_CREATED, ?JSON_HDR, Response_Json),
                            {?HTTP_CREATED, ?JSON_HDR, Response_Json}
                    end
```

### Strings

***
##### IOLists over string concatenation
> Use iolists instead of string concatenation whenever possible

```erlang
% bad
"/users/" ++ UserId ++ "/events/" ++ EventsId

% good
["/users/", UserId, "/events/", EventsId]
```

*Reasoning*: Performance

### Macros

##### Uppercase macros
> Macros should be named in ALL_UPPER_CASE:

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

*Reasoning*: It makes it easier not to duplicate macro names, to find them through grep, etc.

***
##### No module or function name macros
> Don't use macros for module or function names

```erlang
% bad
function() ->
  ?SM:function(Args).

% good
function() ->
  some_module:function(Args).
```

*Reasoning*: Copying lines of code to the console for debugging (something that happens *a lot*) becomes a really hard task if we need to manually replace all the macros.

### Misc

***
##### Write function specs
> Write the **-spec**'s for your public fun's, and for private fun's when it adds real value for documentation purposes. Define as many types as needed.

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

*Reasoning*: Dialyzer output is complicated as is, and is improved with good type names. In general, having semantically loaded type names for arguments makes reasoning about possible type failures easier, as well as the function's purpose.

***
##### Avoid records in specs
> Avoid using records in your specs, use types.

```erlang
% bad
-record(state, {field1, field2}).

-spec function(#state{}) -> #state{}.
function(State) ->
 % ... do something,
 NewState.

% good
-record(state, {field1, field2}).
-type state() :: #state{}.

-spec function(state())) -> state()).
function(State) ->
 % ... do something,
 NewState.
```

***
##### Use -callback attributes over behaviour_info/1.
> Unless you know your project will be compiled with R14 or lower, use ``-callback`` instead of ``behavior_info/1`` for your behavior definitions. In general, avoid deprecated functionality.

***
##### Lock your dependencies
> In your rebar.config or Erlang.mk, specify a tag or commit, but not master.

### Tools

***
##### Loud errors
> Don't let errors and exceptions go unlogged. Even when you handle them, write a log line with the stack trace so that somebody watching it can understand what's happening

***
##### Properly use logging levels
> When using lager, use the different logging levels with the following meanings:

*Meanings*:
  * ``debug``: Very low-level info, that may cover your screen and don't let you type in it :P
  * ``info``: The system's life, in some detail. Things that happen usually, but not all the time. You should be able to use the console with acceptable interruptions in this level.
  * ``notice``: Meaningful things that are worth noticing, like the startup or termination of supervisors or important gen_servers, etc…
  * ``warning``: Handled errors, the system keeps working as usual, but something out of the ordinary happened
  * ``error``: Something bad and unexpected happen, usually an exception or error (**DO** log the **stack trace** here)
  * ``critical``: The system (or a part of it) crashed and somebody should be informed and take action about it
  * ``alert``:
  * ``emergency``:

## Suggestions & Great Ideas

Things that should be considered when writing code, but do not cause a PR rejection.

***
##### CamelCase over Under_Score
> Symbol naming: Use variables in CamelCase and atoms, function and module names with underscores.

```erlang
% bad
Variable_Name = functionName(atomName).

% good
VariableName = function_name(atom_name).
```

*Reasoning*: It helps a lot with the next issue in this list

***
##### Prefer shorter (but still meaningful) variable names
> As long as it's easy to read and understand, keep variable names short

```erlang
% bad
OrganizationToken, OID

% good
OrgID
```

*Reasoning*: It helps reducing line lengths, which is also described below

***
##### Comment levels
> Module comments go with **%%%**, function comments with **%%**, and code comments with **%**.

```erlang
% bad
% @doc My module
-module(my_module).

% @doc My function
my_function() -> ok. %% yeah! it returns ok

% good
%%% @doc My module
-module(my_module).

%% @doc My function
my_function() -> ok. % yeah! it returns ok
```

*Reasoning*: It clearly states what the comment is about, also helpful to search for specific comments, like "%% @".

***
##### Keep functions small
> Try to write functions with a small number of lines. **12** lines per function except for integration tests is a good measure.

***
##### Keep modules small
> If a module is growing too much (because devs keep adding functionality to it), consider splitting it into several smaller modules that handle groups of related functionality

***
##### Honor KISS
> And don't over-engineer.

***
##### Help your tools help you
> Help ``dialyzer`` and ``xref`` as much as you can, so that they can work for you

***
##### Commits that remove code are beautiful
> Less is more. We value negative commits (those with more deletions than additions). Strive to attain the zen of the codeless code.

***
##### Control header inclusion
> When having many _nested_ "include files", use -ifndef(HEADER_FILE_HRL) .... -endif so they can be included in any order without conflicts.

***
##### Use behaviours.
> Encapsulate reusable code in behaviors.

***
##### When programming defensively, do so on client side
One aspect of choosing where want you to crash is how you design your API:
do_it(Pid, X) when is_integer(X) -> gen_server:call(Pid, {do_it, X}).
If you design this way, the caller crashes if the arg is wrong.
If you don't tighten up the function head, the gen_server will crash.


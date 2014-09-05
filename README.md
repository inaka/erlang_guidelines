Erlang Coding Standards & Guidelines
====================================

Suggested reading material: http://www.erlang.se/doc/programming_rules.shtml

***

Table of Contents:
* [Conventions & Rules](#conventions--rules)
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
    * [Don't write spaghetti code](#dont-write-spaghetti-code)
  * [Syntax](#syntax)
    * [Avoid if expressions](#avoid-if-expressions)
  * [Naming](#naming)
    * [Be consistent when naming](#be-consistent-when-naming-concepts)
    * [Explicit state should be explicitly named](#explicit-state-should-be-explicitly-named)
    * [Don't use _Ignored variables](#dont-use-_ignored-variables)
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
    * [Use -callback attributes over behaviour_info/1](use--callback-attributes-over-behaviour_info1)
    * [Lock your dependencies](#lock-your-dependencies)
  * [Tools](#tools)
    * [Loud errors](#loud-errors)
    * [Properly use logging levels](#properly-use-logging-levels)
* [Suggestions & Great Ideas](#suggestions--great-ideas)
  * [CamelCase over Under_Score](#camelcase-over-under_score)
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

*Examples*: [indent](src/indent.erl)

*Reasoning*: This is *not* intended to allow deep nesting levels in the code. 2 spaces are enough if the code is clean enough, and the code looks more concise, allowing more characters in the same line.

***
##### Use your spacebar
> Surround operators and commas with spaces.

*Examples*: [spaces](src/spaces.erl)

*Reasoning*: Again, easier to find / read / etc.

***
##### 80-90 column per line
> Stick to 80-90 chars per line, some of us still have to use vi sometimes, specially when editing code via ssh. Also, it allows showing more than one file simultaneously on a wide screen or laptop monitor.

*Examples*: [col_width](src/col_width.erl)

*Reasoning*: Not having to scroll horizontally while editing is a HUGE gain. Also, in wider screens you can open two files: one beside the other.

***
##### Maintain existing style
> When editing a module written by someone else, stick to the style in which it was written. If a project has an overall style, stick to that when writing new modules as well.

*Examples*: [existing_style](src/existing_style.erl)

*Reasoning*: It's better to keep a module that just looks ugly to you than to have a module that looks half ugly to you, half ugly to somebody else.

***

##### Avoid deep nesting
> Try not to nest more than 3 levels deep.

*Examples*: [nesting](src/nesting.erl)

*Reasoning*: Nested levels indicate deep logic in a function, too many decisions taken or things done in a single function. This hinders not only readability, but also maintainability (making changes) and debugging, and writing unit tests.

***
##### More, smaller functions over case expressions
> Use pattern-maching in clause functions rather than case's. Specially important if the case is:
> - the top-level expression of the function
> - huge

*Examples*: [smaller_functions](src/smaller_functions.erl)

*Reasoning:* it is usually the case that a case in a function body represents some sort of decision, and functions should be as simple as possible. If each branch of a decision's outcome is implemented as a function clause instead of as a case clause, the decision may be given a meaningful name. In other words, the case is acting as an 'anonymous function', which unless they are being used in the context of a higher-order function, merely obscure meaning.

***
##### Group functions logically
> Try to always separate **PRIVATE** and **PUBLIC** functions in groups, with the public ones first, unless it helps readability and code discovery.

*Examples*: [grouping_functions](src/grouping_functions)

*Reasoning*: Well structured code is easier to read/understand/modify.

***
##### No God modules
> Don't design your system using **god**  modules (modules that have a huge number of functions and/or deal with very unrelated things)

*Examples*: [god](src/god.erl)

*Reasoning*: God modules, like god objects, are modules that do too much or know too much. God modules usually come into existence by feature accretion. A beautiful, to-the-point module with one job, one responsibility done well, gains a function. Then another, which does the same thing but with different parameters. Then one day, you have a 6000-line module with 500 functions. Having modules (and functions) that do one and only one thing well makes it easy to explore and reason about code, and thus maintain it.

***
##### Simple unit tests
> Single responsibility applies to tests as well. When writing **unit** tests, keep them short and don't put more than 1 or 2 asserts per test

*Examples*: [test_SUITE](src/test_SUITE.erl)

*Reasoning*: Multiple tests can identify multiple errors in one run, if you put all the things you want to test into one test you'll have to fix one thing at a time until the test passes.

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
                case Client of
                    weiland_yutani -> <<"weiland_yutani:organizations">>;
                    aperture_science -> <<"client:", (?AP_SC_KEY)/binary, ":orgs">>;
                    wayne_enterprises -> <<"client:", (?WAY_ENT_KEY)/binary", ":organisations">>
                end)],

% good
Organizations =
  [binary_to_list(Org) || <- Org <- client_bin_key(Client)],
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
> Stick to one convention when naming modules (i.e: ik_something vs iksomething vs something).

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

***
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

Things that should be considered when writing code, but do not cause a PR rejection, or are too vague to consistently enforce.

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

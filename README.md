Erlang Coding Standards & Guidelines
====================================

Suggested reading material: http://www.erlang.se/doc/programming_rules.shtml

***

Table of Contents:
* [Rules & Conventions](#conventions)
  * [Source Code Layout]()
    * [Maintain existing style](#maintain-existing-style)
    * [Use your spacebar](#use-your-spacebar)
    * [80-90 column per line](#80-90-column-per-line)
    * [Avoid deep nesting](#avoid-deep-nesting)
  * [Syntax](#syntax)
  * [Naming](#naming)
    * [Be consistent when naming](#be-consistent-when-naming)
  * [Comments](#comments)
  * [Exceptions](#exceptions)
  * [Strings](#strings)
  * [Macros](#macros)
    * [Uppercase Macros](#uppercase-macros)
    * [No module or function name macros](#no-module-or-function-name-macros)
  * [Misc](#misc)
  * [Tools](#tools)
* [Suggestions & Great Ideas](#great-ideas)

## Conventions & Rules

"Things that may be used as reason to reject a Pull Request."

### Source Code Layout

Erlang syntax is horrible amirite? So you might as well make the best of it, right? Right?

##### _Maintain existing style_
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

### Syntax

### Naming

##### Be consistent when naming
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

### Tools

## Suggestions & Great Ideas





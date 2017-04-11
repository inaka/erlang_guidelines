
An individual Erlang application has the following structure when it is meant to conform to OTP conventions:

```
    $APP_ROOT
     |
     -- ebin/          % Contains all generated .beam files.
     -- include/       % Contains all public .hrl files.
     -- priv/          % Contains all other artifacts.
     -- src/           % Contains all .erl files and potentially private
     |                 % .hrl files.
     -- test/          % Contains all EUnit test files.
```

You should use this "flat" directory structure if your project is a simple Erlang application (e.g. a generic component which might be used in other projects) or a library application.

If your system consists of several applications working together, the umbrella project structure should be used,  in which the system root contains a folder named `apps` which will contain the individual application folders. Each application can then have its own `ebin`, `include`, `src` and `test` folders, and the entire project can share resources in the `priv` folder, and dependencies can be downloaded and compiled once for the entire project.
 
```
    $PROJECT_ROOT
     |
     -- $SYSTEM_ROOT/               % Main system directory.
          |
          -- config/                % Contains all system configuration
          |                         % files and templates.
          -- deps/                  % Contains external applications which
          |    |                    % are used as part of the system.
          |    |
          |    -- some_dep_app/
          -- apps/                  % Contains all applications which are
          |    |                    % part of the system.
          |    |
          |    -- some_app/
          |    -- another_app/
          -- patches/               % Contains all patches to Erlang/OTP
          |                         % which are custom to this system.
          -- priv/                  % Contains all other artifacts.
```

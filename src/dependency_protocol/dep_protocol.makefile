```makefile
PROJECT = myproject

DEPS = cowboy lager jiffy
# bad
dep_lager = git https://github.com/basho/lager.git master
dep_jiffy = git https://github.com/davisp/jiffy.git master

# good
dep_lager = git git://github.com/basho/lager.git 2.0.1
dep_jiffy = git git://github.com/davisp/jiffy.git 0.13.3

```

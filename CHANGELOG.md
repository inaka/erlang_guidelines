# Change Log

## [1.2](https://github.com/inaka/erlang_guidelines/tree/1.2) (2018-03-15)
[Full Changelog](https://github.com/inaka/erlang_guidelines/compare/1.1...1.2)

**Implemented enhancements:**

- Explicit state should be explicitly annotated with a module prefix [\#64](https://github.com/inaka/erlang_guidelines/pull/64) ([ttyerlsol](https://github.com/ttyerlsol))

**Fixed bugs:**

- Remove NOTE about rebar from Subdirectory rule [\#48](https://github.com/inaka/erlang_guidelines/pull/48) ([elbrujohalcon](https://github.com/elbrujohalcon))

**Closed issues:**

- Move from Erlang.mk to rebar3 [\#79](https://github.com/inaka/erlang_guidelines/issues/79)
- vim syntax plugin? [\#69](https://github.com/inaka/erlang_guidelines/issues/69)
- Don't leave debugging calls in your source code [\#55](https://github.com/inaka/erlang_guidelines/issues/55)
- Spaces per indentation [\#52](https://github.com/inaka/erlang_guidelines/issues/52)
- Change recommendation of git protocol to https for dependency download [\#51](https://github.com/inaka/erlang_guidelines/issues/51)
- How do you guys get your funs indented only 2 characters? [\#49](https://github.com/inaka/erlang_guidelines/issues/49)
- Suggestion: Separate responsibilities in sumo\_db. [\#40](https://github.com/inaka/erlang_guidelines/issues/40)
- Project Structure [\#30](https://github.com/inaka/erlang_guidelines/issues/30)
- Header files: 'should not include' and 'could include' [\#20](https://github.com/inaka/erlang_guidelines/issues/20)

**Merged pull requests:**

- Perform a cleanup round on the guidelines [\#85](https://github.com/inaka/erlang_guidelines/pull/85) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Removed copypasta [\#84](https://github.com/inaka/erlang_guidelines/pull/84) ([igaray](https://github.com/igaray))
- \[FIXES \#20\] Add header guidelines [\#83](https://github.com/inaka/erlang_guidelines/pull/83) ([igaray](https://github.com/igaray))
- Do not use timer:sleep/1 in tests [\#82](https://github.com/inaka/erlang_guidelines/pull/82) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Remove dead hipchat link [\#81](https://github.com/inaka/erlang_guidelines/pull/81) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Close \#79\] Move from Erlang.mk to rebar3 [\#80](https://github.com/inaka/erlang_guidelines/pull/80) ([Euen](https://github.com/Euen))
- Added link to xref to readme file [\#78](https://github.com/inaka/erlang_guidelines/pull/78) ([LuboVarga](https://github.com/LuboVarga))
- Avoid recursion [\#77](https://github.com/inaka/erlang_guidelines/pull/77) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Basho upgrades [\#76](https://github.com/inaka/erlang_guidelines/pull/76) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Basho upgrades [\#75](https://github.com/inaka/erlang_guidelines/pull/75) ([elbrujohalcon](https://github.com/elbrujohalcon))
- should be its [\#72](https://github.com/inaka/erlang_guidelines/pull/72) ([getong](https://github.com/getong))
- Exit better than throw [\#71](https://github.com/inaka/erlang_guidelines/pull/71) ([lucafavatella](https://github.com/lucafavatella))
- Throw is not loud error [\#70](https://github.com/inaka/erlang_guidelines/pull/70) ([lucafavatella](https://github.com/lucafavatella))
- Fixed case of variable `Arg` in dyn\_calls [\#68](https://github.com/inaka/erlang_guidelines/pull/68) ([clarete](https://github.com/clarete))
- \[Fix \#55\] Add rule not to leave debug calls around [\#66](https://github.com/inaka/erlang_guidelines/pull/66) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Add Templates for PRs and Issues [\#65](https://github.com/inaka/erlang_guidelines/pull/65) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Add Radek's blog post link [\#63](https://github.com/inaka/erlang_guidelines/pull/63) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Missing horizontal line [\#62](https://github.com/inaka/erlang_guidelines/pull/62) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix compilation issues in \#60 [\#61](https://github.com/inaka/erlang_guidelines/pull/61) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Avoid export all [\#60](https://github.com/inaka/erlang_guidelines/pull/60) ([kbaird](https://github.com/kbaird))
- No implicit included functions when using mixer [\#59](https://github.com/inaka/erlang_guidelines/pull/59) ([jfacorro](https://github.com/jfacorro))
- Make it explicit that digits are allowed in function names - README.m… [\#58](https://github.com/inaka/erlang_guidelines/pull/58) ([kbaird](https://github.com/kbaird))
- Prefer Pattern Matching over Equality [\#57](https://github.com/inaka/erlang_guidelines/pull/57) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Closes \#51\] Update protocol recommendation [\#54](https://github.com/inaka/erlang_guidelines/pull/54) ([igaray](https://github.com/igaray))
- Updated license [\#53](https://github.com/inaka/erlang_guidelines/pull/53) ([spike886](https://github.com/spike886))
- Create LICENSE [\#50](https://github.com/inaka/erlang_guidelines/pull/50) ([andresinaka](https://github.com/andresinaka))
- \[\#40\] separate responsibilities in sumo\_db [\#47](https://github.com/inaka/erlang_guidelines/pull/47) ([Euen](https://github.com/Euen))

## [1.1](https://github.com/inaka/erlang_guidelines/tree/1.1) (2014-12-30)
**Fixed bugs:**

- 'Move stuff to independent applications' Is not in the table of contents [\#41](https://github.com/inaka/erlang_guidelines/pull/41) ([HernanRivasAcosta](https://github.com/HernanRivasAcosta))

**Closed issues:**

- Using length/1 might be a red flag [\#28](https://github.com/inaka/erlang_guidelines/issues/28)
- Prefer the git protocol over others when specifying dependency URLs [\#27](https://github.com/inaka/erlang_guidelines/issues/27)
- Remove elvis from the list of dependencies [\#25](https://github.com/inaka/erlang_guidelines/issues/25)
- When making a library, make sure all API functions reside in one module [\#19](https://github.com/inaka/erlang_guidelines/issues/19)
- Change lines by expressions [\#17](https://github.com/inaka/erlang_guidelines/issues/17)
- README.md structure [\#5](https://github.com/inaka/erlang_guidelines/issues/5)
- REJECTED.md [\#4](https://github.com/inaka/erlang_guidelines/issues/4)
- Migrate data from inaka's private list of standards [\#3](https://github.com/inaka/erlang_guidelines/issues/3)
- Create CONTRIBUTING.md [\#2](https://github.com/inaka/erlang_guidelines/issues/2)

**Merged pull requests:**

- Revert "\[Closes \#30\] Added project structure guideline" [\#45](https://github.com/inaka/erlang_guidelines/pull/45) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Added the facade pattern examples and reasoning [\#44](https://github.com/inaka/erlang_guidelines/pull/44) ([HernanRivasAcosta](https://github.com/HernanRivasAcosta))
- Added the reasoning [\#43](https://github.com/inaka/erlang_guidelines/pull/43) ([HernanRivasAcosta](https://github.com/HernanRivasAcosta))
- Added examples and logic for issue 28 [\#42](https://github.com/inaka/erlang_guidelines/pull/42) ([HernanRivasAcosta](https://github.com/HernanRivasAcosta))
- \[Closes \#30\] Added project structure guideline [\#39](https://github.com/inaka/erlang_guidelines/pull/39) ([igaray](https://github.com/igaray))
- Modified the contributing doc to conform to current rule format [\#38](https://github.com/inaka/erlang_guidelines/pull/38) ([igaray](https://github.com/igaray))
- \[Closes \#27\] Prefer git protocol for deps [\#37](https://github.com/inaka/erlang_guidelines/pull/37) ([igaray](https://github.com/igaray))
- Atom names [\#36](https://github.com/inaka/erlang_guidelines/pull/36) ([jfacorro](https://github.com/jfacorro))
- Variable Names [\#35](https://github.com/inaka/erlang_guidelines/pull/35) ([jfacorro](https://github.com/jfacorro))
- Function names [\#34](https://github.com/inaka/erlang_guidelines/pull/34) ([jfacorro](https://github.com/jfacorro))
- Record Definition Placement [\#33](https://github.com/inaka/erlang_guidelines/pull/33) ([jfacorro](https://github.com/jfacorro))
- Types in exported functions [\#32](https://github.com/inaka/erlang_guidelines/pull/32) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Don't import [\#31](https://github.com/inaka/erlang_guidelines/pull/31) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Get your types together! [\#29](https://github.com/inaka/erlang_guidelines/pull/29) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[fix \#25\] Remove elvis from dependencies. [\#26](https://github.com/inaka/erlang_guidelines/pull/26) ([Euen](https://github.com/Euen))
- Rule: no nested try…catch blocks [\#24](https://github.com/inaka/erlang_guidelines/pull/24) ([elbrujohalcon](https://github.com/elbrujohalcon))
- No Trailing Whitespace [\#23](https://github.com/inaka/erlang_guidelines/pull/23) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Update README.md [\#22](https://github.com/inaka/erlang_guidelines/pull/22) ([igaray](https://github.com/igaray))
- 80 is the de-facto standard [\#21](https://github.com/inaka/erlang_guidelines/pull/21) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Provide examples in .erl files [\#18](https://github.com/inaka/erlang_guidelines/pull/18) ([igaray](https://github.com/igaray))
- Suggestion: Move stuff to independent applications [\#16](https://github.com/inaka/erlang_guidelines/pull/16) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Replace PRIVATE and PUBLIC by EXPORTED and… humm… not exported? [\#15](https://github.com/inaka/erlang_guidelines/pull/15) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Provide examples in .erl files [\#13](https://github.com/inaka/erlang_guidelines/pull/13) ([igaray](https://github.com/igaray))
- Missing separator. [\#12](https://github.com/inaka/erlang_guidelines/pull/12) ([jfacorro](https://github.com/jfacorro))
- Igaray.toc [\#11](https://github.com/inaka/erlang_guidelines/pull/11) ([igaray](https://github.com/igaray))
- \[Closes \#2\] Added CONTRIBUTING.md [\#10](https://github.com/inaka/erlang_guidelines/pull/10) ([igaray](https://github.com/igaray))
- \[Closes \#4\] Added REJECTED.md [\#9](https://github.com/inaka/erlang_guidelines/pull/9) ([igaray](https://github.com/igaray))
- Suggestion: use lists:foreach or LC instead of lists:map [\#8](https://github.com/inaka/erlang_guidelines/pull/8) ([igaray](https://github.com/igaray))
- Suggestion: Use Macros appropriately [\#7](https://github.com/inaka/erlang_guidelines/pull/7) ([igaray](https://github.com/igaray))
- Suggestion: Do not share types by including them in hrl files. [\#6](https://github.com/inaka/erlang_guidelines/pull/6) ([igaray](https://github.com/igaray))
- Provide examples in .erl files [\#1](https://github.com/inaka/erlang_guidelines/pull/1) ([elbrujohalcon](https://github.com/elbrujohalcon))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
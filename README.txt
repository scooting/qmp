This tool translates lists of files trapped in QMake .pro files to literal list
syntax for either CMake or Lua.

qmp hasn't been tested with arbitrary QMake files. It has mainly been used to
extract file lists out of Qt 5.15's own .pro files, with the goal of outright
replacing Qt's build system with something more controllable and flexible.

You'll need OCaml 4.13.0 or newer. A build script, named build.bash, is
provided; it is simple enough to manually transliterate to your build tool or
command interpreter of choice.

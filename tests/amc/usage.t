Using amc with the help flag displays all sub-command and the top-level options.

  $ amc --help
  Usage: amc [COMMAND] [-v|--version]
    The Amulet compiler and REPL, version [0-9.]+ \(\w+\)(re)
  
  Available options:
    -h,--help                Show this help text
    -v,--version             Show version information
  
  Available commands:
    compile                  Compile an Amulet file to Lua.
    repl                     Launch the Amulet REPL.
    connect                  Connect to an already running REPL instance.

Each sub-command can be run with --help too.

  $ amc compile --help
  Usage: amc compile FILE [-o|--out FILE] [-O|--opt LEVEL] [--export] [--lib ARG]
    Compile an Amulet file to Lua.
  
  Available options:
    FILE                     The file to compile.
    -o,--out FILE            Write the generated Lua to a specific file.
    -O,--opt LEVEL           Controls the optimisation level. (default: 1)
    --export                 Export all declared variables in this module,
                             returning them at the end of the program.
    -t,--test                Provides additional debug information on the output
    --test-tc                Provides additional type check information on the
                             output
    --lib ARG                Add a folder to the library path
    --core-lint              Verify that Amulet's intermediate representation is
                             well-formed. This is an internal debugging flag, and
                             should only be used if you suspect there is a bug in
                             Amulet.
    -h,--help                Show this help text

  $ amc repl --help
  Usage: amc repl [FILE] [--port PORT] [--no-prelude | --prelude PATH] [--lib ARG]
    Launch the Amulet REPL.
  
  Available options:
    FILE                     A file to load into the REPL.
    --port PORT              Port to use for the REPL server. (default: 5478)
    --no-prelude             Do not load files with a prelude.
    --prelude PATH           Specify a custom prelude to use.
    -t,--test                Provides additional debug information on the output
    --test-tc                Provides additional type check information on the
                             output
    --lib ARG                Add a folder to the library path
    --core-lint              Verify that Amulet's intermediate representation is
                             well-formed. This is an internal debugging flag, and
                             should only be used if you suspect there is a bug in
                             Amulet.
    -h,--help                Show this help text

  $ amc connect --help
  Usage: amc connect COMMAND [--port PORT]
    Connect to an already running REPL instance.
  
  Available options:
    COMMAND                  The command to run on the remote REPL.
    --port PORT              Port the remote REPL is hosted on. (default: 5478)
    -h,--help                Show this help text

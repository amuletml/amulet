Amulet is no longer under development. If you're interested in other languages
which compile to Lua, do check out
[this list](https://github.com/hengestone/lua-languages).

# Amulet

Amulet is a simple functional programming language in the ML tradition,
with support for higher-rank parametric polymorphic types and
row-polymorphic records, pattern matching with algebraic data types, and
tail recursion elimination.

[![Build Status](https://shamiko.amulet.works/job/amulet/job/master/badge/icon)](https://shamiko.amulet.works/job/amulet/job/master/)

# Installation

[Installation
instructions](https://amulet.works/tutorials/00-installing.html)

For your convenience, we provide pre-built tarballs for x86_64 Linux
that are compiled every night from the `master` branch (as part of the
nightly build job, linked above). Install them with the magic install
script:

```
curl https://amulet.works/install.sh | env CHANNEL=nightly sh -
```

# Building from Source

Make sure you have a functional Haskell toolchain including Stack. Since
Amulet has a lot of object files (around 125 for the `amc` executable,
not counting all of the Haskell dependencies that are shipped as `.a`
static libraries), we use the LLVM `lld` linker in the Cabal file.

If you do not wish to use `lld`, remove all the `ghc-options:` stanzas
from the `amuletml.cabal` file; This sed command should do the trick,
too.

```
sed -re 's;-optl-fuse-ld=lld;;g' -i amuletml.cabal
```

Amulet ships with its own set of libraries. Make sure they are
installed. `amc` will look for its libraries in one of the following
directories:

```bash
${AMC_LIBRARY_PATH}
$(dirname $(which amc))/lib
$(dirname $(which amc))/../lib
```

The script `tools/quickinstall.sh` will take care of building and
installing the libraries, using Stack.

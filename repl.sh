#!/usr/bin/env bash

set -e

if [ -z ${IN_NIX_SHELL+a} ]; then
  echo -e "This program should be run as \x1b[1;31mnix-shell . --run '$0'\x1b[0m."
  exit 1
fi

WARN="-Wextra -Wall -Wno-name-shadowing -Wno-implicit-prelude -Wno-missing-import-lists -Wredundant-constraints -Wmonomorphism-restriction"

# build lexers
find src/ -type f -name '*.x' | while read -r fname; do
  source_mtime=$(stat -c %Y $fname)
  out_name="${fname%.x}.hs"
  if [[ ! -f $out_name ]]; then
    alex $fname
  elif [[ $source_mtime > $(stat -c %Y $out_name) ]]; then
    alex $fname
  else
    echo "already built lexer $fname"
  fi
done

# build parsers
find src/ -type f -name '*.y' | while read -r fname; do
  source_mtime=$(stat -c %Y $fname)
  out_name="${fname%.y}.hs"
  if [[ ! -f $out_name ]]; then
    happy -g -c $fname
  elif [[ $source_mtime > $(stat -c %Y $out_name) ]]; then
    happy -g -c $fname
  else
    echo "already built parser $fname"
  fi
done

if [[ $1 == "reload" ]]; then
  exit 0
fi


echo "Loading ghci.."

exec ghci $WARN -i./src/:./compiler/ ./compiler/Main.hs $@

#!/usr/bin/env bash

set -e

WARN="-Wextra -Wall -Wno-name-shadowing -Wno-implicit-prelude -Wno-missing-import-lists -Wredundant-constraints"

out_dir=".stack-work/amulet"

# build lexers
find src/ -type f -name '*.x' | while read -r fname; do
  source_mtime=$(stat -c %Y $fname)
  out_name="$out_dir/${fname%.x}.hs"
  mkdir -p $(dirname $out_name)
  if [[ ! -f $out_name ]]; then
    stack exec -- alex -o $out_name $fname
  elif [[ $source_mtime > $(stat -c %Y $out_name) ]]; then
    stack exec -- alex -o $out_name $fname
  else
    echo "already built lexer $fname"
  fi
done

# build parsers
find src/ -type f -name '*.y' | while read -r fname; do
  source_mtime=$(stat -c %Y $fname)
  out_name="$out_dir/${fname%.y}.hs"
  mkdir -p $(dirname $out_name)
  if [[ ! -f $out_name ]]; then
    stack exec -- happy -g -c -o $out_name $fname
  elif [[ $source_mtime > $(stat -c %Y $out_name) ]]; then
    stack exec -- happy -g -c -o $out_name $fname
  else
    echo "already built parser $fname"
  fi
done

if [[ $1 == "reload" ]]; then
  exit 0
fi

echo "Loading ghci.."

exec stack exec -- ghci -O0 -j2 +RTS -A128M -RTS -fhide-source-paths $WARN -i./src/:$out_dir/src/:./compiler/ ./compiler/Main.hs $@

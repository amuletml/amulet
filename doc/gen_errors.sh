#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

while IFS= read -r line; do
  path=$PWD/errors/$(echo $line | cut -d' ' -f1)
  if [[ -f $path ]]; then
    continue
  fi
  mkdir -p $(dirname $path)
  printf "%s\n" "$(echo $line | cut -d' ' -f2-)" > $path
done < errors.txt

#!/usr/bin/env bash

# Check hlint style only on changed files. Useful as a pre-commit hook:
#   ln -s $PWD/tools/lint.sh .git/hooks/pre-commit

set -e

check() {
  git status --porcelain | while read -r file; do
    if echo $file | grep -E '(M|\?\?|A)' | grep -E '.hs$' &>/dev/null; then
      hlint $(echo $file | cut -d' ' -f2) || true
    fi
  done
}

if check | grep -v 'No hints' | grep '.'; then
  exit 1
else
  exit 0
fi

#!/usr/bin/env bash

# This half: Amulet internal compiler error test case minimizer using
# halfempty.
if test "$#" -eq 1; then
  if ! type halfempty >/dev/null; then
    echo "halfempty not found in PATH"
    exit 1
  fi

  exec env RUN_HALFEMPTY=0 halfempty $0 $1 --stable
fi

# This half:
# Program that drives amc and finds internal compiler errors; If the
# compiler aborted with a Haskell exception, exit 0; Otherwise, exit 1.
# Suitable for use with 'halfempty' for minimizing test cases.
tempfile=`mktemp` && cat > ${tempfile}
trap 'rm -f ${tempfile}; exit ${result}' EXIT TERM ALRM

pattern=${ERROR_PATTERN:-'CallStack (from HasCallStack):'}
out=$(eval ${AMC:-amc} compile ${tempfile} 2>&1)

if test $? -eq 1; then
if echo ${out} | grep -F "$pattern" &>/dev/null; then
  result=0
  exit 0
fi
fi

result=1

#!/usr/bin/env sh

if [ "$1" == "" -o  "$1" == "-h" -o "$1" == "--help" ]; then
  echo "PLT lab 3 testsuite runner"
  echo "usage: $0 [OPTIONS] DIRECTORY"
  echo "Takes the same options as plt-test-lab3:"
  runghc plt-test-lab3
  exit 1
fi

runghc plt-test-lab3 -- "$@"

# EOF

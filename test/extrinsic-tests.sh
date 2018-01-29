#!/bin/bash

# Test that we can actually handle no arguments
input[0]="stack exec compiler"
baseline[0]="./test/baselines/noargs.out"

# Test basic echoing
input[1]="stack exec compiler -- foo bar quux"
baseline[1]="./test/baselines/foobarquux.out"

# Test help command
input[2]="stack exec compiler -- --help"
baseline[2]="./test/baselines/help.out"

# Test that help overrides other arguments
input[3]="stack exec compiler -- foo bar quux --help"
baseline[3]="./test/baselines/help.out"

input[4]="stack exec compiler -- foo --help bar quux"
baseline[4]="./test/baselines/help.out"

input[5]="stack exec compiler -- --help foo bar quux"
baseline[5]="./test/baselines/help.out"

input[6]="stack exec compiler -- --length --help foo bar quux"
baseline[6]="./test/baselines/help.out"

# Test that length works no matter where it is
input[7]="stack exec compiler -- --length foo bar quux"
baseline[7]="./test/baselines/length.out"

input[8]="stack exec compiler -- foo --length bar quux"
baseline[8]="./test/baselines/length.out"

input[9]="stack exec compiler -- foo bar quux --length"
baseline[9]="./test/baselines/length.out"

for i in `seq 0 9`;
do
  echo -n "testing '"
  echo -n ${input[$i]}
  echo "'"
  diff <(${input[$i]}) ${baseline[$i]}
done

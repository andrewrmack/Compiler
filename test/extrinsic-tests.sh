#!/bin/bash

for fn in `ls test/baselines/*.in`;
do
  infile=$(basename $fn)
  lexfile=test/baselines/${infile%.*}.lex
  parsefile=test/baselines/${infile%.*}.parse
  outfile=test/baselines/${infile%.*}.out

  echo -n "testing '"
  echo -n $infile
  echo "'"

  if [ -e $outfile ]
  then
      diff <(stack exec compiler \-\- --lex $fn) $lexfile || (>&2 echo "Failed lex"; exit 1)
      diff <(stack exec compiler \-\- --parse $fn) $fn || (>&2 echo "Failed parse"; exit 1)
      diff <(stack exec compiler \-\- $fn) $outfile || (>&2 echo "Failed evaluation"; exit 1)
  else
      >&2 echo "No such file '${outfile}'"
      exit 1;
  fi
done

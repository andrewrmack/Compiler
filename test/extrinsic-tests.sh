#!/bin/bash

for fn in `ls test/baselines/*.in`;
do
  infile=$(basename $fn)
  outfile=test/baselines/${infile%.*}.out

  echo -n "testing '"
  echo -n $infile
  echo "'"

  if [ -e $outfile ]
  then
      diff <(stack exec compiler \-\- $fn) $outfile || exit 1
  else
      >&2 echo "No such file '${outfile}'"
      exit 1;
  fi
done

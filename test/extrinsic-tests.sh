#!/bin/bash

echo -e "Testing normal inputs:\n"
for fn in `ls test/baselines/*.in`;
do
  infile=$(basename $fn)
  lexfile=test/baselines/${infile%.*}.lex
  parsefile=test/baselines/${infile%.*}.parse
  outfile=test/baselines/${infile%.*}.out

  echo -n "testing '"
  echo -n $infile
  echo -n "'... "

  if [ -e $lexfile ]
  then
      if ! diff <(stack exec compiler \-\- --ddump-lex $fn) $lexfile ; then
        >&2 echo "Failed lexing";
        exit 1;
      fi
  else
      >&2 echo "No such file '${lexfile}'"
      exit 1;
  fi

  if [ -e $parsefile ]
  then
      if ! diff <(stack exec compiler \-\- --ddump-parse $fn) $parsefile ; then
        >&2 echo "Failed parsing";
        exit 1;
      fi
  else
      >&2 echo "No such file '${parsefile}'"
      exit 1;
  fi

  if [ -e $outfile ]
  then
      if ! diff <(stack exec compiler \-\- $fn) $outfile ; then
        >&2 echo "Failed evaluation";
        exit 1;
      fi
  else
      >&2 echo "No such file '${outfile}'"
      exit 1;
  fi
  echo "Passed!"
done

echo -e "\nTesting expected failures:\n"
for fn in `ls test/baselines/*.fail`;
do
  infile=$(basename $fn)
  echo -n "testing '"
  echo -n $infile
  echo -n "'... "

  stack exec compiler \-\- $fn &> /dev/null
  if [ $? == 1 ]
  then
      echo "Passed!"
  else
      >&2 echo "Failed!"
      exit 1;
  fi
done

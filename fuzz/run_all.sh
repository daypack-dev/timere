#!/bin/bash

simul_test_count=2

test_timeout="20s"

script_dir=$(dirname $(readlink -f "$0"))

log_dir="$script_dir"/../fuzz-logs

echo "Building"

dune build @all

echo ""

start_date=$(date "+%Y-%m-%d %H:%M")
start_time=$(date "+%s")

names=[]

i=0
for file in "$script_dir"/../_build/default/fuzz/*.exe; do
  name=$(basename $file | sed 's/\.exe$//')
  names[$i]=$name
  i=$[i+1]
done

test_count=${#names[@]}

echo "Fuzzing tests available:"

for name in ${names[@]}; do
  echo "- "$name
done

echo ""
echo "Fuzzing start time:" $start_date
echo ""

echo "Starting $test_count tests"
echo ""

i=0
while (( $i < $test_count )); do
  if (( $test_count - $i >= $simul_test_count )); then
    tests_to_run=$simul_test_count
  else
    tests_to_run=$[test_count - i]
  fi

  echo "Running $tests_to_run tests in parallel"

  mkdir -p "$log_dir"

  j=$i

  for (( c=0; c < $tests_to_run; c++ )); do
    name=${names[$i]}
    if [[ "$t" != "" ]]; then
      echo "    Starting $name"

      AFL_NO_UI=1 timeout "$test_timeout" ./"$script_dir"/run.sh "$name" > "$log_dir"/"$name".log &

      i=$[i+1]
    fi
  done

  echo "Waiting for tests to finish"

  wait

  echo ""
  echo "$[test_count - i] / $test_count tests remaining"
  echo ""
done

end_date=$(date "+%Y-%m-%d %H:%M")
end_time=$(date "+%s")
echo ""
echo "Test end:" $end_date

echo ""

echo "Time elapsed:" $[(end_time - start_time) / 60] "minutes"


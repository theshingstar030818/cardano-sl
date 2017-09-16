#!/bin/bash
# Example:
#  process_number=10 command="curl localhost:8100/api/blocks/pages" ./test_parallel.sh
if [ -z "$command" ]
  then
    echo "You must give the command to execute.";
    exit;
fi

if [ -z "$process_number" ]
  then
    process_number=100;
fi

for i in `seq $process_number`; do {
  echo "Process \"$command\" started";
  $command & PID=$!
  PID_LIST+=" $PID"
  #COMMANDS+=" $command";
} done

trap "kill $PID_LIST" SIGINT;
echo "Parallel processes have started";

wait $PID_LIST

echo "All processes have completed";


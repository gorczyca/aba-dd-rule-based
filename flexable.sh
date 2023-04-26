#!/bin/bash

FLEXABLE_JAR_LOCATION="./target/scala-2.13/flexable-assembly-1.0.jar"

DESCRIPTION="flexABle v1.0
Martin Diller (martin.diller@tu-dresden.de),
Sarah Alice Gaggl (sarah.gaggl@tu-dresden.de),
Piotr Gorczyca (piotr.gorczyca@tu-dresden.de),
"

PROBLEMS="[DC-CO,DS-CO]"


if [[ $# -eq 0 ]]; then
    echo "$DESCRIPTION"
elif [[ $# -eq 1 && "$1" == "--problems" ]]; then
    echo "$PROBLEMS"
else

  TASK=""
  FILE=""
  QUERY=""

  while getopts "hp:f:a:" opt; do
    case ${opt} in
      h )
        echo "Usage: flexable.sh [OPTIONS]"
        echo "  -h              Display this help message"
        echo "  --problems      Run display "
        echo "  -p <task>       Task name"
        echo "  -f <file>       File name"
        echo "  -a <query>      Query string"
        exit 0
        ;;
      - )
        break
        ;;
      p )
        TASK=$OPTARG
        ;;
      f )
        FILE=$OPTARG
        ;;
      a )
        QUERY=$OPTARG
        ;;
      \? )
        echo "Invalid option: $OPTARG" 1>&2
        exit 1
        ;;
    esac
  done

  if [[ -n "$TASK" && -n "$FILE" && -n "$QUERY" ]]; then
    if [[ "$TASK" == "DC-CO" ]]; then
      java -jar "$FLEXABLE_JAR_LOCATION" "$FILE" -g "$QUERY" -i iccma -s -q --da dabf --tc ta
    elif [ "$TASK" == "DC-ST" ]; then
      java -jar "$FLEXABLE_JAR_LOCATION" "$FILE" -g "$QUERY" -i iccma -s -q --da ds --tc ts
    else
      echo "Invalid task."
    fi
  else
      echo "Invalid input parameters."
  fi
fi

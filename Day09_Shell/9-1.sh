#!/bin/bash

read input

# set debug flag
if [ "$1" = "-d" ]; then debug=1; fi

if [ "$debug" = "1" ]; then echo "Original input: $input"; fi

# loop through each character in input
while [ "$input" != "" ]
do
    # if current character is a (, we've found a marker
    if [ "${input:0:1}" = "(" ]
    then
        # get the contents in the parentheses
        marker=$(echo "$input" | grep -o -P "(?<=\().*?(?=\))" | head -1)
        # parse them into number of chars and repeat count
        dims=(${marker//x/ })

        # update input to exclude the marker
        markerLen=$((${#marker}+2))
        input=$(echo "$input" | tail -c +$(($markerLen + 1)))

        # get text to repeat and times to repeat it
        repeatCharCount=${dims[0]}
        repeatPart=$(echo "$input" | head -c $repeatCharCount)
        repeatTimes=${dims[1]}
        if [ "$debug" = "1" ]; then echo "Repeat \"$repeatPart\" $repeatTimes times"; fi

        # repeat text correct number of times, appending to result
        for (( it=0; it < $repeatTimes; it++ ))
        do
            result="$result$repeatPart"
        done

        # update input to exclude repeated characters
        input=$(echo "$input" | tail -c +$(($repeatCharCount + 1)))

        if [ "$debug" = "1" ]; then echo "Current contents of input: $input"; fi
    else
        # if not a (, just append to result
        result="$result${input:$i:1}"

        # get the rest of the input after the current character
        input=$(echo "$input" | tail -c +2)
    fi
done

if [ "$debug" = "1" ]; then echo "Decompressed input: $result"; fi
echo "Decompressed length: ${#result}"

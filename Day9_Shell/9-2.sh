#!/bin/bash

# set debug flag
if [ "$1" = "-d" ]; then debug=1; fi

# $1 = string
function calcDecompressedLengthOfString ()
{
    input="$1"
    res=0

    # loop through each character in input
    until [ "$input" = "" ]
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

            repeatCharCount=${dims[0]}
            repeatTimes=${dims[1]}

            res=$(($res + $repeatTimes * $(calcDecompressedLengthOfString "${input:0:$repeatCharCount}")))

            # update input to exclude repeated characters
            input=$(echo "$input" | tail -c +$(($repeatCharCount + 1)))
        else
            # if not a (, increment result
            res=$(($res + 1))

            # get the rest of the input after the current character
            input=$(echo "$input" | tail -c +2)
        fi
    done

    echo $res
}

read input

if [ "$debug" = "1" ]; then echo "Original input: $input"; fi

result=$(calcDecompressedLengthOfString $input)
echo "Decompressed length: $result"

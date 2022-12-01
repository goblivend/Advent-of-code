#/bin/sh

function GetMostValue() {
    local max=0
    local nb=0

    while IFS= read -r line; do
        if [ -z $line ]; then
            if [ $nb -gt $max ]; then
                max=$nb
            fi
            nb=0
        else
            nb=$((nb + $line))
        fi
    done < $1
    echo $max
}

GetMostValue "input.txt"


function GetMost3Value() {
    local max=0
    local max_1=0
    local max_2=0

    local nb=0

    while IFS= read -r line; do
        if [ -z $line ]; then
            if [ $nb -gt $max_2 ]; then
                max_2=$max_1
                max_1=$max
                max=$nb
            elif [ $nb -gt $max_1 ]; then
                max_2=$max_1
                max_1=$nb
            elif [ $nb -gt $max_2 ]; then
                max_2=$nb
            fi

            nb=0
        else
            nb=$((nb + $line))
        fi
    done < $1
    echo $(($max + $max_1 + $max_2))
}

GetMost3Value "input.txt"

inputFile="../../Data/Day 1/input.txt"

function GetIncrease() {
    local prev=10000
    local nb=0

    while IFS= read -r line; do
        curr=$(($line))
        if [ $curr -gt $(($prev)) ] ; then
            nb=$((nb+1))
        fi
        prev=$curr
    done <"$1"
    echo "$nb"
# source Day1.sh
}

GetIncrease "$inputFile"


function GetGeneralIncreast() {
    local curr_1=100000
    local curr_2=100000
    local curr_3=100000

    local nb=0
    while IFS= read -r line; do
        curr=$(($line))
        if [ $(($curr + $curr_1 + $curr_2)) -gt $(($curr_1 + $curr_2 + $curr_3)) ] ; then
            nb=$((nb+1))
        fi
        curr_3=$curr_2
        curr_2=$curr_1
        curr_1=$curr
    done <"$1"
    echo "$nb"
}

GetGeneralIncreast "$inputFile"
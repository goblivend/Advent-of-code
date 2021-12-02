inputFile="../../Data/Day2/input.txt"
exampleFile="../../Data/Day2/ExampleFile.txt"

function GetIncrease() {
    local depth=0
    local forward=0

    while IFS="" read -r line; do
        IFS=$' '; data=($line); unset IFS;


        if [ ${#data[@]} -ne 2 ] ; then
            echo "${#data[@]} for $data"
            continue
        else
            case $data[0] in
                "up")
                    depth=depth-$((data[1]))
                    ;;
                "down")
                    depth=depth+$((data[1]))
                    ;;
                "forward")
                    forward=forward+$((data[1]))
                    ;;
                *)
                    echo "no found data '${data[0]}'"
                    ;;
            esac

        fi
    done <"$1"

    echo "$depth * $forward"
# source Day1.sh
}

GetIncrease "$exampleFile"
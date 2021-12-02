depth=0 ; forward=0 ; depthaim=0
while IFS=$"" read -r line; do
    IFS=$' '; data=($line); unset IFS
    [ ${#data[@]} -ne 2 ] && [ echo "${#data[@]} for $data" continue ]
    case $data[0] in
        "up"*)
            depth=$(($((depth))-$((data[1])))) ;;
        "down"*)
            depth=$(($((depth))+$((data[1])))) ;;
        "forward"*)
            forward=$(($((forward))+$((data[1]))))
            depthaim=$(($((depthaim))+$((depth))*$((data[1])))) ;;
        *)
        echo "no found data '${data[0]}'" ;;
    esac ; done <"$1"

echo "Part 1 : $depth * $forward" && echo "Part 2 : $depthaim * $forward"

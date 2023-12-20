#!/bin/sh

EDGES=`cat $1`
START='broadcaster [fillcolor="green" style="filled"]'
END='rx [fillcolor="red" style="filled"]'

echo "
digraph my_graph {
    $START
    $END
    $EDGES
}
" | dot -Tpng > graph.png

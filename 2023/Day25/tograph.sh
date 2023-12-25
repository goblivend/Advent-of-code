#!/bin/sh

EDGES=`cat $1` #`sed -E 's/(.{3}) (.{3})/\1 -- \2/' $1 | tail +2`

echo "
graph my_graph {
    $EDGES
}
" | neato -Tpng > graph.png

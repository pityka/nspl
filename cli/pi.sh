#!/usr/bin/env bash

function getData {
    awk 'BEGIN { 
        a=0;
        for (i = 1; i <= 100000; i++) {
            x=rand();
            y=rand();
            if ((x-0.5)^2+(y-0.5)^2 < 0.25) {z=1;++a} else {z=0;++b};
            print x,y,z,(4*a)/i,i,3.14;
            };
        }'
} && \
cli/target/scala-2.11/nsplcli-out \
    file[<( getData ) --min 3 --max 10000] \
    xy[data[point[ --errorTopCol 7 --sizeCol 7 --shapeCol 6 --color colorlist[color[red] color[blue]]] ] \
        --xlim pair[--1 0 --2 1] \
        --ylim pair[--1 0 --2 1] \
        --main circle] \
    xy[data[line[--xCol 4 --yCol 3] line[--xCol 4 --yCol 5 --color color[red]]] --main pi] \
    size[--width 1200 --height 600]
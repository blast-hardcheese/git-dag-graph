#!/bin/bash

if [ ! -d "$1" ]; then
    git init "$1"
    cd "$1"
    git commit --allow-empty -m 'Initial commit'
    echo "first file" > file.txt
    git add file.txt
    git commit -m 'adding file.txt'
fi

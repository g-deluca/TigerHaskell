#!/bin/bash

echo -e "\e[36mCompile and run \e[1m$1\e[0m"

rm "./assem/$1.s"
rm "$1.out"
stack run -- "./test/test_code/good/$1.tig" >> "./assem/$1.s"
gcc -m32 runtime.c "./assem/$1.s" -o "$1.out"

./$1.out
echo -e "\nreturn: $?"

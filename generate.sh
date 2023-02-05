#!/bin/bash

DIR=`pwd`
cd /tmp/
rm -rf residtemp
git clone $DIR residtemp
cd /tmp/residtemp

function doIt() {
    rm -f testCycles
    make -f $DIR/Makefile tc
}

doIt
while [ `git log -1 --pretty=format:"%H"` != "5231482718edf7718ba92c5b07518ea3bb2ad0bc" ]; do
    git reset --hard HEAD^1
    doIt
done

find . -iname "tc-*" | sort | sed -e 's,^\./,,' | awk '{ print "echo -------------- " $0; print $0;}' > runit

lha a testCycles.lha runit tc-*
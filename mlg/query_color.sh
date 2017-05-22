#!/bin/sh

TERMS=`ls -1 /usr/share/terminfo/*/* |\
    xargs basename -a`
echo "("
for t in $TERMS
do
    C=`tput -T$t colors`
    if [ $C -gt 7 ] ; then
	F=`tput -T$t setaf`
	if [ $F = `tput -Tansi setaf` ] ; then
	    echo "\"$t\""
	fi
    fi
done
echo ")"

#!/bin/bash
#
# This procedure compiles all *.c modules
#

cfile=`ls *.c`
for file in ${cfile}
do
  ofile=${file%.*}.o
  if ! [ -e ${ofile} ] || [ ${file} -nt ${ofile} ]; then 
    echo "Compiling file ${file}" 
    gcc -c -g -DUNDERSCORE ${file} 
  fi
done

#!/bin/bash
#
# This procedure performs a tkdiff on *.u and *.u.0 files
#

ufile=`ls *.u`
for file in ${ufile}
do
  ofile=${file}.0
  if  [ -e ${ofile} ]; then 
    echo "Tkdiff of file ${file}" 
    tkdiff ${file} ${file}.0
  fi
done

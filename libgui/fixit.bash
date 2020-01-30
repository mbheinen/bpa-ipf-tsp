#!/bin/bash
#
#

files=`ls *.u`
echo "Checking files ${files}"
for file in ${files}
do
  cp ${file} ${file}.0
  /home/bin/remove_cr ${file}.0 ${file}
done

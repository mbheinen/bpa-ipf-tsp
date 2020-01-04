#!/bin/bash
#
#

files=`ls *.u.1`
for file1 in ${files}
do
  file2=${file1%%.*}.u
  cp ${file1} ${file2}
done

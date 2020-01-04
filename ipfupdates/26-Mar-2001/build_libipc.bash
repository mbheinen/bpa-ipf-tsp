#!/bin/sh
#
# This procedure builds object libraries
#

for file in ipc_inet.c ipc_com.c
do
  ofile=${file%.*}.o
  if ! [ -e ${ofile} ] || [ ${file} -nt ${ofile} ]; then 
    echo "Compiling file ${file}" 
    gcc -c -DUNDERSCORE ${file} 
  fi
done
echo "Creating libipc.a"
rm -f libipc.a
ar -cr libipc.a *.o
ls -al libipc.a

#!/bin/bash
#
# This procedure builds object libraries
#

rm -f ipfinc
ln -s /shr/ipf-3.27/ipf ipfinc

forfile=`ls *.f`
for file in ${forfile}
do
  ofile=${file%.*}.o
  if ! [ -e ${ofile} ] || [ ${file} -nt ${ofile} ]; then 
    echo "Compiling file ${file}" 
    g77 -c -g -DUNDERSCORE -fno-f2c ${file} 
  fi
done

cfile=`ls *.c`
for file in ${cfile}
do
  ofile=${file%.*}.o
  if ! [ -e ${ofile} ] || [ ${file} -nt ${ofile} ]; then 
    echo "Compiling file ${file}" 
    gcc -c -g -DUNDERSCORE ${file} 
  fi
done
echo "Creating libipf.a"
rm -f libipf.a
ar -cr libipf.a *.o
ls -al libipf.a

#!/bin/bash
#
# This procedure builds object libraries using Linux
#
rm -f ipfinc 
ln -s /shr/ipf-3.27/ipf ipfinc
rm -f tspinc 
ln -s /shr/tsp-6.07/tsp tspinc

forfile=" `ls *.f` "
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

rm -f libtsp.a
ar -cr libtsp.a *.o > libtsp.log
echo "Library archive in file libtsp.log"
ranlib libtsp.a
exit


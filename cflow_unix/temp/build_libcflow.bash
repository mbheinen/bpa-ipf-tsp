#!/bin/bash
#
# This procedure builds object libraries
#

for file in cflow_lib_ipc.c convtbl.c ft.c pf_rec.c run_ipfsrv.c
do
  ofile=${file%.*}.o
  if ! [ -e ${ofile} ] || [ ${file} -nt ${ofile} ]; then 
    echo "Compiling file ${file}" 
#   gcc -c -g -DUNDERSCORE -Wall ${file} 
    gcc -c -g -DUNDERSCORE ${file} 
  fi
done
echo "Creating libcflow.a"
rm -f libcflow.a
ar -cr libcflow.a *.o
ls -al libcflow.a

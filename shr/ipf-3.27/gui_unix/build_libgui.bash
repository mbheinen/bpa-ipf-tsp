#!/bin/bash
#
# This procedure builds object libraries
#

cfile=`ls *.c`
for file in ${cfile}
do
  ofile=${file%.*}.o
  if ! [ -e ${ofile} ] || [ ${file} -nt ${ofile} ]; then 
    echo "Compiling file ${file}" 
    gcc -g -DUNDERSCORE -DTOP_DOWN -DOSF_2_0 -DIPFCONSTOK -DX_LOCALE \
		-I../gui \
		-I../ipc \
       		-I/usr/X11R6 \
       		-I/usr/X11R6/include \
       		-I/usr/X11R6/include/Mrm \
       		-I/usr/X11R6/include/X11 \
       		-I/usr/X11R6/include/Xm \
		-I/usr/include/sys/include -c ${file}
  fi
done
echo "Creating libgui.a"
rm -f libgui.a
ar -cr libgui.a *.o
ls -al libgui.a

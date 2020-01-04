#!/usr/bin/csh
#
# This procedure builds object libraries using Alpha/OSF
#
rm ipfinc; 
ln -s /shr3/shr/ipf-3.24/ipf ipfinc
rm tspinc; 
ln -s /shr3/shr/tsp-6.06/tsp tspinc
foreach l ( a b c d e f g h i j k l m n o p q r s t u v w x y z )
  set files=" `ls $l*.f` "
  foreach file ( $files )
    set fn=$file:r.o
    if ( -e $fn ) then
    else
      echo "Compiling $file "
      f77 -fpe -g -C -align commons -O0 -c $file
    endif
  end
  set files=" `ls $l*.c` "
  foreach file ( $files )
    set fn=$file:r.o
    if ( -e $fn ) then
    else
      echo "Compiling $file "
      c89 -DUNDERSCORE -std -g -I. -I/usr/lib/DXM/lib  -I/usr/lib/DXM/lib/Xt \
	-I/usr/include/mit -c $file
    endif
  end
end
rm -f libtsp.a
ar lrvs libtsp.a *.o > libtsp.log
echo "Library archive in file libtsp.log"
exit


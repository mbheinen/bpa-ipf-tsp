#!/bin/csh
#
set echo
foreach l ( a b c d e f g h i j k l m n o p q r s t u v w x y z )
  set files=" `ls $l*.[fcu]` "
  foreach file ( $files )
    if ( -e ../gui/$file ) then 
      echo "Commparing files ../gui_vax/$file and ../gui/$file"
      echo "Enter <RETURN> to continue"
      set inp=($<)
      cmp -s ../gui_vax/$file ../gui/$file 
      if ( $status != 0 ) then 
        dxdiff ../gui_vax/$file ../gui/$file
      else  
        echo "Files are identical"
      endif
    else  
      echo "File $file does not exist in dir ../gui"
    endif
  end
end
exit

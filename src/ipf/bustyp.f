C    @(#)bustyp.f	20.3 2/13/96
       character*2 function bustyp( nb )
 
*
*      ROUTINE TO RETURN BUSTYPE
*      FROM THE POWER FLOW DATA TABLES.
*
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/bus.inc'
c	Global variables used:
c		kbsdta
c 
      character type(13)*2
      data (type(i),i=1,10)
     1     /'B ','BE','BS','BC','BD','BV','BQ','BG','BO','BT'/
*             1    2    3    4    5    6    7    8    9   10
      data (type(i),i=11,13)
     1     /'BX','BM','BF'/
*            11   12   13
 
***************************************************************
***************************************************************
 
      bustyp  = type(  kbsdta( 1, nb ) )
      return
      end

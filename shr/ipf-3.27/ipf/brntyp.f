C    @(#)brntyp.f	20.3 2/13/96
      character*2 function brntyp( ptr )
      integer ptr
c
c     Return branch type from the power flow data tables.
c
      include 'ipfinc/parametr.inc'

      character type(9)*2

      data (type(i),i=1,9)
     1     /'PI','LM','L ','R ','T ','TP','LD','E ','RZ'/
c             1    2   3    4    5    6    7    8    9
 
      brntyp  = type( ptr )
 
      return
      end

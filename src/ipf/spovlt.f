C    @(#)spovlt.f	20.3 2/13/96
      subroutine spovlt(i,j)
C ***                                                         
C *** This subroutine swaps row entities I and J in overloaded
C *** transformer array OLTR.                                 
C ***                                                         
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'
 
      do 100 ix = 1,4
         itemp = koltr(ix,i)
         koltr(ix,i) = koltr(ix,j)
         koltr(ix,j) = itemp
  100 continue
 
      return
      end

C    @(#)spovll.f	20.3 2/13/96
      subroutine spovll(i,j)
C ***                                                          
C *** This subroutine swaps row entities I and J in overloaded 
C *** transmission line array OLBR.                            
C ***                                                          
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'
 
      do 100 ix = 1,6
         itemp = kolbr(ix,i)
         kolbr(ix,i) = kolbr(ix,j)
         kolbr(ix,j) = itemp
  100 continue
 
      return
      end

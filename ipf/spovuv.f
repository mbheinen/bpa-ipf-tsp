C    @(#)spovuv.f	20.3 2/13/96
      subroutine spovuv(i,j)
C ***                                                                  *
C *** This subroutine swaps sort entities I and J in over/under        *
C *** voltage sort index array VLTSRT.                                 *
C ***                                                                  *
 
      include 'ipfinc/parametr.inc'
 
      common /sortsw/ sortsw, vltsrt(MAXBUS)
      integer sortsw, vltsrt
 
      ix = vltsrt(i)
      vltsrt(i) = vltsrt(j)
      vltsrt(j) = ix
 
      return
      end

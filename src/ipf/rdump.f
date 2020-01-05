C    @(#)rdump.f	20.3 2/13/96
      subroutine rdump
c
      include 'ipfinc/parametr.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/elim.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/renum.inc'
c
      integer array(10)

      write (dbug,10)
   10 format('1      KOWNT       INDEX        ICH1        ',
     &       'ICH2        LOC1      KOLUM1      KORDR1        ',
     &       'LOC2      KOLUM2      KORDR2')
      do j = 1,nsize
         do l = 1,10
            array(l) = 0
         enddo
         array(6) = kolum1(j)
         array(7) = kordr1(j)
         array(9) = kolum2(j)
         array(10) = kordr2(j)
         if (j .le. ntot) then
            array(1) = kownt(2,j)
            array(3) = ich1(j)
            array(4) = ich2(j)
            array(5) = loc1(j)
            array(8) = loc2(j)
            k = j
         else
            k = 0
         endif
         if (j .le. 250) then
            array(2) = xindex(j)
         else
            array(2) = 0
         endif
         write (dbug,60) k,array,j
   60    format(2i6,10i12)
      enddo
      return
      end

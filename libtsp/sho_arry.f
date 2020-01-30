C    %W% %G%
      subroutine sho_arry(stklvl, left, rite)
      integer stklvl, left, rite

C     A debugging routine to dump out the current part of an array
C     that's not sorting properly
C     -  This version uses the area load rep array AREAC2

      include 'tspinc/params.inc'
      include 'tspinc/areanz.inc'
      include 'tspinc/pointr.inc'

      common /areac2/areac2(60)
      character*10 areac2
      common /lqiks/lqiks
C     -           -               -
      if (lqiks .eq. 11) then
        write (13, '(a,i4,a,i4,a,i4)')
     &   ' QIKSRT / SHO_ARRY - show range ', left, ' to ', rite,
     &   ' | level = ', stklvl
        do la = left, rite
          write (13, '(a,i4,a,a)') '   LA = ', la, ' | AREAC2[LA] = ',
     &     areac2(la)
        enddo
      endif
      return
      end

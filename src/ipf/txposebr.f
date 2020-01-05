C    @(#)txposebr.f	20.1 6/27/97
C****************************************************************
C
C     File: txposebr.f
C
C     Purpose: Routine to transpose branch k2-k2)
C
c     Input Parameters:
c         ptr1 - branch pointer for k1-k2
c         ptr2 - branch pointer for k2-k2
c
C     Author: Walt Powell  Date: 12 Feb 1997
C     Called by: ldptisec.f
C
C****************************************************************
      subroutine txposebr (ptr1, ptr2)
      integer ptr1, ptr2

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/alpha.inc' 

      integer q, qold

      qold = brnch_ptr(ptr1)
      q = iabs(qold)
      if (brtype(ptr1) .eq. 5) then
        temp = brnch(9,q)
        brnch(9,q) = brnch(10,q)
        brnch(10,q) = temp
      else if (brtype(ptr1) .eq. 6) then
        temp = brnch(9,q)
        brnch(9,q) = -temp
      else if (brtype(ptr1) .eq. 8) then
        temp = brnch(7,q)
        brnch(7,q) = brnch(9,q)
        brnch(9,q) = temp
        temp = brnch(8,q)
        brnch(8,q) = brnch(10,q)
        brnch(10,q) = temp
      endif
      if (kbrnch(15,q) .eq. 0) then
        kbrnch(15,q) = 1
      else
        kbrnch(15,q) = 0
      endif
      if (kbrnch(17,q) .eq. 0) then
        kbrnch(17,q) = 1
      else
        kbrnch(17,q) = 0
      endif
      brnch_ptr(ptr1) = isign (q, -qold)
      q = brnch_ptr(ptr2)
      brnch_ptr(ptr2) = isign (q, qold)

c     k1 = kx(ptr1)
c     k2 = ky(ptr1)
c     write (errbuf(1), 230) bus(k1), base(k2), bus(k2), base(k2), 
c    &                       brid(ptr1), brsect(ptr1)
c 230 format (' Branch section (', a8, f7.1, 1x, a8, f7.1, 1x, a1, i2, 
c    &        ') is is reoriented') 
c     call prterx ('W',1)

      return
      end

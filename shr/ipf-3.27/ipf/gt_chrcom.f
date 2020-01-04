C    @(#)gt_chrcom.f	20.2 2/28/00
C****************************************************************
C
C     File: gt_chrcom.f
C
C     Purpose: Routine to retrieve specific 3-character arrays from 
c     specific commons //
C
C     Input parameters:
C
C             tag      - "NUMOWN"
C                        "OWNER_O"
C             variable -
c             size     -
c             wordsize -
C
C     Author: Walt Powell  Date: 21 Jan 2000
C     Called by: ext_geown.f
C
C****************************************************************
      subroutine gt_chrcom (tag, array, size)
      integer size
      character tag*(*), array(*)*(3)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/ownhash.inc'

      if (tag .eq. 'OWNER_O') then
        do i = 1, size
          array(i) = owner_o(i)
        enddo
      else
        write (errbuf(1), 10000) tag
10000   format ('Illegal call to subroutine gt_chrcom (', a, ')')
        call prterx ('W', 1)
      endif

      return
      end

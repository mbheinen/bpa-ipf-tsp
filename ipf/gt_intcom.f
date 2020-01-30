C    @(#)gt_intcom.f	20.2 2/28/00
C****************************************************************
C
C     File: gt_intcom.f
C
C     Purpose: Routine to retrieve specific arrays from specific 
c     commons //
C
C     Input parameters:
C
C             tag      - "NUMOWN"
C                        "OWNER_O"
C             variable -
c             size     -
C
C     Author: Walt Powell  Date: 21 Jan 2000
C     Called by: ext_geown.f
C
C****************************************************************
      subroutine gt_intcom (tag, array, size)
      character *(*) tag
      integer array(*), size

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/ownhash.inc'

      if (tag .eq. 'NUMOWN') then
        array(1) = numown
      else
        write (errbuf(1), 10000) tag
10000   format ('Illegal call to subroutine gt_intcom (', a, ')')
        call prterx ('W', 1)
      endif

      return
      end

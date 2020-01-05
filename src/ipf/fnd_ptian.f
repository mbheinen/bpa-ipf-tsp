C    @(#)fnd_ptian.f	20.4 2/28/00
C****************************************************************
C
C     File: fnd_ptiaa.f
C
C     Purpose: Routine to find PTI hash index given its area number
C              
C
c     Return code:  n > 0 : PTI area hash number
c                   n = 0 : Error or termination flag
c
C     Author: Walt Powell  Date: 4 October 1999
C     Called by: clnup_ge.f
C
C****************************************************************
      integer function fnd_ptian (areanum)
      integer areanum

      include 'ipfinc/parametr.inc'

      include 'ipfinc/pti_data.inc'

      integer        h, p

      fnd_ptian = 0
      if (areanum .gt. 0) then
c
c       Hash area number
c
        h = mod (areanum, MAXCAR-1) + 1
        p = htable_a(h)
        do while (p .gt. 0)              !search for existing entities
          if (areanum .eq. pti_anum(p)) then
            fnd_ptian = p
            p = -p
          else
            p = nextptr_a(p)
          endif
        enddo
      endif
      return
      end

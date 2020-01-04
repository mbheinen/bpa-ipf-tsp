C    @(#)fnd_ptia.f	20.2 11/11/97
C****************************************************************
C
C     File: fnd_ptia.f
C
C     Purpose: Routine to find PTI area index given its PTI name.
C
c     Return code:  n > 0 : PTI area hash number
c                   n = 0 : Error -  number not found
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
        integer function fnd_ptia (areaname)
        character *(*) areaname

        include 'ipfinc/parametr.inc'

        include 'ipfinc/pti_data.inc'

        integer        h, p
      
        h = 0
        do i = 1, 8
          h = h + h + ichar (areaname(i:i))
        end do
        h = mod (h, MAXCAR-1) + 1
        p = htable_c(h)
        do while (p .gt. 0)
          if (areaname(1:8) .ne. pti_anam(p)(1:8)) then
            p = nextptr_c(p)
          else
            p = -p
          endif
        enddo
        fnd_ptia = -p
        return
        end


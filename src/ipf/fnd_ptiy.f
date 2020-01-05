C    @(#)fnd_ptiy.f	20.1 7/18/96
C****************************************************************
C
C     File: fnd_ptiy.f
C
C     Purpose: Routine to find PTI hash index given its zone name
C
c     Return code:  n > 0 : PTI zone hash number
c                   n = 0 : Error -  number not found
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: saveptid.f
C
C****************************************************************
        integer function fnd_ptiy (zonename)
        character *(*) zonename

        include 'ipfinc/parametr.inc'

        include 'ipfinc/pti_data.inc'

        integer        h, p, num
        logical found

        h = 0
        do i = 1, 2
          h = h + h + ichar (zonename(i:i))
        end do
        h = mod (h, MAXCZN-1) + 1
        p = htable_y(h)
        found = .false.
        do while (p .gt. 0 .and. .not. found)
          if (zonename .ne. pti_znam(p)) then
            p = nextptr_y(p)
          else
            found = .true.
          endif
        enddo
        fnd_ptiy = p   
        return
        end

C    @(#)fnd_ptiz.f	20.2 11/11/97
C****************************************************************
C
C     File: fnd_ptiz.f
C
C     Purpose: Routine to find PTI zone index given PTI zone number
C
c     Return code:  n > 0 : PTI zone hash index
c                   n = 0 : Error -  number not found
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
        integer function fnd_ptiz (numzone)
        integer numzone

        include 'ipfinc/parametr.inc'

        include 'ipfinc/pti_data.inc'

        integer h, p
        logical found
      
        h = mod (numzone, MAXCZN-1) + 1
        p = htable_z(h)
        found = .false.
        do while (p .gt. 0 .and. .not. found)
          if (numzone .ne. pti_znum(p)) then
            p = nextptr_z(p)
          else
            found = .true.
          endif
        enddo
        fnd_ptiz = p
        return
        end


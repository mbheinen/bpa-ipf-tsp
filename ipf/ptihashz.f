C    @(#)ptihashz.f	20.2 11/11/97
C****************************************************************
C
C     File: ptihashz.f
C
C     Purpose: Integer function to generate a hash index from a
C              PTI zone number
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
        integer function ptihashz (zonenum, zonename)
        integer zonenum
        character *(*) zonename

        include 'ipfinc/parametr.inc'

        include 'ipfinc/pti_data.inc'

        integer        h, p
      
        h = mod (zonenum, MAXCZN-1) + 1
        p = htable_z(h)
        if (p .eq. 0) then
          num_znam = num_znam + 1
          p = num_znam
          htable_z(h) = p
          pti_znum(p) = zonenum
          pti_znam(p) = zonename
        endif
        ptihashz = p
c
c       Hash zone name
c
        if (iabs(ptihashz) .gt. 0) then
          h = 0
          do i = 1, 2
            h = h + h + ichar (zonename(i:i))
          end do
          h = mod (h, MAXCZN-1) + 1
          p = htable_y(h)
          do while (p .gt. 0)
            if (zonename .ne. pti_znam(p)) then
              p = nextptr_y(p)
            else
              p = -p
            endif
          enddo
          if (p .eq. 0) then
            p = iabs(ptihashz)
            nextptr_y(p) = htable_y(h)
            htable_y(h) = p
          endif
        endif
        return
        end

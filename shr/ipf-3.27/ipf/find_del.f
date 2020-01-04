C    @(#)find_del.f	20.3 2/13/96
        integer function find_del (record)
        character record *(*)

c       This function searches delete(*) for record.

        include 'ipfinc/parametr.inc'
        include 'ipfinc/delete.inc'

        logical found

        i1 = 1
        i2 = ndelete
        found = .false.
c
c       This routine exploits komp_del by temporarily installing
c       the searched record in delete(ndelete+1)
c
        delete(ndelete+1) = record

        do while (i1 .le. i2 .and. .not. found)
           ix = (i1 + i2) / 2
           komp = komp_del (ix, ndelete+1)
           if (komp .lt. 0) then
              i1 = ix + 1
           else if (komp .gt. 0) then
              i2 = ix - 1
           else
              found = .true.
              find_del = ix
           endif
        enddo
        if (.not. found) then
           find_del = -ix
        else if (delete(ix)(3:3) .eq. '*') then
           find_del = -ix
        endif
        return
        end

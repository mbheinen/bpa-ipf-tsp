C    @(#)ptihasha.f	20.2 11/11/97
C****************************************************************
C
C     File: ptihasha.f
C
C     Purpose: Integer function to generate hash indices from an
C              PTI area number and an area name
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f and load_ge.f
C
C****************************************************************
        integer function ptihasha (areanum, areaname)
        integer areanum
        character *(*) areaname

        include 'ipfinc/parametr.inc'

        include 'ipfinc/pti_data.inc'

        integer      h, p, pold
      
        h = mod (areanum, MAXCAR-1) + 1
        p = htable_a(h)
        if (p .eq. 0) then
          num_anam = num_anam + 1
          p = num_anam
          htable_a(h) = p
          pti_anum(p) = areanum
          pti_anam(p) = areaname
        endif
        ptihasha = p

        h = 0
        do i = 1, 8
          h = h + h + ichar (areaname(i:i))
        end do
        h = mod (h, MAXCAR-1) + 1
        pold = 0
        p = htable_c(h)
        do while (p .gt. 0)
          if (areaname(1:8) .ne. pti_anam(p)(1:8)) then
            pold = p
            p = nextptr_c(p)
          else
            p = -p
          endif
        enddo
        if (p .eq. 0) then
          if (pold .eq. 0) then
            htable_c(h) = ptihasha
          else
            nextptr_c(pold) = ptihasha
            nextptr_c(ptihasha) = 0
          endif
        endif
        return
        end

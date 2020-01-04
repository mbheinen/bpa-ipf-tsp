C    @(#)add_ptia.f	20.4 2/28/00
C****************************************************************
C
C     File: add_ptia.f
C
C     Purpose: Routine to define two PTI area hash indices:
C              Given its name, define its PTI name index;
C              Given its number, define its PTI number index.
C              
C
c     Return code:  n > 0 : PTI area hash number
c                   n < 0 : PTI area hash number (duplicate entity)
c                   n = 0 : Error or termination flag
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function add_ptia (areanum, areaname)
      integer areanum
      character areaname*10

      include 'ipfinc/parametr.inc'

      include 'ipfinc/pti_data.inc'

      integer        h, p

      if (areanum .eq. 0) then
        add_ptia = 0
      else
c
c       Hash area number
c
        h = mod (areanum, MAXCAR-1) + 1
        p = htable_a(h)
        do while (p .gt. 0)
          if (areanum .eq. pti_anum(p) .and.
     &        areaname .eq. pti_anam(p)) then
            p = -p
          else
            p = nextptr_a(p)
          endif
        enddo
        if (p .eq. 0) then                !search for existing entities
          num_anam = num_anam + 1
          p = num_anam
          htable_a(h) = p
          pti_anum(p) = areanum
          pti_anam(p) = areaname
        endif
        add_ptia = p
c
c       Hash area name
c
        if (iabs(add_ptia) .gt. 0) then
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
          if (p .eq. 0) then
            p = iabs(add_ptia)
            nextptr_c(p) = htable_c(h)
            htable_c(h) = p
          endif
        endif
      endif
      return
      end

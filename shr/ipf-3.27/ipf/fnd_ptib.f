C    @(#)fnd_ptib.f	20.3 11/11/97
C****************************************************************
C
C     File: fnd_ptib.f
C
C     Purpose: Routine to find PTI bus hash index given PTI name
C              bus hash tables.
C
c     Return code:  n > 0 : PTI bus hash number
c                   n = 0 : Error -  number not found
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function fnd_ptib (busname, basekv, areaname)
      character *(*) busname, areaname

      include 'ipfinc/parametr.inc'

      include 'ipfinc/pti_data.inc'

      integer        h, p, areanum, fnd_ptia
      logical found

      areanum = fnd_ptia(areaname)
      if (areanum .ne. 0) areanum = pti_anum(areanum)

      h = 0
      do i = 1, 8
         h = h + h + ichar (busname(i:i))
      end do
      h = h + int (basekv) 
      h = mod (h, PTI_HASHSIZE) + 1
      p = htable_b(h)
      found = .false.
      do while (p .gt. 0 .and. .not. found)
        if (busname .ne. pti_name(p) .or. 
     &      basekv .ne. pti_base(p)) then
          p = nextptr_b(p)
        else if (areanum .ne. 0 .and. areanum .ne. pti_area(p)) then
          p = nextptr_b(p)
        else
          found = .true.
        endif
      enddo
      if (.not. found) p = 0
      fnd_ptib = p   
      return
      end

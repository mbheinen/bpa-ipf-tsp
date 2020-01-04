C    @(#)add_ptib.f	20.3 11/11/97
C****************************************************************
C
C     File: add_ptib.f
C
C     Purpose: Routine to define PTI bus hash index given bus name
c              and area  (Area numbers are part of the name)
C
c     Return code:  n > 0 : PTI bus hash number
c                   n < 0 : PTI bus hash number (duplicate entity)
c                   n = 0 : Error or termination flag
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function add_ptib (busname, basekv, areaname, indx)
      character *(*) busname, areaname
      integer indx
      
      include 'ipfinc/parametr.inc'

      include 'ipfinc/pti_data.inc'

      integer        h, p, fnd_ptia, areanum

      areanum = fnd_ptia(areaname)
      if (areanum .ne. 0) areanum = pti_anum(areanum)

      h = 0
      do i = 1, 8
         h = h + h + ichar (busname(i:i))
      end do
      h = h + int (basekv) 
      h = mod (h, PTI_HASHSIZE) + 1
      p = htable_b(h)
      do while (p .gt. 0)            !search for existing entities
        if (busname .ne. pti_name(p) .or. 
     &      basekv .ne. pti_base(p)) then
          p = nextptr_b(p)
        else if (areanum .ne. 0 .and. areanum .ne. pti_area(p)) then
          p = nextptr_b(p)
        else
          p = -p                   
        endif
      enddo
      if (p .eq. 0) then
        nextptr_b(indx) = htable_b(h)
        htable_b(h) = indx
        pti_name(indx) = busname
        pti_base(indx) = basekv
        pti_area(indx) = areanum
        add_ptib = indx
      else
        add_ptib = p                  ! p < 0 flags duplicate entity!
      endif
      return
      end

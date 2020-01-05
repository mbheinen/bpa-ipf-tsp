C    %W% %G%
C****************************************************************
C
C     File: add_ptiz.f
C
C     Purpose: Routine to define PTI zone hash index from zone name
C
c     Return code:  n > 0 : PTI zone hash number
c                   n < 0 : PTI zone hash number (duplicate entity)
c                   n = 0 : Error or termination flag
c
C     Note:  Arrays pti_znam() and pti_znum() are parallel, but are
C            constructed such that either name or number hashing points
c            to the same location.
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function add_ptiz (numzone, zonename)
      integer numzone
      character zonename*2

      include 'ipfinc/parametr.inc'
      include 'ipfinc/pti_data.inc'

      integer        h, p

c
c     Hash zone number
c
      h = mod (numzone, MAXCZN-1) + 1
      p = htable_z(h)
      do while (p .gt. 0)
        if (numzone .eq. pti_znum(p) .and.
     &      zonename .eq. pti_znam(p)) then
          p = -p
        else
          p = nextptr_z(p)
        endif
      enddo
      if (p .eq. 0) then
        num_znam = num_znam + 1
        p = num_znam
        nextptr_z(p) = htable_z(h)
        htable_z(h) = p
        pti_znum(p) = numzone
        pti_znam(p) = zonename             ! Reserved 
      endif
      add_ptiz = iabs(p)
c
c     Hash zone name
c
      if (iabs(add_ptiz) .gt. 0) then
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
          p = iabs(add_ptiz)
          nextptr_y(p) = htable_y(h)
          htable_y(h) = p
        endif
      endif
      return
      end

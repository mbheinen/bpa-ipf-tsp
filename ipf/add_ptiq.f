C    @(#)add_ptiq.f	20.4 2/28/00
C****************************************************************
C
C     File: add_ptiq.f
C
C     Purpose: Routine to find the PTI onwer hash index from the 
C              owner code, or create a unique number if non exist.
C
c     Return code:  n > 0 : PTI owner hash number
c                   n < 0 : Not found
c
C     Note:  Arrays pti_onam() and pti_onum() are parallel, but are
C            constructed such that either name or number hashing points
c            to the same location.
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function add_ptiq (ownercode)
      character ownercode*3

      include 'ipfinc/parametr.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/owner_cm.inc'

      integer        h, p, add_ptio
c
c     Hash owner code
c
      h = 0
      do i = 1, 3
        h = h + h + ichar (ownercode(i:i))
      end do
      h = mod (h, MAXOWN-1) + 1
      p = htable_q(h)
      do while (p .gt. 0)
        if (ownercode .ne. pti_onam(p)) then
        p = nextptr_q(p)
        else
          p = -p
        endif
      enddo
      if (p .eq. 0) then
        maxhashno = 0
        do p = 1, num_onam
          maxhashno = max0 (maxhashno, pti_onum(p))
        enddo
        add_ptiq = add_ptio (maxhashno+1, ownercode)
        p = add_ptiq
        owner_number(p) = maxhashno+1
        owner_code(p) = ownercode
        owner_name(p) = ownercode
      else
        add_ptiq = -p
      endif
      return
      end

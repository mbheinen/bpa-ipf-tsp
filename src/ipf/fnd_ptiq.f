C    @(#)fnd_ptiq.f	20.1 11/11/97
C****************************************************************
C
C     File: fnt_ptiq.f
C
C     Purpose: Routine to find the PTI onwer hash index from the 
C              owner code
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
      integer function fnd_ptiq (ownercode)
      character ownercode*3

      include 'ipfinc/parametr.inc'
      include 'ipfinc/pti_data.inc'

      integer        h, p
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
      fnd_ptiq = -p
      return
      end

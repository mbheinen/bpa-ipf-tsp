C    @(#)add_ptio.f	20.3 2/28/00
C****************************************************************
C
C     File: add_ptio.f
C
C     Purpose: Routine to define PTI onwer hash index from owner name
C
c     Return code:  n > 0 : PTI owner hash number
c                   n < 0 : PTI owner hash number (duplicate entity)
c                   n = 0 : Error or termination flag
c
C     Note:  Arrays pti_onam() and pti_onum() are parallel, but are
C            constructed such that either name or number hashing points
c            to the same location.
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function add_ptio (numowner, ownercode)
      integer numowner
      character ownercode*3

      include 'ipfinc/parametr.inc'
      include 'ipfinc/pti_data.inc'

      integer        h, p

c
c     Hash owner number
c
      h = mod (numowner, MAXOWN-1) + 1
      p = htable_o(h)
      do while (p .gt. 0)
        if (numowner .eq. pti_onum(p) .and.
     &      ownercode .eq. pti_onam(p)) then
          p = -p
        else
          p = nextptr_o(p)
        endif
      enddo
      if (p .eq. 0) then
        num_onam = num_onam + 1
        p = num_onam
        nextptr_o(p) = htable_o(h)
        htable_o(h) = p
        pti_onum(p) = numowner
        pti_onam(p) = ownercode            ! Reserved 
      endif
      add_ptio = iabs(p)
c
c     Hash owner name
c
      if (iabs(add_ptio) .gt. 0) then
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
          p = iabs(add_ptio)
          nextptr_q(p) = htable_q(h)
          htable_q(h) = p
        endif
      endif
      return
      end

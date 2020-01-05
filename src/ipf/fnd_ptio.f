C    @(#)fnd_ptio.f	20.1 11/11/97
C****************************************************************
C
C     File: fnt_ptio.f
C
C     Purpose: Routine to find the PTI onwer hash index from the 
C              owner number
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
      integer function fnd_ptio (numowner)
      integer numowner
      character ownercode*3

      include 'ipfinc/parametr.inc'
      include 'ipfinc/pti_data.inc'

      integer        h, p

      if (numowner .eq. 0) then
        fnd_ptio = 0
      else
c
c       Hash owner number
c
        h = mod (numowner, MAXOWN-1) + 1
        p = htable_o(h)
        do while (p .gt. 0)
          if (numowner .eq. pti_onum(p)) then
            p = -p
          else
            p = nextptr_o(p)
          endif
        enddo
        fnd_ptio = -p
      endif
      return
      end

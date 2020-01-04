C    @(#)fndpsptr.f	20.1 10/10/96
      integer function fndpsptr (ptr)
      integer ptr
C****************************************************************
C
C   File: qutdifrpt.f
C
C   Purpose: Routine to create/access branch pointer hash tables for
c            use with pseudo-buses.
C
c   Return values:  N > 0 : "ptr" is pre-existant entity at pseudo(1,N)
c                   N < 0 " "ptr" is new entity at pseudo(1,-N)
c                   N = 0 : "ptr" overflowed.
c
C   Author: Walt Powell  Date: 3 Oct 1996
c
C   Called by: ext_bus.f, ext_brn.f
C
C****************************************************************
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pseudo_b.inc'

      integer        h, i, p
      logical        status

      status = (num_pseudo .lt. MAX_PSEUDO)

      if (status) then
         h = mod (ptr, HASHSIZE) + 1
         p = htable(h)
         do while (p .gt. 0)         !search for existing entities
            if (ptr .ne. pseudo(1,p)) then
               p = nextptr(p)
            else
               p = -p                   
            endif
         enddo
         if (p .eq. 0) then
            num_pseudo = num_pseudo + 1
            pseudo(1,num_pseudo) = ptr
            nextptr(num_pseudo) = htable(h)
            htable(h) = num_pseudo
            fndpsptr = -num_pseudo       ! p < 0 flags new entity
            do i = 2, 8
              pseudo(i,num_pseudo) = 0
            enddo
         else
            fndpsptr = -p                ! p > 0 flags existing entity
         endif
      else
         fndpsptr = 0                    ! p = 0 flags overflow
      endif
      return
      end

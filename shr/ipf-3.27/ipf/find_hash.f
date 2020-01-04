C    @(#)find_hash.f	20.2 10/13/99
      integer function find_hash (i1, i2)
      integer i1, i2
C****************************************************************
C
C   File: find_hash
C
C   Purpose: A generic hashing routine using general arrays 
c            array1, array2, array3, array4
C
c   Return values:  N > 0 : "i1, i2, i3" is existing entity at (*,N)
c                   N < 0 " "i1, i2, i3" is new entity at (*,-N)
c                   N = 0 : overflowed.
c
C   Author: Walt Powell  Date: 1 Oct 1999
c
C   Called by: clnup_ge
C
C****************************************************************
C
      include 'ipfinc/parametr.inc'

      common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),
     &                 nextptr_2(MAXBUS)
      integer array, count, htable_2, nextptr_2

      integer HASHSIZE
      parameter (HASHSIZE = 1999)     !number of linked lists

      integer        h, p
      logical        status

      status = (count .lt. MAXBUS)

      if (status) then
         h = mod (i1, HASHSIZE) + 1
         p = htable_2(h)
         do while (p .gt. 0)         !search for existing entities
            if (i1 .ne. array(1,p) .or. i2 .ne. array(2,p)) then
               p = nextptr_2(p)
            else
               p = -p                   
            endif
         enddo
         if (p .eq. 0) then
            count = count + 1
            array(1,count) = i1
            array(2,count) = i2
            nextptr_2(count) = htable_2(h)
            htable_2(h) = count
            find_hash = -count            ! p < 0 flags new entity
         else
            find_hash = p                 ! p > 0 flags existing entity
         endif
      else
         find_hash = 0                    ! p = 0 flags overflow
      endif
      return
      end

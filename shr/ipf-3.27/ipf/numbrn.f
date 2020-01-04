C    @(#)numbrn.f	20.3 2/13/96
      function numbrn (k1, k2, id, ksect)
      character id * 1
 
C     This function determines the branch index for
C     K1 - K2 - ID - KSECT.  If NUMBRN > 0, the branch
C     is found in the PF branch data; if NUMBRN = 0, the
C     branch is not in the system.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
 
      integer ptr
      logical found
 
      if (k1 .le. 0 .or. k2 .le. 0) then
         numbrn = 0
      else
         numbrn = 0
         found = .false.
         ptr = kbsdta(16,k1)
         do while (ptr .gt. 0 .and. .not. found)
            if (ky(ptr) .eq. k2) then
               if (brtype(ptr)  .eq. 4 .or. brtype(ptr) .eq. 9) then
                  if (id .eq. '*') then
                     numbrn = ptr
                     found = .true.
                  endif
               else if (id .eq. '*') then
                  numbrn = ptr
                  found = .true.
               else if (brtype(ptr)  .eq. 7) then
                  numbrn = ptr
                  found = .true.
               else
                  if (brid(ptr) .eq. id) then
                     if (ksect .ge. 0) then
                        if (brsect(ptr) .eq. ksect) then
                           numbrn = ptr
                           found = .true.
                        endif
                     else
                        numbrn = ptr
                        found = .true.
                     endif
                  endif
               endif
            endif
            ptr = brnch_nxt(ptr)
         enddo
      endif
      return
      end

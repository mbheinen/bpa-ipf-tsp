C    @(#)xnumbrn.f	20.3 2/13/96
      integer function xnumbrn (k1, k2, id, ksect)
      character id * 1
 
C     This function determines the branch index for
C     K1 - K2 - ID - KSECT.  
c
C     If XNUMBRN > 0 means the branch is found in the PF branch data
C                = 0 means the branch is not in the system.
c
      include 'ipfinc/parametr.inc'
      include 'ipfinc/alt_case.inc'
 
      integer ptr
      logical found
 
      if (k1 .le. 0 .or. k2 .le. 0) then
         xnumbrn = 0
      else
         xnumbrn = 0
         found = .false.
         ptr = okbsdta(16,k1)
         do while (ptr .gt. 0 .and. .not. found)
            if (oky(ptr) .eq. k2) then
               if (obrtype(ptr)  .eq. 4 .or. obrtype(ptr) .eq. 9) then
                  if (id .eq. '*') then
                     xnumbrn = ptr
                     found = .true.
                  endif
               else if (id .eq. '*') then
                  xnumbrn = ptr
                  found = .true.
               else if (obrtype(ptr)  .eq. 7) then
                  xnumbrn = ptr
                  found = .true.
               else
                  if (obrid(ptr) .eq. id) then
                     if (ksect .ge. 0) then
                        if (obrsect(ptr) .eq. ksect) then
                           xnumbrn = ptr
                           found = .true.
                        endif
                     else
                        xnumbrn = ptr
                        found = .true.
                     endif
                  endif
               endif
            endif
            ptr = obrnch_nxt(ptr)
         enddo
      endif
      return
      end

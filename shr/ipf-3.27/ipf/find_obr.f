C    @(#)find_obr.f	20.1 7/18/96
C****************************************************************
C
C     File: find_obr.f
C
C     Purpose: Integer function to locate branch (K1,K2,ID,SECT)
C              in alternate reference) base 
C
C     Return value: N > 0 : Index in okx(), oky(),...
C                   N = 0 : Branch not found
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: ldaltbse.f
C
C****************************************************************
      integer function find_obr (k1, k2, id, ksect)
      character id * 1
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alt_case.inc'
 
      integer ptr
      logical found
 
      if (k1 .le. 0 .or. k2 .le. 0) then
         find_obr = 0
      else
         find_obr = 0
         found = .false.
         ptr = okbsdta(16,k1)
         do while (ptr .gt. 0 .and. .not. found)
            if (oky(ptr) .eq. k2) then
               if (obrtype(ptr)  .eq. 4 .or. obrtype(ptr) .eq. 9) then
                  if (id .eq. '*') then
                     find_obr = ptr
                     found = .true.
                  endif
               else if (id .eq. '*') then
                  find_obr = ptr
                  found = .true.
               else if (obrtype(ptr)  .eq. 7) then
                  find_obr = ptr
                  found = .true.
               else
                  if (obrid(ptr) .eq. id) then
                     if (ksect .ge. 0) then
                        if (obrsect(ptr) .eq. ksect) then
                           find_obr = ptr
                           found = .true.
                        endif
                     else
                        find_obr = ptr
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

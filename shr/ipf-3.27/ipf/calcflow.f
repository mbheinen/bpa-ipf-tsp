C    @(#)calcflow.f	20.4 5/27/98
        subroutine calcflow (k1, k2, id, pintot, qintot, pouttot, 
     &                       qouttot, error)
        integer error
        character id * 1
c***********************************************************************
c
c     File: calcflow.f
c
c     Purpose: Calculates line flow quantities for the given line. 
c
c     Author: Walt Powell                 Date: 7 December 1992
c
c     Modified: 
c
c     Called by: savewscc.f from the batch version
c
c***********************************************************************

C
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
c	Global variables used
c		bus, base, kbsdta
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
 
        integer ptr, ptrold, whichend1, whichend2
 	character ratec * 10
 
        pintot = 0.0
        qintot = 0.0
        pouttot = 0.0
        qouttot = 0.0
        plosstot = 0.0
        qlosstot = 0.0
 
        ptr = kbsdta(16,k1)
        do while (ptr .gt. 0 .and. (k2 .ne. ky(ptr)))
           ptr = brnch_nxt(ptr)
        enddo
        if (ptr .eq. 0) then
           write (errbuf(1), 100) bus(k1), base(k1), bus(k2), base(k2)
  100      format(' Cannot find branch ', a8, f6.1, 1x, a8, f6.1)
           call prterx ('W', 1)
           go to 900
        endif
        do while (ptr .gt. 0 .and. (k2 .eq. ky(ptr)))
           ltyp = brtype(ptr)
           if (ltyp .eq. 1) then
              call gtlfq (ptr, pin, qin, ploss, qloss, ovld, ratec,
     &                    actual_amps, whichend1, actual_mva, whichend2)
              if (id .eq. '*' .or. id .eq. brid(ptr)) then
                 pintot = pintot + pin
                 qintot = qintot + qin
                 pouttot = pouttot + pin - ploss
                 qouttot = qouttot + qin - qloss
                 plosstot = plosstot + ploss
                 qlosstot = qlosstot + qloss
              endif
              ptrold = ptr
              j = brnch_nxt(ptr)
              do while (j .gt. 0 .and.
     &                 (ky(j) .eq. k2 .and. brid(j) .eq. brid(ptr))) 
                 ptrold = j
                 j = brnch_nxt(j)
              enddo
              ptr = ptrold
           else if (ltyp .eq. 4) then
           else
              call gtlfq (ptr, pin, qin, ploss, qloss, ovld, ratec,
     &                    actual_amps, whichend1, actual_mva, whichend2)
              if (id .eq. '*' .or. id .eq. brid(ptr)) then
                 pintot = pintot + pin
                 qintot = qintot + qin
                 pouttot = pouttot + pin - ploss
                 qouttot = qouttot + qin - qloss
                 plosstot = plosstot + ploss
                 qlosstot = qlosstot + qloss
              endif
           endif
           ptr = brnch_nxt(ptr)
        enddo
  900   return
        end

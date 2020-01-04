C    @(#)gtmetpnt.f	20.3 2/13/96
        integer function gtmetpnt (ptr)
        integer ptr
C 
C       This function returns the metering point for branch PTR.
C
C       Returned value:
C         1 indicates metering point is at bus 1 (line losses or tie
c           line metering point assigned to bus 2)
C         2 indicates metering point is at bus 2 (line losses or tie
c           line metering point assigned to bus 1)
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/prt.inc'

c       Set up branch type codes... 
  
        include 'ipfinc/brtype.inc' 

        integer p, qptr, locsec(10), ptrsec(10)
        character own*3

        gtmetpnt = 0
        k1 = kx(ptr)
        k2 = ky(ptr)
        qptr = brnch_ptr(ptr)
        nbr = iabs(qptr)
        intovr = kbrnch(15,nbr)
        if (qptr .lt. 0 .and. intovr .gt. 0) intovr = 3 - intovr
        if (intovr .ne. 0) then
           gtmetpnt = intovr

        else if (brtype(ptr) .eq. BRTYP_LD) then   
  
c          If DC line, set metering point at receiving end 
 
           pdc = brnch(8,nbr)     
           if (qptr .lt. 0) pdc = -pdc 
           if (pdc .lt. 0.0) then  
              gtmetpnt = 1 
           else                                
              gtmetpnt = 2 
           endif                               

        else if (brtype(ptr) .eq. BRTYP_PEQ .or.
     &           brsect(ptr) .gt. 0) then 
c
c          Examine sections for consistency
c
           if (brtype(ptr) .eq. BRTYP_PEQ) then
              p = ptr
           else            ! Align p to BRTYP_PEQ entity
              p = kbsdta(16,k1)
              do while (p .gt. 0 .and. 
     &                 (ky(p) .ne. k2 .or. brid(p) .ne. brid(ptr)))
                 p = brnch_nxt(p)
              enddo
           endif

           jt = 0
           do while (p .gt. 0 .and. 
     &              (ky(p) .eq. k2 .and. brid(p) .eq. brid(ptr))) 
              jt = jt + 1
              ptrsec(jt) = p
              loc = 0
              qptr = brnch_ptr(p)
              nbr = iabs(qptr)
              intovr = kbrnch(15,nbr)
              if (qptr .lt. 0 .and. intovr .gt. 0) intovr = 3 - intovr
              if (brtype(p) .ne. BRTYP_PEQ) then
                 if (intovr .ne. 0) then
                    loc = intovr + 100
                 else
                    call getchr(3, own, kbrnch(3,nbr))
                    if (owner(k1) .eq. owner(k2)) then
                       loc = 1
                       if (inp2alf(k1) .gt. inp2alf(k2)) loc = 2
                    else
                       if (own .eq. owner(k1) .and. 
     &                     own .ne. owner(k2)) then
                          loc = 2 + 10
                       else if (own .eq. owner(k2) .and. 
     &                          own .ne. owner(k1)) then
                          loc = 1 + 10
                       else
                          loc = 1
                          if (inp2alf(k1) .gt. inp2alf(k2)) loc = 2
                       endif
                    endif
                 endif
              endif
              p = brnch_nxt(p)
              locsec(jt) = loc
           enddo
 
           loc1=0
           loc2=0
           do j=1,jt
              if (mod(locsec(j),10).eq.1) loc1=loc1+locsec(j)
              if (mod(locsec(j),10).eq.2) loc2=loc2+(locsec(j)-1)
           enddo
 
           if (loc1 .gt. loc2) then
              loc = 1
           else if (loc2 .gt. loc1) then
              loc = 2
           else if (inp2alf(k1) .lt. inp2alf(k2)) then
              loc = 1
           else
              loc = 2
           endif

C          Set Equivalent Pi to LOC

           gtmetpnt = loc

        else

           call getchr(3, own, kbrnch(3,nbr))
           if (owner(k1) .eq. owner(k2)) then
              gtmetpnt = 1
              if (inp2alf(k1) .gt. inp2alf(k2)) gtmetpnt = 2
           else
              if (own .eq. owner(k1) .and. own .ne. owner(k2)) then
                 gtmetpnt = 2
              else if (own .eq. owner(k2) .and. 
     &                 own .ne. owner(k1)) then
                 gtmetpnt = 1
              else
                 gtmetpnt = 1
                 if (inp2alf(k1) .gt. inp2alf(k2)) gtmetpnt = 2
              endif
           endif
        endif
        return
        end

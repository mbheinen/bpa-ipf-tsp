C    @(#)chkmet.f	20.3 2/13/96
        subroutine chkmet (k1, k2, id)
        character id*(*)
 
C       This subroutine checks "BRNCH" for consistency of metering
C       points in sections.
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/prt.inc'
C
        integer ptr, oldptr, qptr, locsec(10), ptrsec(10)
        character xbuf*120, lown*3, flag*1

C       Locate branch (k1-k2-id)

        oldptr = kbsdta(16,k1)
 
        do while (oldptr .gt. 0) 
           if (ky(oldptr) .eq. k2) then
              if (brid(oldptr) .eq. id) then
                 if (brtype(oldptr) .eq. 1) go to 530
              endif
           endif
           oldptr = brnch_nxt(oldptr)
        enddo
        go to 900

C       Determine metering point

  530   loc=0
        ptr = oldptr

        jt = 0
        do while (ptr .gt. 0 .and. 
     &           (ky(ptr) .eq. k2 .and. brid(ptr) .eq. id)) 
           jt = jt + 1
           ptrsec(jt) = ptr
           loc = 0
           qptr = brnch_ptr(ptr)
           nbr = iabs(qptr)
           intovr = kbrnch(15,nbr)
           if (qptr .lt. 0 .and. intovr .gt. 0) intovr = 3 - intovr
           if (brtype(ptr) .ne. 1) then
              if (intovr .ne. 0) then
                 loc = intovr + 100
              else
                 call getchr(3, lown, kbrnch(3,nbr))
                 if (owner(k1) .eq. owner(k2)) then
                   loc = 1
                   if (inp2alf(k1) .gt. inp2alf(k2)) loc = 2
                 else
                    if (lown .eq. owner(k1) .and.
     &                  lown .ne. owner(k2)) then
                       loc = 2 + 10
                    else if (lown .eq. owner(k2) .and. 
     &                       lown .ne. owner(k1)) then
                       loc = 1 + 10
                    else
                       loc = 1
                       if (inp2alf(k1) .gt. inp2alf(k2)) loc = 2
                    endif
                 endif
              endif
           endif
           ptr = brnch_nxt(ptr)
           locsec(jt) = loc
        enddo
 
C       Examine consistency of metering point
 
  574   loc1=0
        loc2=0
        do 580 j=1,jt
           if (mod(locsec(j),10).eq.1) loc1=loc1+locsec(j)
           if (mod(locsec(j),10).eq.2) loc2=loc2+(locsec(j)-1)
  580   continue
 
        if (loc1 .gt. loc2) then
           loc = 1
        else if (loc2 .gt. loc1) then
           loc = 2
        else if (inp2alf(k1) .lt. inp2alf(k2)) then
           loc = 1
        else
           loc = 2
        endif
        if (loc1.eq.0.and.loc2.gt.0) go to 650
        if (loc1.gt.0.and.loc2.eq.0) go to 650
        if (jarzn(k1) .ne. jarzn(k2)) then
 
           write (errbuf(1),590)
  590      format(' Inconsistent metering point on Line section ',
     1     'will cause Loss accounting errors. ("*" indicates ',
     2     'metering point.)')
 
           write (errbuf(2),600)
  600      format(' Metering point     ---------Line------------')
 
           do i = 1, jt
              ptr = ptrsec(i)
              if (brtype(ptr) .eq. 1) then
                 errbuf(i+2) = ' '
              else
                 call bcdbrn(ptr, xbuf)
                 loc1 = mod (locsec(i), 10)
                 flag = ' '
                 if (loc.eq.loc1) flag = '*'
                 write (errbuf(i+2),610) loc1,flag,xbuf(1:80)
  610            format(10x,i1,a1,9x,'(',a80,')')
              endif
           enddo
           call prterx ('W',jt+2)
        endif
 
C       Set Equivalent Pi to LOC

  650   qptr = brnch_ptr(oldptr)
        nbr = iabs (qptr)
        intovr = loc
        if (qptr .lt. 0 .and. intovr .gt. 0) then
           kbrnch(15,nbr) = 3 - intovr
        else
           kbrnch(15,nbr) = intovr
        endif 
  900   continue
        return
        end

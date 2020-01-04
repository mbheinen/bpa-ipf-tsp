C****************************************************************
C
C       File: bldownint.f
C       Purpose: Routine to build ownership analysis arrays
C
C       Author: Walt Powell  Date: 17 Feb 1995
C                            Modified: 17 Feb 1995
C       Called by:
C
C****************************************************************
	integer function bldownint ( status )

        integer status

        include 'ipfinc/parametr.inc'
  
        include 'ipfinc/anlys.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
      	include 'ipfinc/zbo.inc'
      	include 'ipfinc/owntie.inc'

        common /scratch/ sort(1000)
        integer sort

        external kompsrt2, swapsrt2

        integer buildzbo, ownsch, p, q, pold, whichend1, whichend2,
     &          section(10), count
        character lown*3, lownsec(10)*3, cbtype*1, cbown*3, 
     &            cbyear*2, idx*1, ratec*10
        logical finished

        save

        bldownint = 0
c
c       Build ownership array
c
        if (numown .eq. 0) status = buildzbo (status)

        write (*, 90)
   90   format (' * Rebuilding ownership interchange arrays. This will t
     &ake a minute.')

        do i = 0, numown
           own1_inx(i) = 0
        enddo

        do i = 1, numown
           j = alf2own(i)
           own2alf(j) = i
        enddo

c       Build tie array     
                            
        numtie = 0
        numref = 0          
        numptr = 0
  
        do nbx = 1, ntot_alf
           nb = alf2inp(nbx)
           iown1 = ownsch (owner(nb))
           iown1 = own2alf(iown1)
c
c          Loop through continuation buses
c
           p = kbsdta(15,nb) 
           do while (p .gt. 0) 
              call getchr (1, cbtype, kbctbl(8,p))
              call getchr (3, cbown, kbctbl(10,p))
              call getchr (2, cbyear, kbctbl(9,p))
              iown2 = ownsch (cbown)
              iown2 = own2alf(iown2)
              if (iown1 .ne. iown2) then
                 if (numtie .gt. MAXOWNTIE) go to 900
                 numtie = numtie + 1
                 lowntie(1,numtie) = 1      ! bus -> + bus
                 lowntie(2,numtie) = iown1
                 lowntie(3,numtie) = iown2
                 lowntie(4,numtie) = nb 
                 lowntie(5,numtie) = p

                 status = link_own(iown1, iown2, numtie)
                 status = link_own(iown2, iown1, -numtie)
              endif
              p = bctbl_nxt(p)
           enddo
c
c          Loop through branches
c
           p = kbsdta(16,nb) 
           do while (p .gt. 0) 
              ltyp = brtype(p)
              k2 = ky(p)
              idx = brid(p)
              if (ltyp .eq. 1) then
                 pold = p
                 p = brnch_nxt(p)
                 numsec = 0
                 do while (p .gt. 0 .and.
     &                    (ky(p) .eq. k2 .and. brid(p) .eq. idx))
                    nbr = iabs (brnch_ptr(p))
                    numsec = numsec + 1
                    call getchr (3, lownsec(numsec), kbrnch(3,nbr))
                    section(numsec) = p
                    p = brnch_nxt(p)
                 enddo
                 lown = lownsec(1)
                 iown2 = ownsch (lown)
                 iown2 = own2alf(iown2)
c
c                Check for type 2 interface (B -> L)
c
                 if (iown1 .ne. iown2) then
                    call gtlfq (pold, pin, qin, ploss, qloss, ovld, 
     &                          ratec, actual_amps, whichend1, 
     &                          actual_mva, whichend2)
                    call gtlfq (section(1), pin, qin, ploss, 
     &                          qloss, ovld, ratec, actual_amps, 
     &                          whichend1, actual_mva, whichend2)
                    meter_pnt = 1
                    if (numtie .gt. MAXOWNTIE) go to 900
                    numtie = numtie + 1
                    lowntie(1,numtie) = 2      ! bus -> branch 
                    lowntie(2,numtie) = iown1
                    lowntie(3,numtie) = iown2
                    lowntie(4,numtie) = nb 
                    lowntie(5,numtie) = section(1)
                    lowntie(6,numtie) = meter_pnt
                    if (meter_pnt .eq. 1) then
                       owntie(1,numtie) = pin
                       owntie(2,numtie) = qin
                    else
                       owntie(1,numtie) = pin - ploss
                       owntie(2,numtie) = qin - qloss
                    endif
c
c                   link owner1-owner2 and owner2-owner1
c
                    status = link_own(iown1, iown2, numtie)
                    status = link_own(iown2, iown1, -numtie)
                 endif
c
c                Check for type 3 interface (L -> L)
c
                 if (inp2alf(kx(pold)) .lt. inp2alf(ky(pold))) then
                    call gtlfq (pold, pin, qin, ploss, qloss, ovld, 
     &                          ratec, actual_amps, whichend1, 
     &                          actual_mva, whichend2)
                    n = 1
                    do while (n .lt. numsec)
                       call gtlfq (section(n), pin, qin, ploss, qloss, 
     &                             ovld, ratec, actual_amps, 
     &                             whichend1, actual_mva, whichend2)
                       if (lownsec(n) .ne. lownsec(n+1)) then
                          if (numtie .gt. MAXOWNTIE) go to 900
                          numtie = numtie + 1
                          lowntie(1,numtie) = 3      ! Branch -> branch 
                          iown2 = ownsch (lownsec(n))
                          iown2 = own2alf(iown2)
                          lowntie(2,numtie) = iown2
                          iown3 = ownsch (lownsec(n+1))
                          iown3 = own2alf(iown3)
                          lowntie(3,numtie) = iown3
                          lowntie(4,numtie) = section(n)
                          lowntie(5,numtie) = section(n+1)
                          meter_pnt = 2
                          lowntie(6,numtie) = meter_pnt
                          if (meter_pnt .eq. 1) then
                             owntie(1,numtie) = pin
                             owntie(2,numtie) = qin
                          else
                             owntie(1,numtie) = pin - ploss
                             owntie(2,numtie) = qin - qloss
                          endif
c
c                         link owner1-owner2 and owner2-owner1
c
                          status = link_own(iown2, iown3, numtie)
                          status = link_own(iown3, iown2, -numtie)
                       endif
                       n = n + 1
                    enddo
                 endif

              else if (ltyp .eq. 4) then
                 p = brnch_nxt(p)
              else
                 nbr = iabs (brnch_ptr(p))
                 call getchr (3, lown, kbrnch(3,nbr))
                 iown2 = ownsch (lown)
                 iown2 = own2alf(iown2)
                 if (iown1 .ne. iown2) then
c
c                   link owner1-owner2 and owner2-owner1
c
                    call gtlfq (p, pin, qin, ploss, qloss, ovld, 
     &                          ratec, actual_amps, whichend1, 
     &                          actual_mva, whichend2)
                    if (numtie .gt. MAXOWNTIE) go to 900
                    numtie = numtie + 1
                    lowntie(1,numtie) = 2      ! bus -> branch 
                    lowntie(2,numtie) = iown1
                    lowntie(3,numtie) = iown2
                    lowntie(4,numtie) = nb 
                    lowntie(5,numtie) = p
                    meter_pnt = 1
                    lowntie(6,numtie) = meter_pnt
                    if (meter_pnt .eq. 1) then
                       owntie(1,numtie) = pin
                       owntie(2,numtie) = qin
                    else
                       owntie(1,numtie) = pin - ploss
                       owntie(2,numtie) = qin - qloss
                    endif

                    status = link_own(iown1, iown2, numtie)
                    status = link_own(iown2, iown1, -numtie)
                 endif
                 p = brnch_nxt(p)
              endif
           enddo
        enddo
c
c       Resort ownership interchange cross-reference arrays
c
        finished = .false.
        do io = 1, numown
           p = own1_inx(io)
           do while (p .gt. 0)
              jo = own1_own2(p)
              count = 0
              q = own1_ref(p)
              do while (q .ne. 0)
                 count = count + 1
                 sort(count) = own1_tie_ptr(q)
                 q = own1_tie_nxt(q)
              enddo
              call shellsrt (1, count, kompsrt2, swapsrt2)
              icount = 0
              q = own1_ref(p)
              do while (q .ne. 0)
                 icount = icount + 1
                 own1_tie_ptr(q) = sort(icount)
                 q = own1_tie_nxt(q)
              enddo
              p = own1_nxt(p)
           enddo
        enddo
        go to 920
                 
  900   write (errbuf(1), 910) MAXOWNTIE
  910   format (' Overflow of ownership tie array - limit is ', i4)
        call prterx ('W', 1)

  920   continue
        return
        end

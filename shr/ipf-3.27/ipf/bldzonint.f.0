C****************************************************************
C
C       File: bldzonint.f
C       Purpose: Routine to build zone interchange analysis arrays
C
C       Author: Walt Powell  Date: 17 Feb 1995
C                            Modified: 17 Feb 1995
C       Called by:
C
C****************************************************************
	integer function bldzonint ( status ) 

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

        integer buildzbo, find_zon, p, pold, whichend1, whichend2,
     &          gtmetpnt
        character lown*3, idx*1, ratec*10

        save

        bldzonint = 0
c
c       Build zone interchange array
c
        if (numown .eq. 0) status = buildzbo (status)

        write (*, 90)
   90   format (' * Rebuilding zone interchange arrays. This will take a
     & minute.')

        if (nztot .gt. MAXOWNTIE) go to 900

        do i = 0, nztot
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
           izone1 = find_zon(zone(nb))
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
                 do while (p .gt. 0 .and.
     &                    (ky(p) .eq. k2 .and. brid(p) .eq. idx))
                    nbr = iabs (brnch_ptr(p))
                    call getchr (3, lown, kbrnch(3,nbr))
                    p = brnch_nxt(p)
                 enddo
                 izone2 = find_zon(zone(ky(pold)))
                 if (izone1 .ne. izone2 .and. 
     &               inp2alf(kx(pold)) .lt. inp2alf(ky(pold))) then
                    call gtlfq (pold, pin, qin, ploss, qloss, ovld, 
     &                          ratec, actual_amps, whichend1, 
     &                          actual_mva, whichend2)
                    meter_pnt = gtmetpnt(pold)
                    if (numtie .gt. MAXOWNTIE) go to 900
                    numtie = numtie + 1
                    lowntie(1,numtie) = 2      ! bus -> branch 
                    lowntie(2,numtie) = izone1
                    lowntie(3,numtie) = izone2
                    lowntie(4,numtie) = nb 
                    lowntie(5,numtie) = pold
                    meter_pnt = gtmetpnt(pold)
                    lowntie(6,numtie) = meter_pnt
                    if (meter_pnt .eq. 1) then
                       owntie(1,numtie) = pin
                       owntie(2,numtie) = qin
                    else
                       owntie(1,numtie) = pin - ploss
                       owntie(2,numtie) = qin - qloss
                    endif
c
c                   link zone1-zone2 and zone2-zone1
c
                    status = link_own(izone1, izone2, numtie)
                    status = link_own(izone2, izone1, -numtie)
                 endif
              else if (ltyp .eq. 4) then
                 p = brnch_nxt(p)
              else
                 izone2 = find_zon(zone(ky(p)))
                 if (izone1 .ne. izone2 .and. 
     &               inp2alf(kx(p)) .lt. inp2alf(ky(p))) then
c
c                   link zone1-zone2 and zone2-zone1
c
                    call gtlfq (p, pin, qin, ploss, qloss, ovld, 
     &                          ratec, actual_amps, whichend1, 
     &                          actual_mva, whichend2)
                    if (numtie .gt. MAXOWNTIE) go to 900
                    numtie = numtie + 1
                    lowntie(1,numtie) = 2      ! bus -> branch 
                    lowntie(2,numtie) = izone1
                    lowntie(3,numtie) = izone2
                    lowntie(4,numtie) = nb 
                    lowntie(5,numtie) = p
                    meter_pnt = gtmetpnt(p)
                    lowntie(6,numtie) = meter_pnt
                    if (meter_pnt .eq. 1) then
                       owntie(1,numtie) = pin
                       owntie(2,numtie) = qin
                    else
                       owntie(1,numtie) = pin - ploss
                       owntie(2,numtie) = qin - qloss
                    endif

                    status = link_own(izone1, izone2, numtie)
                    status = link_own(izone2, izone1, -numtie)
                 endif
                 p = brnch_nxt(p)
              endif
           enddo
        enddo
        go to 920

  900   write (*, 910) 
  910   format (' Fatal error - data overflow')
        bldzonint = 1

  920   continue
        return
        end

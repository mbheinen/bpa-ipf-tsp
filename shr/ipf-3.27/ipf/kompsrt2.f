C    @(#)kompsrt2.f	20.3 2/13/96
C****************************************************************
C
C       File: kompsrt2.f
C       Purpose: Routine to compare ownership entities in array
C                lowntie().
C
C       Author: Walt Powell  Date: 6 Jul 1995
C                            Modified: 
C       Called by:
C
C****************************************************************
	integer function kompsrt2 (m, n)
        integer m, n

        include 'ipfinc/parametr.inc'
  
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
      	include 'ipfinc/zbo.inc'
      	include 'ipfinc/owntie.inc'

        common /scratch/ sort(1000)
        integer sort

        integer q, ptr
        character cbtype * 1, cbown * 3, cbyear * 2, lown * 3, 
     &            type11 * 10, type12 * 10, type21 * 10, type22 * 10,
     &            brntyp * 2
c
c       Retrieve attributes of argument "m"
c
        if (m .eq. n) then
           kompsrt2 = 0
        else
           i = sort(m)
           ix = iabs (i)
           if (lowntie(1,ix) .eq. 1) then
              ncb = lowntie(5,ix)
              call getchr (1, cbtype, kbctbl(8,ncb))
              call getchr (3, cbown, kbctbl(10,ncb))
              call getchr (2, cbyear, kbctbl(9,ncb))
              k11 = kbctbl(1,ncb)
              k12 = 0
              if (i .gt. 0) then
                 type11 = 'B ' // owner(k11)
                 type12 = '+' // cbtype // cbown // cbyear
              else
                 type11 = '+' // cbtype // cbown // cbyear
                 type12 = 'B ' // owner(k11)
              endif
           else if (lowntie(1,ix) .eq. 2) then
              ptr = lowntie(5,ix)
              nbr = iabs (brnch_ptr(ptr))
              call getchr (3, lown, kbrnch(3,nbr)) 
              if (i .gt. 0) then
                 k11 = kx(ptr)
                 k12 = ky(ptr)
                 type11 = 'B ' // owner(k11)
                 type12 = 'L ' // lown // brid(ptr) // char(brsect(ptr))
              else
                 k11 = ky(ptr)
                 k12 = kx(ptr)
                 type11 = 'L ' // lown // brid(ptr) // char(brsect(ptr))
                 type12 = 'B ' // owner(k11)
              endif
           else
              ptr = lowntie(4,ix)
              nbr = iabs (brnch_ptr(ptr))
              call getchr (3, lown, kbrnch(3,nbr)) 
              if (i .gt. 0) then
                 k11 = kx(ptr)
                 k12 = ky(ptr)
              else
                 k11 = ky(ptr)
                 k12 = kx(ptr)
              endif
              type11 = 'L ' // lown // brid(ptr) // char(brsect(ptr))
              ptr = lowntie(5,ix)
              nbr = iabs (brnch_ptr(ptr))
              call getchr (3, lown, kbrnch(3,nbr)) 
              type12 = 'L ' // lown // brid(ptr) // char(brsect(ptr))
           endif
c
c          Retrieve attributes of argument "n"
c
           i = sort(n)
           ix = iabs (i)
           if (lowntie(1,ix) .eq. 1) then
              ncb = lowntie(5,ix)
              call getchr (1, cbtype, kbctbl(8,ncb))
              call getchr (3, cbown, kbctbl(10,ncb))
              call getchr (2, cbyear, kbctbl(9,ncb))
              k21 = kbctbl(1,ncb)
              k22 = 0
              if (i .gt. 0) then
                 type21 = 'B ' // owner(k21)
                 type22 = '+' // cbtype // cbown // cbyear
              else
                 type22 = 'B ' // owner(k21)
                 type21 = '+' // cbtype // cbown // cbyear
              endif
           else if (lowntie(1,ix) .eq. 2) then
              ptr = lowntie(5,ix)
              nbr = iabs (brnch_ptr(ptr))
              call getchr (3, lown, kbrnch(3,nbr)) 
              if (i .gt. 0) then
                 k21 = kx(ptr)
                 k22 = ky(ptr)
                 type21 = 'B ' // owner(k21)
                 type22 = 'L ' // lown // brid(ptr) // char(brsect(ptr))
              else
                 k21 = ky(ptr)
                 k22 = kx(ptr)
                 type21 = 'L ' // lown // brid(ptr) // char(brsect(ptr))
                 type22 = 'B ' // owner(k21)
              endif
           else
              ptr = lowntie(4,ix)
              nbr = iabs (brnch_ptr(ptr))
              call getchr (3, lown, kbrnch(3,nbr)) 
              if (i .gt. 0) then
                 k21 = kx(ptr)
                 k22 = ky(ptr)
              else
                 k21 = ky(ptr)
                 k22 = kx(ptr)
              endif
              type21 = 'L ' // lown // brid(ptr) // char(brsect(ptr))
              ptr = lowntie(5,ix)
              nbr = iabs (brnch_ptr(ptr))
              call getchr (3, lown, kbrnch(3,nbr)) 
              type22 = 'L ' // lown // brid(ptr) // char(brsect(ptr))
           endif
       
           kompsrt2 = inp2alf(k11) - inp2alf(k21)
           if (kompsrt2 .eq. 0) then
              kompsrt2 = index ('B+L', type11(1:1)) 
     &                 - index ('B+L', type21(1:1))
           endif
           if (kompsrt2 .eq. 0) then
              kompsrt2 = index ('B+L', type12(1:1)) 
     &                 - index ('B+L', type22(1:1))
           endif
           if (kompsrt2 .eq. 0) then
              kompsrt2 = kompr (type11, type21, junk)
           endif
           if (kompsrt2 .eq. 0 .and. min0 (k12, k22) .eq. 0) then
              kompsrt2 = kompr (type12, type22, junk)
           endif
           if (kompsrt2 .eq. 0 .and. min0 (k12, k22) .gt. 0) then
              kompsrt2 = inp2alf(k12) - inp2alf(k22)
           endif
           if (kompsrt2 .eq. 0) then
              kompsrt2 = kompr (type12, type22, junk)
           endif
        endif
        return
        end

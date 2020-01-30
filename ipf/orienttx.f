C    @(#)orienttx.f	20.3 2/13/96
	integer function orienttx (p1, p2, k1, k2, tap1, tap2)
        integer p1, p2
c
c       This function determines the LTC polarity branch kx(p1), 
c       ky(p1).
c
c       Input parameters:
c
c            p1   : index to "R" record in double-entry branch data.
c            p2   : index to "T" record in double-entry branch data.
c
c       Output parameters:
c
c           k1    : fixed tap side
c           k2    : variable tap side
c           tap1  : fixed tap (or phase shift)
c           tap2  : variable tap
c
c        orienttx : Orientation of "R in KX(p1), KY(p1) with respect to 
c                   fixed tap - variable tap
c
c              1 = same orientation
c              2 = opposite orientation.
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/tran.inc'
        include 'ipfinc/lfiles.inc'
c
        integer rtype, q1, q2, aq1, aq2
        character type * 1

        k1 = kx(p1)
        k2 = ky(p1)

        q1 = brnch_ptr(p1)
        aq1 = iabs(q1)

        tmax = brnch(6,aq1)
        tmin = brnch(7,aq1)

        write (type, '(a1)') brnch(3,aq1)
        rtype = index (' VQNPM', type)
        if (rtype .eq. 5 .or. rtype .eq. 6) then

c          Phase shifters are constructed in LTRAN always as 
c          low to high (fixed to variable)
c
           if (inp2alf(k1) .lt. inp2alf(k2)) then
              orienttx = 1
           else
              orienttx = 2
           endif
        else
c
c          Find variable tap side
c
c          1. Assign by "ktpsde"
c
           if (q1 .gt. 0) then
              ktpsde = kbrnch(15,aq1)
           else
              ktpsde = mod(3-kbrnch(15,aq1),3)
           endif
           if (ktpsde .eq. 1) then
              orienttx = 2
           else if (ktpsde .eq. 2) then
              orienttx = 1
           else
c
c             2. Assign by tmax, tmin
c
              t1 = dim(base(k1),tmax) - dim(tmin,base(k1))
              t2 = dim(base(k2),tmax) - dim(tmin,base(k2))
              if (abs(t1) .lt. abs(t2)) then
                 orienttx = 2
              else if (abs(t1) .gt. abs(t2)) then
                 orienttx = 1
              else
c
c                3. Assign by "list"
c
                 if (q1 .lt. 0) then
                    orienttx = 2
                 else
                    orienttx = 1
                 endif
              endif
           endif
        endif

        if (orienttx .eq. 1) then
        else
           k1 = ky(p1)
           k2 = kx(p1)
        endif
        q2 = brnch_ptr(p2)
        aq2 = iabs(q2)
c
c       ktmap : Orientation of "T in BRNCH(*,aq2) with respect to 
c                  fixed tap - variable tap
c
c              1 = same orientation
c              2 = opposite orientation.
c
        if ((orienttx .eq. 1 .and. q2 .gt. 0) .or.
     &      (orienttx .eq. 2 .and. q2 .lt. 0)) then
           ktmap = 1
        else
           ktmap = 2
        endif
        if (rtype .lt. 5) then
           if (ktmap .eq. 1) then
              tap1 = brnch(9,aq2)
              tap2 = brnch(10,aq2)
           else
              tap2 = brnch(9,aq2)
              tap1 = brnch(10,aq2)
           endif
        else
           if (ktmap .eq. 1) then
              tap1 = brnch(9,aq2)
              tap2 = brnch(10,aq2)
           else
              tap1 = -brnch(9,aq2)
              tap2 = brnch(10,aq2)
           endif
        endif
        return
        end

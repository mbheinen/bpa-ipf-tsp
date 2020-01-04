C    @(#)rgone.f	20.6 7/18/96
      subroutine rgone(kerr)

C     improve G/B ratios by truncating G
C     IF G/B < -1.0 or G/B > 0.50.

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		gkmu(r*8), bkmu(r*8), gkku(r*8), bkku(r*8),
c		km, kmlen, ikmu
      include 'ipfinc/blank.inc'
c	Global variables used:
c		ntot
      include 'ipfinc/branch.inc'
c	Global variables used:
c		brnch_nxt, brid, brnch, brnch_ptr, kbrnch, 
c		ky, brtype, brsect
      include 'ipfinc/bus.inc'
c	Global variables used:
c		opt2inp, kbsdta, e(r*8), f(r*8)
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		intbus, intbas
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/prt.inc'
c
      complex * 16 oldy(2,2), y(2,2), v(2), z, a(2), b(2)
c
      integer ptr, p
c
      character id*1
c
      external komparbr, swap_br

C     Initialize terminators.

C     Examine G/B ratio of all branches

      do kt = 1, ntot
        kf = km(kt)
        kl = kf + kmlen(kt) - 1
        do l = kf, kl
          if (bkmu(l) .ne. 0.0) then
             ratio = gkmu(l)/bkmu(l)
          else
             ratio = 1.0
          endif
          if (ratio .le. -1.00 .or. ratio .ge. 0.50) then

C           Modify existing branch

            mt = ikmu(l)
            kb = opt2inp(kt)
            mb = opt2inp(mt)
            v(1) = cmplx(e(kt), f(kt))
            v(2) = cmplx(e(mt), f(mt))
            ptr = kbsdta(16, kb)
            do while (ptr .gt. 0 .and. (ky(ptr) .ne. mb))
               ptr = brnch_nxt(ptr)
            enddo
            do while (ptr .gt. 0 .and. (ky(ptr) .eq. mb))
              nbr = iabs(brnch_ptr(ptr))
              ltype = brtype(ptr)
              if (ltype .ne. 4) then
                if (ltype .eq. 6) goto 100
                id = brid(ptr)
                call pieqiv(ptr, oldy, ierr)
                if (ltype .eq. 1) then
                  nxtptr = brnch_nxt(ptr)
                  nxtnbr = iabs(brnch_ptr(nxtptr))
c
c                 Double entry - change single entry only on second pass
c
                  if (kt .gt. mt) then
                    do i = 1, 18
                      kbrnch(i, nbr) = kbrnch(i, nxtnbr)
                    enddo
                  endif
                endif
                yb = sngl(dimag(oldy(1, 2)))
                yg = sngl(dreal(oldy(1, 2)))
                y(1, 2) = dcmplx(0.0, yb)
                y(1, 1) = oldy(1, 1) + v(2)/v(1)*(oldy(1, 2)-y(1, 2))
                z =  - dcmplx(1.0, 0.0)/y(1, 2)
                yb = sngl(dimag(oldy(2, 1)))
                yg = sngl(dreal(oldy(2, 1)))
                y(2, 1) = dcmplx(0.0, yb)
                y(2, 2) = oldy(2, 2) + v(1)/v(2)*(oldy(2, 1)-y(2, 1))
                if (ltype .eq. 5) then
                  if (brnch_ptr(ptr) .gt. 0) then
                    tk=brnch(9,nbr)/base(kb)   
                    tm=brnch(10,nbr)/base(mb)    
                  else
                    tk=brnch(10,nbr)/base(kb)    
                    tm=brnch(9,nbr)/base(mb)   
                  endif
                  tkm=1.0/(tk*tm)       
                  z =  -dcmplx(1.0, 0.0) / y(1, 2) * dcmplx (tk*tm, 0.0)
                  r = dreal (z)
                  x = dimag (z)
                  z = y(1, 1) + y(1, 2) * dcmplx (tm/tk, 0.0)
     &              + y(2, 2) * dcmplx ((tm/tk)**2, 0.0) 
     &              + y(2, 1) * dcmplx (tm/tk, 0.0)
                  z = dcmplx (1.0/tkm, 0.0) * z
                  yg = sngl(dreal (z))
                  yb = sngl(dimag (z))
                  if (yb .gt. 0.02) then
                    write (errbuf(1), 10010) bus(kb), base(kb), bus(mb), 
     &                  base(mb)
10010               format ('Modified 2-port Y for Tx has B > 0 '
     &                      , a8, f7.1, 1x, a8, f7.1)
                    call prterx ('W', 1)
                  endif
                  brsect(ptr) = 0
                else
                  brtype(ptr) = 8
                  brsect(ptr) = 0
                  z =  -dcmplx(1.0, 0.0) / y(1, 2) * dcmplx (1.0, 0.0)
                  r = dreal (z)
                  x = dimag (z)
                endif
c
c               Double entry - change single entry only on second pass
c
                if (kt .gt. mt) then
                  brnch(5, nbr) = r
                  brnch(6, nbr) = x
                  if (brtype(ptr) .eq. 8) then
                    if (brnch_ptr(ptr) .gt. 0) then
                       brnch(7, nbr) = sngl(dreal(y(1, 1)+y(1, 2)))
                       brnch(8, nbr) = sngl(dimag(y(1, 1)+y(1, 2)))
                       brnch(9, nbr) = sngl(dreal(y(2, 2)+y(2, 1)))
                       brnch(10, nbr) = sngl(dimag(y(2, 2)+y(2, 1)))
                    else
                       brnch(7, nbr) = sngl(dreal(y(2, 2)+y(2, 1)))
                       brnch(8, nbr) = sngl(dimag(y(2, 2)+y(2, 1)))
                       brnch(9, nbr) = sngl(dreal(y(1, 1)+y(1, 2)))
                       brnch(10, nbr) = sngl(dimag(y(1, 1)+y(1, 2)))
                    endif
                  else
                    if (brnch_ptr(ptr) .gt. 0) then
                       brnch(7, nbr) = yg
                       brnch(8, nbr) = yb
                    else
                       brnch(7, nbr) = yg
                       brnch(8, nbr) = yb
                    endif
                  endif
                endif
                gkmu(l) = gkmu(l) + dreal(y(1, 2)-oldy(1, 2))
                bkmu(l) = bkmu(l) + dimag(y(1, 2)-oldy(1, 2))
                gkku(kt) = gkku(kt) + dreal(y(1, 1)-oldy(1, 1))
                bkku(kt) = bkku(kt) + dimag(y(1, 1)-oldy(1, 1))

                a(1) = oldy(1, 1)*v(1) + oldy(1, 2)*v(2)
                a(2) = oldy(2, 1)*v(1) + oldy(2, 2)*v(2)
                b(1) = y(1, 1)*v(1) + y(1, 2)*v(2)
                b(2) = y(2, 1)*v(1) + y(2, 2)*v(2)

                write (dbug, 10000) intbus(kt), intbas(kt), intbus(mt), 
     &           intbas(mt), id, oldy, y, gkku(kt), bkku(kt), gkmu(l), 
     &           bkmu(l), a, b
10000           format (' Modify G/B on branch  ', a8, f7.1, 1x, a8, 
     &           f7.1, 1x, a, 1x, /, '         2-port Y(old) ', 8e12.5, 
     &           /, '         2-port Y(new) ', 8e12.5, /, 
     &           '                Y      ', 4e12.5, /, 
     &           '                A(old) ', 4e12.5, /, 
     &           '                A(new) ', 4e12.5)

C               Eliminate sections by changing pointer

                p = brnch_nxt(ptr)
                do while (p .gt. 0 .and. 
     &                   (ky(p) .eq. mb .and. brid(p) .eq. id))
                  p = brnch_nxt(p)
                enddo
                brnch_nxt(ptr) = p
              endif
              ptr = brnch_nxt(ptr)
            enddo
          endif
  100     continue
        enddo
      enddo
      return
      end

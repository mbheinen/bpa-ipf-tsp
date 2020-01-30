C    @(#)sympha.f	20.6 7/18/96
      subroutine sympha (idebug)
 
C     change selected phase shifters to symmetric PI branches. 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		gkku(r*8), bkku(r*8), gkmu(r*8), bkmu(r*8),
c		km, kmlen
      include 'ipfinc/blank.inc'
c	Global variables used:
c		bmva, jphno
      include 'ipfinc/branch.inc'
c	Global variables used:
c		brnch_nxt, brid, brnch, brnch_ptr, brtype, brsect, ky
      include 'ipfinc/bus.inc'
c	Global variables used:
c		bus, inp2opt, kbsdta, base, e(r*8), f(r*8)
      include 'ipfinc/cbus.inc'
c	Global variables used:
c		bctbl, kbctbl, bctbl_nxt
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/phase.inc'
c	Global variables used:
c		jphid
      include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
      include 'ipfinc/red7.inc'
c	Global variables used:
c		None
      include 'ipfinc/slnopt.inc'
c	Global variables used:
c		None
 
      common /scratch/ nbr, array(2,100),
     &                 ndel1, lindel1(MAXBRN), ndel2,
     &                 lindel2(MAXBRN2), ndel3, cbdel(MAXBUS),
     &                 br_status(MAXBRN)
c
      complex * 16 oldy(2,2), y(2,2), v(2), a(2), s(2), ydiff
c
      integer array, cbdel, br_status
c
c      complex oldy(2,2),y(2,2),v(2),a(2),s(2), ydiff
c
      character id*1, kode*1, kodeyr*2, kodeow*3
c
      integer ptr, qptr, status, add_eqcbs, oldptr, find_br, ptr2
c
      logical found, finished
c
      real pold(2), qold(2), pnew(2), qnew(2)
c 
      ndel3 = 0     ! Set counter for use with add_eqcbs

      do 240 jt=1,jphno
         j1=jphid(1,jt)
         j2=jphid(2,jt)
c
C        Phase shifters flagged for symmetric-conversion have at least 
C        one negative terminal number.   
c
         if (min0 (j1,j2) .gt. 0) go to 240   
         jphid(1,jt) = 0 
         jphid(2,jt) = 0 
         call getchr (1, id, jphid(3,jt))
c
C        Find phase shifter in BRNCH array
c
         k1 = iabs(j1) 
         k2 = iabs(j2) 
         ptr = kbsdta(16,k1) 
         found = .false.
         do while (ptr .gt. 0 .and. .not. found)
            if (ky(ptr) .eq. k2 .and. 
     &          brid(ptr) .eq. id .and. 
     &          brtype(ptr) .ne. 4) then
               found = .true.
            else
               ptr = brnch_nxt(ptr)
            endif
         enddo
         if (.not. found) then
            call erexit
         endif
         call pieqiv (ptr, oldy, ierr)   

C        Set phase shift to zero and compute new Y-matrix 

         finished = .false.
         oldptr = ptr
         isect = 0
         do while (ptr .gt. 0 .and. .not. finished)
            nbr = iabs (brnch_ptr(ptr))
            if (ky(ptr) .eq. k2 .and. 
     &          brid(ptr) .eq. id) then
               if (brtype(ptr) .eq. 6) then
                  if (brnch_ptr(ptr) .gt. 0) then
                     angle = brnch(9,nbr) 
                  else
                     angle = -brnch(9,nbr) 
                  endif
c
c                 Convert phase shifter to a transformer
c
                  brtype(ptr) = 5
c
c                 Change transpose entity also
c
                  ptr2 = find_br (k2, k1, brid(ptr), brsect(ptr), 6)
                  brtype(ptr2) = 5

                  if (brnch_ptr(ptr) .gt. 0) then
                     brnch(9,nbr) = base(k1)
                     brnch(10,nbr) = base(k2)
                  else
                     brnch(9,nbr) = base(k2)
                     brnch(10,nbr) = base(k1)
                  endif

C                 Set X > 0.005

                  r = brnch(5,nbr)
                  x = brnch(6,nbr)
                  z = sqrt (r**2 + x**2)
                  if (z .lt. 0.005) then
                     brnch(5,nbr) = r * 0.005 / z
                     brnch(6,nbr) = x * 0.005 / z
                     write (errbuf(1),120) bus(k1), base(k1), 
     &                  bus(k2), base(k2), z
  120                format('Phase shifter ',a8,f7.1,2x,a8,f7.1,
     &                  ' impedance (',f6.5,
     &               ') is raised to (0.005) to improve solvability')
                     call prterx ('W',1)
                  endif
               endif
               if (brtype(ptr) .ne. 1 .and. brtype(ptr) .ne. 4) then
                  call pieqiv (ptr, y, ierr)
                  if (isect .eq. 0) then
                     call firsecd (y)
                  else  
                     call nexsecd (y)
                  endif 
                  isect = isect + 1 
               endif
               ptr = brnch_nxt(ptr)
            else
               finished = .true.
            endif
         enddo
         if (isect .eq. 0) call erexit 
         call finsecd (y)   

C        If sections, update equivalent PI 

         if (isect .gt. 1) then
            qptr = brnch_ptr(oldptr)
            nbr = iabs(qptr)
            if (qptr .gt. 0) then
               do i = 1,2
                  do j = 1,2
                     k = 4*i + 2*j - 2
                     brnch(k,nbr) = sngl(dreal (y(i,j)))
                     brnch(k+1,nbr) = sngl(dimag (y(i,j)))
                  enddo
               enddo
            else
               do i = 2,1,-1
                  do j = 2,1,-1
                     k = 4*i + 2*j - 2
                     brnch(k,nbr) = sngl(dreal (y(i,j)))
                     brnch(k+1,nbr) = sngl(dimag (y(i,j)))
                  enddo
               enddo
            endif
         endif
         kt = inp2opt(k1)
         mt = inp2opt(k2)
         v(1) = dcmplx(e(kt),f(kt))
         v(2) = dcmplx(e(mt),f(mt))
 
         do i=1,2
            a(i) = oldy(i,1)*v(1) + oldy(i,2)*v(2)
            s(i) = v(i) * dconjg(a(i))
            pold(i) = dreal(s(i))
            qold(i) = dimag(s(i))
         enddo
 
         do i=1,2
            a(i) = y(i,1)*v(1) + y(i,2)*v(2)
            s(i) = v(i) * dconjg(a(i))
            pnew(i) = sngl(dreal(s(i)))
            qnew(i) = sngl(dimag(s(i)))
         enddo

         if (idebug .gt. 0) then
            write (dbug,200) bus(k1), base(k1), bus(k2), base(k2),
     1               angle, pold(1), qold(1), pnew(1), qnew(1)   
  200       format (' Phase shifter ',a8,f7.1,2x,a8,f7.1,' angle ',
     &         f7.1, ' original branch flow ',2f10.3,' new flow ',
     &         2f10.3)  
         endif
c
c        Determine if compensation is excessive and jeopardizes 
c        terminal voltages.
c
         dp = sqrt (pold(1) ** 2 + qold(1) ** 2) 
     &      - sqrt (pnew(1) ** 2 + qnew(1) ** 2)
         dv = abs(dp) / cdabs (oldy(1,2))
         if (dv .gt. 0.25) then
            kt = inp2opt(k1)
            if (ntypu(kt) .eq. 1 .or. ntypu(kt) .eq. 4 .or.
     &          ntypu(kt) .eq. 10) ntypu(kt) = 2
            mt = inp2opt(k2)
            if (ntypu(mt) .eq. 1 .or. ntypu(mt) .eq. 4 .or.
     &          ntypu(mt) .eq. 10) ntypu(mt) = 2
         endif

C        Update PNET, QNET by removing the phase shift correction   

         do ksw = 1, 2
            jsw = 3 - ksw
            kt = inp2opt(k1)
            mt = inp2opt(k2)
            dp = pnew(ksw) - pold(ksw)
            dq = qnew(ksw) - qold(ksw)  
            vk = e(kt)**2 + f(kt)**2  
            y(ksw,ksw) = y(ksw,ksw) + dcmplx(-dp/vk,dq/vk) 
            gkku(kt) = gkku(kt) - dreal (oldy(ksw,ksw)) 
     &                          + dreal (y(ksw,ksw))   
            bkku(kt) = bkku(kt) - dimag (oldy(ksw,ksw)) 
     &                          + dimag (y(ksw,ksw)) 
            kmluu = km(kt) - 1
            found = .false.
            l = 1
            do while (l .le. kmlen(kt) .and. .not. found)
               lt = ikmu(l+kmluu)                                   
               if (lt .eq. mt) then
                  found = .true.
                  gkmu(l+kmluu) = gkmu(l+kmluu) - dreal(oldy(ksw,jsw)) 
     &                                          + dreal(y(ksw,jsw))   
                  bkmu(l+kmluu) = bkmu(l+kmluu) - dimag(oldy(ksw,jsw)) 
     &                                          + dimag(y(ksw,jsw)) 
               else
                  l = l + 1
               endif
            enddo
            if (.not. found) call erexit   

C           Add new shunt to +A record   

            ydiff = y(ksw,ksw) - oldy(ksw,ksw)
            ncb = kbsdta(15,k1)
            finished = .false.
            do while (ncb .gt. 0 .and. .not. finished) 

C              Search for existing +A record 
c
               call getchr(1,kode,kbctbl(8,ncb)) 
               call getchr(2,kodeyr,kbctbl(9,ncb)) 
               call getchr(3,kodeow,kbctbl(10,ncb)) 
               if (kode .eq. 'A' .and. kodeyr .eq. '01' .and.
     &             kodeow .eq. '***') then
                  bctbl(4,ncb) = bctbl(4,ncb) 
     &                         + bmva * sngl(dreal(ydiff))
                  bctbl(5,ncb) = bctbl(5,ncb) 
     &                         + bmva * sngl(dimag(ydiff))
                  finished = .true.
               else
                  ncb = bctbl_nxt(ncb)
               endif
            enddo
            if (.not. finished) then
C
C              Not found, generate a new +A record 
C
               status = add_eqcbs (k1, 'A', '***', '01', ncb)
               bctbl(4,ncb) = bmva*sngl(dreal(ydiff))
               bctbl(5,ncb) = bmva*sngl(dimag(ydiff))
               bctbl(6,ncb) = 0.0
               bctbl(11,ncb) = 0.0   
               bctbl(12,ncb) = 0.0   
            endif
c
c           Transpose indices for second pass (processing transpose)
c
            jx = k1
            k1 = k2
            k2 = jx
         enddo

  240 continue  
  250 continue  
      return
      end   

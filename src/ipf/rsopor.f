C    @(#)rsopor.f	20.4 2/13/96
      subroutine rsopor
C
C     REstore OPtimal ORder to natural optimal order with relocation of
C     slack node.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		pnetu(r*8), qnetu(r*8), gkku(r*8), bkku(r*8), qloadu(r*8),
c		inetr(r*8), ineti(r*8), gkmu(r*8), bkmu(r*8), vlimn(r*4),
c		vlimx(r*4), ploadu(r*8), km, kmlen, ikmu
      include 'ipfinc/beta.inc'
c	Global variables used:
c		kt
      include 'ipfinc/blank.inc'
c	Global variables used:
c		ntot, nbslck
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8), capcor(r*8), 
c		inp2alf, opt2inp, inp2opt, alf2inp
      include 'ipfinc/qksrt.inc'
c	Global variables used:
c		None
      include 'ipfinc/red7.inc'
c	Global variables used:
c		None
      include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf

      common /is_batch / is_batch

      do i=1,ntot
         alf2inp(i)=opt2inp(i)  ! Note: alf2inp used as scratch array
      enddo
C
C     Reorder slack nodes. "KSLLOC" is natural optimal order.
C
      do jt = 1,nbslck
         it = 0
         do i = 1,nbslck   
           if (it .eq. 0) then   
              it = i  
           else  
              if (nslkxx(2,i) .gt. nslkxx(2,it)) it = i   
           endif 
         enddo
         kslack=nslkxx(1,it)   
         kslloc=nslkxx(2,it)   
         nslkxx(2,it) = -nslkxx(2,it)  
         do it = 1,kslloc  
            if (opt2inp(it) .eq. kslack) go to 150  
         enddo
         write (errbuf(1), 100) bus(kslack), base(kslack)
  100    format (' (RSOPOR) - Slack bus ', a8, f6.1, 
     &      ' could not be located in OPT2INP() array ')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         go to 900
  150    if (kslloc .gt. it) then
c
c           Reorder y-matrix arrays
c
            do i = it+1, kslloc
               inp2alf(i) = i - it  ! Note: alf2inp used as 
c                                   ! scratch array
            enddo
            inp2alf(it) = kslloc - it + 1
c
c           Reorder entities between (it:kslloc)
c
            ktemp = kslloc - it + 1
            call mvnew1  (opt2inp(it),  inp2alf(it), ktemp)
            call mvnew1  (km(it),       inp2alf(it), ktemp)
            call mvnew1  (kmlen(it),    inp2alf(it), ktemp)
            call mvnew1d (pnetu(it),    inp2alf(it), ktemp)
            call mvnew1d (qnetu(it),    inp2alf(it), ktemp)
            call mvnew1d (gkku(it),     inp2alf(it), ktemp)
            call mvnew1d (bkku(it),     inp2alf(it), ktemp)
            call mvnew1  (ntypu(it),    inp2alf(it), ktemp)
            call mvnew1  (nspar(it),    inp2alf(it), ktemp)
            call mvnew1  (vlimn(it),    inp2alf(it), ktemp)
            call mvnew1  (vlimx(it),    inp2alf(it), ktemp)
            call mvnew1d (ploadu(it),   inp2alf(it), ktemp)
            call mvnew1d (qloadu(it),   inp2alf(it), ktemp)
            call mvnew1d (inetr(it),    inp2alf(it), ktemp)
            call mvnew1d (ineti(it),    inp2alf(it), ktemp)
            call mvnew1d (e(it),        inp2alf(it), ktemp)
            call mvnew1d (f(it),        inp2alf(it), ktemp)
            call mvnew2d (capcor(1,it), inp2alf(it), ktemp)

         endif
      enddo

      do i=1, ntot   
         k=opt2inp(i)
         inp2opt(k)=i
      enddo
c
c     Reorder y-matrix
c
      do kt = 1, ntot  
         ksw = 0
         ls = km(kt) 
         lf = km(kt) - 1 + kmlen(kt)
         do l = ls, lf
            mt = ikmu(l)
            nt = alf2inp(mt)
            if (mt .ne. inp2opt(nt)) then  ! Is the optimal order
c                                          ! unchanged?
               ikmu(l) = inp2opt(nt)
               ksw = 1                     ! Yes!
            endif
         enddo                                        
         if (ksw .eq. 1) then              ! Resort y-matrix using
c                                          ! bubble sort
            do l = ls+1, lf
               k = l - 1
               do while (k .ge. ls .and. (ikmu(k+1) .lt. ikmu(k)))
                  itemp = ikmu(k+1)
                  ikmu(k+1) = ikmu(k)
                  ikmu(k) = itemp
                  temp = gkmu(k+1)
                  gkmu(k+1) = gkmu(k)
                  gkmu(k) = temp
                  temp = bkmu(k+1)
                  bkmu(k+1) = bkmu(k)
                  bkmu(k) = temp
                  k = k -1
               enddo
            enddo
         endif 
      enddo
  900 return
      end   

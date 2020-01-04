C    @(#)cutsys.f	20.4 11/12/98
      subroutine cutsys (nsyst, lunbus, lunbrn)

      include 'ipfinc/parametr.inc'
	
      include 'ipfinc/blank.inc'
      include 'ipfinc/cut.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/pqcurves.inc'
 
      character xbuf*120
      integer ptr 
c
c     *******************************************************
c
c     Set up pointers to P/Q curve data (buspqptr) and X data
C     (busxdtptr).
c
      if (.not. pq_flag) then
         do nb = 1, ntot
            buspqptr(nb) = 0
         enddo

         do i = 1, numcurv
            ieq = pqbusptr(i)
            if (ieq .gt. 0) buspqptr(ieq) = i
         enddo
         pq_flag = .true.
      endif

      if (.not. xdt_flag) then
         do nb = 1, ntot
            busxdtptr(nb)  = 0
         enddo
         do i = 1, kxtot
            kxd = xdata(1,i)
            if (kxd .gt. 0) busxdtptr(kxd) = i
         enddo
         xdt_flag = .true.
      endif
                                                                                
      call forbtm
      call fortop
      write(outbuf,100) nsyst
  100 format(' Data list for cut network No. ',i4)
      call prtout (1)

C     IKK assignments:  
C       
C     (1,*) --- 0/1 bus is not/is saved 
C     (2,*) --- index to CBCTBL 
C     (3,*) --- network number of saved bus 
C     (4,*) --- piback bus index in BUSPI ("0" designates   
C               a non-piback bus)   
C     (5,*) --- index to branch data in PIPARL  
C       
      do 210 nb = 1,ntot
         if (ikk(1,nb) .eq. 0 .or. ikk(5,nb) .ne. nsyst .or.
     1       ikk(4,nb) .gt. 0) go to 210
         call bcdbus (nb,xbuf)
         write(lunbus,120) xbuf
  120    format(a)
         call list_bus (nb)

         if (kbsdta(1,nb) .eq. 11) then
            kxd = busxdtptr(nb)
            if (kxd .gt. 0) then
               call bcdxdt (kxd, xbuf)
               write (lunbus,120) xbuf
               call list_xdt (kxd)
            else
               write (errbuf(1), 130) bus(nb), base(nb)
  130          format (' BX bus ', a8, f7.1, ' is missing X-record')
               call prterx ('W', 1)
            endif
         endif

         linepq = buspqptr(nb)
         if (linepq .gt. 0) then
            call bcdqpd (linepq, xbuf)
            write (lunbus,120) xbuf
            call list_pqd (linepq)
         endif

         ncb = kbsdta(15,nb)
         do while (ncb .gt. 0) 
            call bcdcbs (ncb,xbuf)
            write (lunbus,120) xbuf
            call list_cbs (ncb)
            ncb = bctbl_nxt(ncb)
         enddo
 
         ptr = kbsdta(16,nb)
         do while (ptr .gt. 0)
            mt = ky(ptr)
            if (ikk(1,mt) .eq. 1 .and. ikk(4,mt) .eq. 0 .and.
     &          brtype(ptr) .ne. 1) then
               call list_brn (ptr)
               if (brnch_ptr(ptr) .gt. 0) then
                  call bcdbrn (ptr,xbuf)
                  write (lunbrn, 120) xbuf
               endif
            endif
            ptr = brnch_nxt(ptr)
         enddo
  210 continue

      endfile lunbus
      endfile lunbrn
      return
      end

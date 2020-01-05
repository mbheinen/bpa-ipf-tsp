C    @(#)reduct.f	20.6 1/7/99
      subroutine reduct
 
C     Reduce a network into an equivalent system.
C     Output is in the form of BUSDTA and BRNCH arrays with deleted
C     bus and branch items.  Any bus SRTLST is deleted.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/red4.inc'
      include 'ipfinc/red5.inc'
      include 'ipfinc/red6.inc'
      include 'ipfinc/red7.inc'
      include 'ipfinc/reic.inc'
      include 'ipfinc/sortbs.inc'
C
      common /eqarea/ iarea,kadata(MAXCAR)
 
      common /scratch/ nbr, array(2,100),
     &                 ndel1, lindel1(MAXBRN), ndel2,
     &                 lindel2(MAXBRN2), ndel3, cbdel(MAXBUS),
     &                 br_status(MAXBRN)
c
      common /is_batch / is_batch
c
      double precision ai1(2,3)
c
      real basn(5), temp_pk, temp_vk, temp_qk
c
      integer ptr, oldptr, status, del_eqcbs, del_eqbrn, assequiv,
     &        ktemp(5), rei, bldequiv, rename_bus, array, 
     &        cbdel, br_status, shift
c
      character bsn(5)*8, com(5)*3, tempc(3)*10, bus_name*8
c
      ntotx = ntot
      ntotr = 0
      ltotr = 0

      if (ntot .eq. 0) then
         write (errbuf(1), 90)
   90    format ('No base data in residence')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         go to 9000 
      endif 

      tempc(1) = chase1(1)  ! STORE CASE NAME AND DESCRIPTION   
      tempc(2) = chase1(34) 
      tempc(3) = chase1(35) 

      lprtsw = 1  ! PRINT ON, FICHE ON IF FILE BEING DISPOSED TO FICHE. 
      fichsw = kspare(3)
      call forbtm   
      write (outbuf,100)
  100 format('S Y S T E M     R E D U C T I O N' )  
      call rpnlod
      outbuf=' '
      do i = 1, 5
         call shdlod(i)
      enddo

      call fortop
c
c     Update pnetu(), qnetu() for slack nodes      
c
      do i = 1, nbslck 
         nb = nslkxx(1,i) 
         kt = inp2opt(nb)
c
c	nrpqv is expecting single precision arguments.
c
	 temp_pk = pk
         temp_qk = qk
         temp_vk = vk
         call nrpqv (kt, temp_pk, dp, temp_qk, dq, temp_vk)
         pk = temp_pk
         qk = temp_qk
         vk = temp_vk
         pnetu(kt) = pk   
         qnetu(kt) = qk   
      enddo
C
C     Restore optimal order to natural optimal order with relocation of
C     slack node.
C
      call rsopor
C
C     Define internal bus arrays
C
      do i = 1,ntot
         kt = inp2opt(i)
         intbus(kt) = bus(i)
         intbas(kt) = base(i)
      enddo
C
C     Process / REDUCTION records to define reduced system.
C
      call prcred (kerrsw)
      if (kerrsw .ne. 0) go to 9000
      idebug = kase1(33)
      if (idebug .gt. 0) call dbgprt (1)
C       
      call redttl  ! Print out reduction header 

      read (chase1, 750) tol 
  750 format (f10.5)
      if (tol .eq. 0.0) then  
         if (chase1(1) .eq. ' ') tol=0.02   
      endif
C
c     Force retention of all system slack buses, update angles.
c
      do i = 1, nbslck  
         k = nslkxx(1,i)
         kt = inp2opt(k)
         angle = datan2 (f(kt),e(kt)) 
         busdta(12,k) = angle
         ikk(1,kt) = 1
      enddo
C
C     If "KASE1(11)" is 1, retain all generators above "PGMAX" mw
c
      if (kase1(11) .gt. 0) then
         read (chase1(35),842) pgmax
  842    format (f10.0) 
         pgmax = pgmax / bmva   
         do kt = 1,ntot 
            if (kmlen(kt) .gt. 0)   then              
               if (pnetu(kt)+ploadu(kt) .ge. pgmax) ikk(1,kt)=1   
            endif   
         enddo
      endif 

C     If "KASE1(20)" is 1, retain all area tie line nodes and all 
c     area slack nodes

      if (kase1(20) .gt. 0 .and. jtie .gt. 0) then  
         do i = 1, ntotc   
            kt=karea(1,i)  
            kt=inp2opt(kt)   
            ikk(1,kt)=1
         enddo
         do i = 1, jtie
            k1=tie(1,i)
            k2=tie(7,i)
            k1=inp2opt(k1)
            k2=inp2opt(k2)
            ikk(1,k1)=1
            ikk(1,k2)=1
         enddo
      endif
C
C     1. Determine status of any passive d-c nodes.
C     2. Assure that all retained converters have unconditionally
C        retained commutator buses.
C
      do i = 1,mtdcbs
         k = dcmtbs(1,i)
C
C        Examine converters status.  if retained, unconditionally retain
C        all other d-c nodes in same circuit.
C
         kt=inp2opt(k)
         i1=ikk(1,kt)
         if (i1 .eq. 1) then
            jckt = dcmtbs(21,i)
            do j = 1,mtdcbs
               n = dcmtbs(1,j)
               if (dcmtbs(21,j) .eq. jckt) then
                  if (i .ne. j) then
                     nt=inp2opt(n)
                     ikk(1,nt)=ikk(1,kt)
                  endif
               endif
            enddo
            m = dcmtbs(3,i)
C
C           Retain commutator bus if converter bus is also retained.

            if (m .ne. 0) then
               mt=inp2opt(m)
               ikk(1,mt)=ikk(1,kt)
            endif
         endif
      enddo
C
C     If either converter of a two-terminal d-c system is retained,
C     unconditionally retain the other converter.
C
      do i = 1,kdtot
         k1 = dc2t(1,i)
         k2 = dc2t(3,i)
         kt=inp2opt(k1)
         mt=inp2opt(k2)
         if (ikk(1,kt) .gt. 0 .or. ikk(1,mt) .gt. 0) then
            ikk(1,kt)=1
            ikk(1,mt)=1
            k1 = dc2t(33,i)
            k2 = dc2t(34,i)
            kt=inp2opt(k1)
            mt=inp2opt(k2)
            ikk(1,kt)=1
            ikk(1,mt)=1
         endif
      enddo
C
C     "OPTSIZ" performs optimal expansion of retained network.
C
      if (kase1(24) .ne. 0) call optsiz
C
C     Print out cluster populations.
C       
      call clustr   
C       
C     Process appended lossless susystems   
C       
      if (iarea .gt. 0) call arezln (iarea,kadata)  
C       
C     Process REI subsystem.  Note that certain entities in the 
c     y-matrix become linked.
C       
      call sortbus
      ntoto = ntot
      status = rei (ntoto)
      if (status .gt. 0) go to 9000

      if (ntotx .gt. ntot) then
         ntot = ntotx
         call sortbus
         ntot = ntoto
      endif

      call forbtm   
      call fortop   
      write (outbuf,960)
  960 format ('0 INPUT LISTING OF RETAINED NODES -- " " PERTAINS TO ',
     1        'INTERNAL RETAINED NODE ' )
      call prtout(1)
      write (outbuf,970)
  970 format(  37x,'"E" PERTAINS TO ENVELOPE NODE ' )   
      call prtout(1)
      write (outbuf,980)
  980 format(37x,'"X" PERTAINS TO RETAINED NODE WITH ALL IMMEDIATE ',
     1           'ADJACENT NODES ELIMINATED')
      call prtout(1)
      write (outbuf,990)
  990 format(37x,'"*"PERTAINS TO ADDITIONAL NODES OPTIMALLY SELECTED')  
      call prtout(1)
      outbuf = '0'  
      call prtout(1)
c
c     NSW = 1 : First pass, print out all retained buses
c           0 : Second pass, print out all eliminated buses
c
      do nsw = 1, 0, -1
         knt=0 
         knt1=0
         do nb = 1,ntotx
            k = alf2inp(nb) 
            kt = inp2opt(k)   
            i1 = ikk(1,kt)  
            if (i1 .eq. nsw) then
               knt1=mod(knt,5)+1 
               knt=knt+1 
               bsn(knt1)=bus(k)  
               basn(knt1)=base(k)
               ktemp(knt1) = ikk(3,kt)   
               com(knt1)='   '   
               if (ikk(4,kt) .eq. 1) com(knt1) = '*  '   
               if (nsw .gt. 0 .and. kmlen(kt) .gt. 0) then
c
c                 Determine if retained bus is a border node
c
                  ic = 0                                        
                  ikmuu = km(kt) - 1
                  do l = 1, kmlen(kt)
                     mt = shift (shift (ikmu(l+ikmuu), 16), -16)
                     if (ikk(1,mt) .eq. 0) ic = ic + 1         
                  end do                                        
                  ls = ikmuu + kmlen(kt)
                  if (shift (ikmu(ls), -16) .gt. 0) then
                     l = shift (shift (ikmu(ls), 16), -16)
                     mt = shift (shift (ikmu(l), 16), -16)
                     if (ikk(1,mt) .eq. 0) ic = ic + 1         
                  endif
                  if (ic .gt. 0) then
                     ikk(2,kt)=1   
                     if (ic .eq. kmlen(kt)) then
                        com(knt1)(2:3)='X '
                     else
                        com(knt1)(2:3)='E '   
                     endif
                  endif
               endif
            endif
            if (nsw .eq. 1 .and. knt1 .eq. 5) then
               write (outbuf,1060) (com(i),ktemp(i),bsn(i),basn(i),
     &                              i=1,knt1) 
 1060          format(5(1x,a3,i3,1x,a8,f7.1,3x)) 
               call prtout(1)
               knt=0 
               knt1=0
            endif
         enddo

         if (nsw .eq. 1) then
            if (knt1 .lt. 5) then
               write (outbuf,1060) (com(i),ktemp(i),bsn(i),basn(i),
     &                           i=1,knt1) 
               call prtout(1)
            endif
         endif
      enddo
C
C     Restore all deleted phase shifters to zero degrees
C       
      if (jphno .gt. 0) then
         do jt=1,jphno
            j1=jphid(1,jt)
            j2=jphid(2,jt)
            kt=inp2opt(j1)  
            mt=inp2opt(j2)  
            if(ikk(1,kt) .eq. 0) jphid(1,jt) = -j1
            if(ikk(1,mt) .eq. 0) jphid(2,jt) = -j2
         enddo
         call sympha(idebug)   
      endif
C       
C     Debug check of validity of Y-matrix data   
C       
      if (idebug .gt. 0) then   
         do i = 1, 3
            ai1(1,i) = 0.0
            ai1(2,i) = 0.0
         enddo
         do kt = 1,ntotx   
            if (kmlen(kt) .gt. 0 .and. km(kt) .gt. 0) then             
               kmluu = km(kt) - 1
               js = kmlen(kt)
               do i = 1, js 
                  yred(1,i) = shift (shift (ikmu(i+kmluu), 16), -16)
                  yred(2,i) = gkmu(i+kmluu)
                  yred(3,i) = bkmu(i+kmluu)
               enddo
               ix = yred(1,js)                                 
               ls = shift (ix, -16)                            
               if (ls .gt. 0) then
                  js = js + 1
                  yred(1,js) = ikmu(ls)
                  yred(2,js) = gkmu(ls)
                  yred(3,js) = bkmu(ls)
               endif
               js = js + 1
               yred(1,js) = kt
               yred(2,js) = gkku(kt)
               yred(3,js) = bkku(kt)
               call pkqk1 (kt, js, yred, ai1) 
            endif  
         enddo
      endif 

      if (idebug .gt. 1) then
         write (dbug,1160) 
 1160    format ('1 DEBUG DUMP OF ARRAYS'//'    BUS       BASE        ',
     1        'inp2opt       opt2inp          IKK '/)
         do k = 1, ntotx
            kt = inp2opt(k)   
            j = opt2inp(k)
            write(dbug,1170) k,bus(k),base(k),kt,j,(ikk(i,kt),i=1,5)  
 1170       format(1x,i6,2x,a8,f6.1,i10,i6,6x,5i4)
         enddo
      endif
c
c     Perform y-matrix reduction 
c
      status = bldequiv ()
      if (status .gt. 0) go to 9000
c
C     Store index of all deleted branches.  These positions will
C     be used for equivalent branches.  This is done in two passes. 
c
C     1. Store all branch indices whose bus 1 is deleted.   
C     2. Store deleted branch indices whose bus 1 is retained bus whose 
C        bus 2 is deleted.  
C       
      ndel1 = 0  
      ndel2 = 0  
      ndel3 = 0  
      do i = 1, ltot
        br_status(i) = 0            
      enddo
      do nb = 1,ntot
         kt = inp2opt(nb)
         if (ikk(1,kt) .eq. 0) then
c
c           Delete all continuation buses and reserve their space
c
            ptr = kbsdta(15,nb)
            oldptr = 0
            do while (ptr .gt. 0)
               status = del_eqcbs (ptr, oldptr)
               oldptr = ptr
               ptr = bctbl_nxt(ptr)
            enddo
c
c           Delete all branches and reserve their space
c
            ptr = kbsdta(16,nb)
            oldptr = 0
            do while (ptr .gt. 0)
               status = del_eqbrn (ptr, oldptr)
               oldptr = ptr
               ptr = brnch_nxt(ptr)
            enddo
         endif  
      enddo
C
C     Assemble equivalent reduced network from reduced y_matrix data
C
      status = assequiv (ntoto, ntotr, ltotr)
      if (status .gt. 0) go to 9000

      do kt = 1,ntotx
         if (ikk(1,kt) .ne. 1) then
            nb = opt2inp(kt) 
            bus_name = srtlst   
            bus_base = base(nb)
            status = rename_bus (nb, bus_name, bus_base)
         endif 
      enddo
c
c     Set flag to rebuild base case
c
      kspare(1) = 1

      ltotr=ltotr/2 
      write (outbuf,2400) ntotr,ltotr   
 2400 format ('0 REDUCED SYSTEM --- ',i4,' BUSSES ',i5,' BRANCHES ' )   
      call prtout(1)
      write (outbuf,2410) yptr
 2410 format('0   LENGTH OF Y-MATRIX ',i5)   
      call prtout(1)

      ntot = ntotx
      read (buf,2420) card  
 2420 format (a1)   

      call dbgprt (0)   
C       
C     Restore case name and description 
C       
      chase1(1) = tempc(1)  
      chase1(34) = tempc(2) 
      chase1(35) = tempc(3) 
C       
C     Eliminate area interchange system if option selected  
C       
      if (chase1(20) .eq. ' ' .or. chase1(20) .eq. '0') then
         ntotc = 0   
         ntotic = 0  
         jtie = 0
      endif 
      go to 9020

 9000 write (errbuf(1), 9010)
 9010 format ('0 Program aborted by reduction errors .')
      if (is_batch .eq. 0) then
         call prterx ('E',1)
      else
         call prterx ('F',1)
      endif
      write (*, 9010)
      call erexit   

 9020 return
      end   

C    @(#)prcred.f	20.4 2/13/96
      subroutine prcred (kerrsw) 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/red4.inc'
      include 'ipfinc/red5.inc'
      include 'ipfinc/red6.inc'
 
      common /eqarea/ iarea,kadata(MAXCAR)

      common /is_batch / is_batch

      character blnk*1
      integer find_bus
      external find_bus
C
C     IKK ASSIGNMENT
C
C     IKK(1,*)  0   NODE IS ELIMINATED
C               1   NODE IS RETAINED
C
C     IKK(2,*)  0   NODE IS AN INTERNAL NODE
C               1   RETAINED NODE IS A BOUNDARY NODE
C
C     IKK(3,*)  N   REI SUBSYSTEM
C                   (0 IF NO REI SUBSYSTEM)
C
C     IKK(4,*)  0   NODE IS EXPLICITLY RETAINED
C               1   NODE IS OPTIMALLY RETAINED
C
C     IKK(5,*)  0   NORMAL
C               1   NODE IS MODIFIED BY REI MODELING
C
      blnk = ' '
      do kt=1,ntot
         do i=1,5
            ikk(i,kt)=0
         enddo
         ikkind(1,kt)=1
         ikkind(2,kt)=1
      enddo
C
C     Initialize reduction options
C
      do i=1,40
         chase1(i)=' '
         kase1(i)=0
      enddo
C
C     Process / REDUCTION records
C
      kerrsw=0
      inptls=0
      inptsw=0
      iarea = 0
      call read_red
      if (inptsw .eq. 0) go to 490
      go to (250,450,490,490,490,234) inptsw
C
C     Process > SAVE_AREAS ...<
C
  234 write (outbuf,236)
  236 format('0 EQUIVALENT NETWORK WILL RETAIN THE FOLLOWING AREAS.')
      call prtout(1)
      call space (1)
C
C     This option unconditionally retains all area interchange
C     slack nodes and tie lines.
C
      do i = 1,ntotc
         kt=karea(1,i)
         kt=inp2opt(kt)
         ikk(1,kt)=1
      enddo
      do i = 1,jtie
         k1=tie(1,i)
         k2=tie(7,i)
         k1=inp2opt(k1)
         k2=inp2opt(k2)
         ikk(1,k1)=1
         ikk(1,k2)=1
      enddo
C
C     Set up dummy area in case all areas are made into equivalents.
C
      iarea = iarea + 1
      kadata(iarea) = 0
      do 247 l = 1,idat
      write (outbuf,243) adata(l)
  243 format (6x,a10)
      call prtout (1)
      do k = 1,ntotc
         if (arcnam(k) .eq. adata(l)) then
            iarea = iarea + 1
            kadata(iarea) = k
            idata(1,l)=1
            do i = 1,ntot
               if (jarzn(i) .eq. k) then
                  kt = inp2opt(i)
                  ikk(1,kt) = 1
               endif
            enddo
            go to 247
         endif
      enddo
  247 continue
      do l=1,idat
         if (idata(1,l) .eq. 0) then
            write (errbuf(1),248) adata(l)
  248       format (' Saved area ',a10,' is not in system.')
            call prterx ('W',1)
         endif
      enddo
      go to 470
C
C     Process > SAVE ZONES...<
C
  250 write (outbuf,260)
  260 format('0 STUDY AREA DEFINED BY THE FOLLOWING SAVED ZONES..')
      call prtout(1)
      write (outbuf,270) blnk
  270 format(1h0,20x,20(4h--  ),a1)
      call prtout(1)
      kend = idat
      do kst=1,idat,20
         kend=min0 (kst+19,idat)
         write (outbuf,280) (zdata(i),i=kst,kend)
  280    format(1h0,20x,20(a2,2x))
         call prtout(1)
      enddo
      write (outbuf,270) ' '
      call prtout(1)
      if (idat2 .eq. 0) go to 350
C
C     Process > SAVE ZONES....SAVE BASES <
C
      write (outbuf,300)
  300 format('0 Study area defined by the following saved base KV''s.')
      call prtout(1)
      write (outbuf,310) blnk
  310 format(1h0,37x,10(7h  -----),a1)
      call prtout(1)
      do kst=1,idat2,10
         kend=min0 (kst+9,idat2)
         write (outbuf,320) (basedt(i),i=kst,kend)
  320    format(1h0,37x,10f7.1)
         call prtout(1)
      enddo
  340 write (outbuf,310) blnk
      call prtout(1)
  350 continue
      do k=1,ntot
         l = 0
         kt = inp2opt(k)
         if (idat .gt. 0) then
            do l=1,idat
               if (zone(k) .eq. zdata(l)) then
                  idata(1,l)=1
                  go to 370
               endif
            enddo
            l = -1
  370       continue
         endif
         i = 0
         if (idat2 .gt. 0) then
            do i=1,idat2
               if (base(k) .eq. basedt(i)) then
                  idata(2,i)=1
                  go to 390
               endif
            enddo
            i = -1
  390       continue
         endif
         if (min0 (i,l) .ne. -1) ikk(1,kt)=1
      enddo
      if (idat .gt. 0) then
         do l=1,idat
            if (idata(1,l) .eq. 0) then
               write (errbuf(1),410) zdata(l)
  410          format (' Missing zone ',a2,' is ignored. ')
               call prterx ('W',1)
            endif
         enddo
      endif
      if (idat2 .gt. 0) then
         do i=1,idat2
            if (idata(2,i) .eq. 0) then
               write (errbuf(1),430) basedt(i)
  430          format (' Missing base kv ',f6.1,' is ignored. ')
               call prterx ('W',1)
            endif
         enddo
      endif
      go to 470
C
C     Process > SAVE BUSES <
C
  450 n=1
      call forbtm
      write (outbuf,460)
  460 format ('0 INPUT LISTING OF SAVED BUSSES '  )
      call fortop
      call prtout(1)
      call space(2)
      go to 560

  470 if (index('(/',card) .ne. 0) go to 600
  480 call read_red
      if (inptsw .eq. 0) go to 600
      go to (250,490,520,540,600) inptsw

  490 write (errbuf(1),500)
  500 format (' CONTROL CARD OUT OF SEQUENCE ' )
      write (errbuf(2),510) buf(1:80)
  510 format(19x,'(',a80,')' )
      if (is_batch .eq. 0) then
         call prterx ('E',2)
      else
         call prterx ('F',2)
      endif
      kerrsw=1
      go to 470
C
C     Process > INCLUDE BUSES <
C
  520 n=1
      write (outbuf,530)
      call prtout(1)
  530 format ('0           INPUT LISTING OF INCLUDED BUSSES ' )
      go to 560
C
C     Process > EXCLUDE BUSES <
C
  540 n=0
      write (outbuf,550)
      call prtout(1)
  550 format ('0           INPUT LISTING OF EXCLUDED BUSSES ' )
  560 do i=1,idat
         write (outbuf,570)busdt(i),basedt(i)
  570    format(7x,a8,f6.1)
         call prtout(1)
         nb = find_bus(busdt(i),basedt(i))
         if (nb .le. 0) then
            write (errbuf(1),580) busdt(i),basedt(i)
  580       format (' Missing bus ',a8,f7.1,' is ignored. ')
            call prterx ('W',1)
            kerrsw=1
         else
            kt=inp2opt(nb)
            ikk(1,kt)=n
         endif
      enddo
      go to 480

  600 continue
      return
      end

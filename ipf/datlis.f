C    @(#)datlis.f	20.7 11/12/98
      subroutine datlis
 
C     Formated input data listing of the powerflow data
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arsort.inc'
      include 'ipfinc/asort.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/zonlst.inc'
      include 'ipfinc/bstype.inc'
      include 'ipfinc/brtype.inc'
      include 'ipfinc/ownhash.inc'

      character zn*2, znl*2, kown*3, kownl*3

      integer fichsx, fichpv, chkerr, p
c
c     *******************************************************
c     set up pointers to any P/Q curve data
c
c     Store X-data pointer in busxdtptr() and P/Q curve
c     data pointer in buspqptr()
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
c
c     save old print switch values....
 
      lprtpv = lprtsw
      fichpv = fichsw
      fichpv = fichsw
 
      call forbtm     ! Clear last print page and print its footer
C
C               Set up new HEADING
 
      write (outbuf,130)  kbsknt,kbrknt
  130 format ('* * * INPUT LISTING * * *',i7,' BUSSES',
     1         i7,' EQUIVALENT BRANCHES')
      call rpnlod
      call fortop
      call space(2)
C
C     SUPRESS INPUT LISTING IF FATAL ERRORS DETECTED AND P_INPUT
C     OR F_INPUT HAS NOT ENABLED OPTION "ERRORS=LIST"
C
      if (chkerr('F') .ne. 0) then
         if (mod(kspare(3),10) .eq. 0) kspare(4) = 0
         if (kspare(3)/10 .eq. 0) kspare(5) = 0
      endif
        lprtsw = min0 (kspare(4),1)
        fichsw = min0 (kspare(5),1)
        lprtsx=lprtsw
        fichsx=fichsw
        lprtsw = 0
        fichsw = 0
        if ( lprtsx .eq. 0 ) lprtsw = 1
        if ( fichsx .eq. 0 .and. kspare(16).ge.0 ) fichsw = 1
 
        outbuf = '0  Input data Listing not requested.'
        call prtout(1)
 
        lprtsw = lprtsx
        fichsw = fichsx
 
C     PRINT COMMENT CARDS.
 
        if (ncom.gt.0) then
           write (outbuf,90)
   90      format(50x,'CASE COMMENTS')
           call prtout(1)
 
           write (outbuf,92)
   92      format(t21,84('*'))
           call prtout(1)
 
           write (outbuf,94)
   94      format(t21,'*',t104,'*')
           call prtout(1)
 
           do 100 i=1,ncom
              write (outbuf,96) com(i)(1:80)
   96         format(t21,'* ',a,t104,'*')
              call prtout(1)
  100      continue
 
           write (outbuf,94)
           call prtout(1)
           write (outbuf,92)
           call prtout(1)
        endif
 
        iframe = 0
        if( fichsw.gt.0 ) then
          write (outbuf,120)
  120     format( '$MFCL     INPUT ' )
          call pfomf(2)
        endif
 
      karsw = 2
      if (iopton(17) .ne. 0) then
 
         if (ntotc .eq. 0) then
            write (errbuf(1),170)
  170       format ('0 NO AREA INTERCHANGE SYSTEM EXISTS ',
     &              'FOR REQUESTED INTERCHANGE CONTROL.')
            call prterx ('I',1)
            kabort=1
         else
 
C        CONTROL " AC " CARDS
 
            karsw = 1
 
            if (iopton(17).eq.1) then
               write (outbuf,190)
  190          format('0',28x,'AREA INTERCHANGE CONTROL' )
               call prtout(1)
 
            else
               write (outbuf,192)
  192          format('0',28x,'AREA INTERCHANGE MONITOR' )
               call prtout (1)
            endif
 
            call space(1)
            write (outbuf,200)
  200       format(t10,'AREA 1',t22,'AREA 2',t34,'SCHEDULED',t46,
     1       '   NET   ',t58,'  CONTROL BUS',t78,'ZONES(S)')
            call prtout(1)
 
            write (outbuf,202)
  202       format (t35,'EXPORT ',t46,' EXPORT ',t58,'NAME       BASE')
            call prtout(1)
 
            write (outbuf,204)
  204       format(t34,'  (MW)  ',t46,'  (MW)  ',t78,10('-- '))
            call prtout(1)

            do i=1,ntotc
               call space(1)
               call list_are (i)
            enddo
 
            outbuf= '0'
            call prtout(1)
         endif
      endif
 
      lprtsw = lprtsx
c***      if( kspare(11)-1 ) 450,290,370
      if ( kspare(11) .eq. 0 ) goto 450
      if ( kspare(11) .eq. 1 ) goto 290
      if ( kspare(11) .eq. 2 ) goto 370
      if ( kspare(11) .ne. 3 ) goto 450
 
  250 if (numown .eq. 0) then
         kspare(11) = 0
         go to 450
      endif
 
C     LIST SORT BY OWNER-NAME
 
      call space (1)
      write (outbuf,261)
  261 format(t21,'INPUT LISTING SORTED BY THE FOLLOWING OWNERS ')
      call prtout(1)
 
      call space (1)
      write (outbuf,263) ' '
  263 format(t21,16('---  '), a1)
      call prtout(1)
 
      kend = numown
      kst = 1
  270 if( kend .gt. (kst+15) ) kend = kst + 15
      write (outbuf,271)(owner_o(alf2own(i)),i=kst,kend)
  271 format(1h0,20x,16(a3,2x) )
      call prtout(1)
      if ( kend .lt. numown ) then
         kst = kend + 1
         kend = numown
         go to 270
      endif
      write (outbuf,263) ' '
      call prtout(1)
      go to 450
C
  290 if (nztot .eq. 0) then
         kspare(11) = 0
         go to 450
      endif
 
C     LIST SORT BY ZONE-NAME
 
      call space (1)
      write (outbuf,330)
  330 format(t21,'INPUT LISTING SORTED BY THE FOLLOWING ZONES ')
      call prtout(1)
 
      call space (1)
      write (outbuf,340) ' '
  340 format(t21,20('--  '), a1)
      call prtout(1)
 
      kend = nztot
      kst = 1
  320 if( kend .gt. (kst+19) ) kend = kst + 19
      write (outbuf,350)(acznam(i),i=kst,kend)
  350 format(1h0,20x,20(a2,2x) )
      call prtout(1)
      if ( kend .lt. nztot ) then
         kst = kend + 1
         kend = nztot
         go to 320
      endif
      write (outbuf,340) ' '
      call prtout(1)
      go to 450
C

  370 if (natot.eq.0) go to 450
      call forbtm
      call fortop
C
C      LIST SORT BY AREA-NAME
C
      call space (1)
      write (outbuf,380)
  380 format(t21,'INPUT LISTING SORTED BY AREAS ' )
      call prtout(1)
      call space (1)
      write (outbuf,390)
  390 format(t21,'AREA        ZONE COMPOSITION  ')
      call prtout(1)
      call space (1)
      write (outbuf,400) ' '
  400 format(t21,'----------',2x,22('--  '),a1)
      call prtout(1)
      do k=1,natot
         write (outbuf,410) arsnam(k),(arsens(j,k),j=1,22)
  410    format(t21,a10,2x,22(a2,2x)  )
         call prtout(1)
      enddo
C
      call space(2)
  450 continue
      if (npzdta .gt.0) then
         lprtsw = 1
         fichsw = 0
         call space (1)
         write (outbuf,460)
  460    format(t27,'Selected data listing from the following zones:')
         call prtout(1)
         call space (1)
C
         do 470 kst=1,npzdta ,20
            kend = min0(kst+19,npzdta )
            write (outbuf,350) (pzdlst (k),k=kst,kend)
            call prtout(1)
  470    continue
         call space(1)
      endif
C
      if (nfzdta .gt.0) then
         lprtsw = 0
         if(kspare(16).ge.0) fichsw = 1
         write (outbuf,460)
         call prtout(1)
         call space(1)
C
         do kst=1,nfzdta ,20
            kend = min0(kst+19,nfzdta )
            write (outbuf,350) (fzdlst (k),k=kst,kend)
            call prtout(1)
         enddo
C
         call space(1)
      endif
C
      lprtsw = lprtsx
      fichsw = fichsx
 
c
c     Check and initialize sort options wanted...
c
c           kspare(11) is the SORT order switch:
c                      = 0 for Busname,Kv (default)
c                      = 1 for Zone,Busname,Kv
c                      = 2 for Area,Busname,Kv
c                      = 3 for Owner,Busname,Kv
c
      kzsrt = kspare(11) + 1
c
      if ( kzsrt .gt. 1 ) then
         znl = '**'
         kownl = '***'
         if ( natot .gt. 0 ) then
            iral = 0
         else if ( kzsrt .lt. 4 ) then
            kzsrt = min0(kzsrt,2)
         endif
      endif

c     *******************************************************
c     begin bus print loop                                  *
c                                                           v
      do knt = 1,ntot_alf
         nb = sorto(knt)
         kown = owner(nb)
         zn = zone(nb)
         lprtsw = lprtsx
         fichsw = fichsx

         if ( npzdta .eq.0 ) go to 630

c        ZL cards, check zones

         lprtsw = 0
         do 620 k=1,npzdta
            if ( zn.eq.pzdlst (k) ) then
               lprtsw = 1
               goto 630
            endif
  620    continue
c
  630    if ( nfzdta  .eq. 0 ) go to 650
c                                       zf cards, check zones
         fichsw = 0
         do 640 k=1,nfzdta
            if ( zn.eq.fzdlst (k) ) then
               if(kspare(16).ge.0) fichsw = 1
               goto 650
            endif
  640    continue
  650    if ( (lprtsw+fichsw) .eq. 0 ) go to 1220

         if (kzsrt.eq.3) then
c                             Area sort order
            ira=barea(knt)
            if (ira.ne.iral) then
c                             start a new area
               iral = ira
               write (outbuf,548)  arsnam(ira)
  548          format ('* * * INPUT LISTING * * * AREA ',a,
     1                 ' SYSTEM DATA')
               call rpnlod
               call space (1)
               write (outbuf,550) arsnam(ira)
  550          format(t11,'AREA ',a10,' SYSTEM DATA '   )
               call prtout(1)
               call space(1)
            endif
         endif
 
c***         if (kzsrt.eq.2.and.zn.ne.znl.and.znl.ne.'**') then
         if ( kzsrt .eq. 2  .and.  zn .ne. znl ) then
c                                       print this zone
            call forbtm
            write (outbuf,722)  zn
  722       format ('* * * INPUT LISTING * * * ZONE ',a,
     1              ' SYSTEM DATA')
            call rpnlod
            call fortop
         endif
         znl = zn

         if ( kzsrt .eq. 4  .and.  kown .ne. kownl ) then
c                                       print this owner
            call forbtm
            write (outbuf,732)  kown
  732       format ('* * * INPUT LISTING * * * OWNER ',a,
     1              ' SYSTEM DATA')
            call rpnlod
            call fortop
         endif
         kownl = kown

         ktype = kbsdta(1,nb)
         call list_bus(nb)

c        do the customer bus records for this bus...

         ncb = kbsdta(15,nb) 
         do while (ncb .gt. 0)
            call list_cbs(ncb)
            ncb = bctbl_nxt(ncb) 
         enddo
 
c        print the type x switched reactance data

         linex = busxdtptr(nb)
         if (linex .gt. 0) then
            call list_xdt (linex)
         endif

         if (ktype.eq.BSTYP_BE .or. ktype.eq.BSTYP_BG .or.
     1       ktype.eq.BSTYP_BQ .or. ktype.eq.BSTYP_BX .or.
     2       ktype.eq.BSTYP_BS ) then

            linepq = buspqptr(nb)
            if (linepq .gt. 0 ) then
               call list_pqd (linepq)
            endif
         endif
c
c     *******************************************************
c       bus data printed, begin printing connected branches *
c       ( Transpose data as needed when q is negitive )     *
c                                                           v
         p=kbsdta(16,nb)    ! pointer to 1st Branch connected to bus nb
         do while (p .gt. 0)
            call list_brn(p)
            p = brnch_nxt(p)
         enddo    ! end of branch print loop

 1220 continue

c     END of bus loop
      enddo    ! end of bus print loop

      lprtsw=lprtpv
      fichsw=fichpv
 
      return
      end

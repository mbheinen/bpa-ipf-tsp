C    @(#)sinput.f	20.12 9/10/96
      subroutine sinput
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/bxlock.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com005.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/svc.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/ordsta.inc'
 
      common /is_batch / is_batch

      parameter (MXQPEN = 0)
 
      character word(100)*30, capital*30, type*1, bus1*8, fmt*10,
     1          cbown*3, cbtyp*1, cbkyr*2, bigbuf * 512, ljstfy * 512
      logical found, endrcd
      real binit(MAXXDT), vref(MAXXDT)  

      integer find_bus, error, findstr

      external find_bus   

C     Set up pointers to data (busxdtptr) and TBX data (ptrtbx)

      if (.not. xdt_flag) then
        do nb = 1, ntot
          busxdtptr(nb) = 0
        enddo
        do i = 1, kxtot
          kxd = xdata(1, i)
          if (ordtbx .eq. 2) kxd = opt2inp(kxd)
          if (kxd .gt. 0) busxdtptr(kxd) = i
        enddo
        xdt_flag = .true.
      endif

      do nb = 1, ntot
        ptrtbx(nb) = 0
      enddo

      do i = 1, ntotb
        nb = tbx(2, i)
        if (nb .gt. 0) then
           if (ordtbx .eq. 2) nb = opt2inp(nb)
           ptrtbx(nb) = i
        endif
      enddo

      tbx_loaded = ordtbx

C       INPTLS:  0 -  BUF processed. Read next card 
C                1 -  BUF not processed. Do not read next card  

        idat = 0
        idat2 = 0   
        islnsw = 0  
        iopton(20) = 0

C       PROCESS INPUT TEXT  

        if (init.eq.0) then 
           init=1   
           inpold=inp   
           kerrsw=0 
           call space (2)   
           write (outbuf,100) buf(1:80) 
  100      format (' SOLUTION TEXT (',a,')')
           call prtout(1)   
        endif   
        go to 740   
  110   card = buf(1:1) 
        assign 740 to inext 

C       COMMENT CARD 

        if (card.eq.'.') then   
           inptls=0 
           go to 740

C       HEADER CARD  

        else if (card.eq.'H') then  
           outbuf = buf(2: )
           call comlod(2)   
           inptls=0 
           go to 740

C      COMMENT CARD

        else if (card.eq.'C') then
           if(ncom.gt.MAXCMT) then
              write (errbuf(1),120) MAXCMT
  120         format(' TOO MANY COMMENTS ..MAX=',i5,
     &               '. FOLLOWING RECORD SKIPPED')
              write (errbuf(2),122) buf(1:80)
  122         format(' (',a,')')
              call prterx ('W',2)   
           else 
              ncom = ncom + 1   
              com(ncom) = buf(2:80) 
           endif
           inptls=0 
           go to 740
C       
C          PROGRAM CONTROL CARD 
C       
        else if (card .eq. '/') then
           if (findstr (buf,'SOLUTION') .ne. 0) then  
              inptls=0  
              go to 740 
           else if (findstr (buf,'CHANGE_BUS') .ne. 0 .or.
     1              findstr (buf,'CHANGEBUS') .ne. 0) then
              inptls=1  
              call hotchg   
              go to 740 
           else 
              go to 760 
           endif
C       
C          Check for meaningless control record 
C       
        else if (card .eq. '(') then
           inptls = 0   
           go to 760
        else if (card .ne. '>') then
           write (errbuf(1),130) buf(1:80)  
  130      format('Unrecognizable text: (',a80,')') 
           call prterx ('W',1)  
           inptls=0 
           go to 740
        endif   
C       
C       PROCESS ">" CONTROL 
C       
        bigbuf = buf
        endrcd = .false.
  114   last = lastch (bigbuf)
        if (bigbuf(last:last) .eq. '-') then  
           read (inp, 190, end=116) buf   
           write (outbuf,100) buf(1:80) 
           call prtout(1)   
           if (buf(1:1) .ne. '.') bigbuf(last:) = ljstfy (buf)
           buf = bigbuf   
           go to 114  
  116      endrcd = .true.
        endif 
        
        call scan (bigbuf, word, nwrd)   
        do i = 1, nwrd 
           word(i)=capital (word(i))
        enddo
C       
C       SEARCH DICTIONARY   
C       
        found = .false.
        i = 1
        do while (i .le. lsin .and. .not. found)
           last = index(sindic(i),'*')-1  
           if (last .le. 0) last = len(sindic(i))
           if (findstr(word(1), sindic(i)(1:last)) .ne. 0) then
              found = .true.
           else
              i = i + 1
           endif
        enddo
        if (.not. found) then
           write (errbuf(1)(1:120),160) word(1)(1:10),buf(1:70)
  160      format('UNIDENTIFIED KEYWORD(',a,') IN TEXT(',a,')')
           call prterx ('W',1) 
           kerrsw=1
           inptls = 0  
           go to 740   
        endif

        idat = 0
        idat2 = 0   
        assign 740 to inext 
        inptls = 0  
C       
C              OPT REA DIS INC EXC SEN INC  DEB   TRL SOL  TOL  AIC 
C                1   2   3   4   5   6   7    8     9  10   11   12 
        go to (180,320,390,450,460,530,590, 680,10800,620, 640, 610,
C       
C          LIM MIS   TRB LTC   BAS   GEN   LOD    BX   SVC  
C           13  14    15  16   17   18   19 
     1     660,700,10700,600,10870,10900,10910,10920,20920) i   
C       
C               ** OPTIMIZE **          : ISLNSW = 1
C       
  180   continue
        islnsw = 1  
        inptls = 0  
        assign 210 to inext 
C       
C       CHECK FOR CONTINUATION RECORDS (ONE MAXIMUM,
C       "+" IN COLUMN 1)
C       
        if (index (buf,'<').eq.0) then  
           read (inp,190,end=750) buf   
  190      format (a)   
           write (outbuf,100) buf(1:80) 
           call prtout(1)   
           if (buf(1:1).eq.'+') then
              call scan(buf(2:),word(nwrd+1),jwrd)  
              do 200 i = 1,jwrd 
  200         word(i+nwrd) = capital (word(i+nwrd))  
              nwrd = nwrd + jwrd
           else 
              inptls=1  
           endif
        endif   
  210   i=0 
  220   i=i+1   
        if (i.gt.nwrd) go to 300
        if (findstr(word(i),'ZONE').ne.0) then
           do 260 j=i+1,nwrd
           if (findstr(word(j),'BASE').ne.0) go to 270
           idat=idat+1  
           if(idat.gt.100) then 
              write (errbuf(1),230) 
  230         format('MORE THAN 100 OPTIMIZE ZONES SUBMITTED.') 
              call prterx ('W',1)   
              idat=100  
           else 
              if (word(j)(3: ).eq.' ') go to 250
              write (errbuf(1),240) word(j) 
  240         format('0 ZONE"',a10,'" HAS MORE THAN TWO CHARACTERS.',   
     1        ' ONLY FIRST TWO CHARACTERS ACCEPTED.')   
              call prterx ('W',1)   
  250         zondat(idat) = word(j)(1:2)   
              data(1,idat) = 0.0
           endif
  260      continue 
           go to 300
  270      i=j-1
           go to 220
        else if (findstr(word(i),'BASE').ne.0) then   
           do 290 j=i+1,nwrd
           idat2=idat2+1
           if(idat2.gt.100) then
              write (errbuf(1),280) 
  280         format('MORE THAN 100 OPTIMIZED BASE KV''S SUBMITTED.')   
              call prterx ('W',1)   
              idat=100  
           else 
              bsedat(idat2)=rval(word(j))   
           endif
  290      continue 
           go to 300
        else
           write (errbuf(1)(1:120),160) word(i)(1:10),buf(1:70) 
           call prterx ('W',1)  
        endif   
C       
C       CHECK FOR EXTRACTED DATA
C       
  300   if(idat+idat2.eq.0) then
           write (errbuf(1),310) buf(1:80)  
  310      format('0 NO ZONE/BASE DATA EXTRACTED FROM TEXT:(',a80,')')  
           call prterx ('W',1)  
           islnsw = 0   
        endif   
        go to 740   
C       
C       PROCESS > BASE_SOLUTION <   
C       
10870   islnsw = 0  
        iopton(20) = 1  
        go to 740   
C       
C       PROCESS > GEN_DROP <
C       
10900   islnsw = 9  
        go to 740   
C       
C       PROCESS > LOAD_SOLUTION <   
C       
10910   islnsw = 0  
        iopton(20) = 2  
        call lodsol (error) 
        inptls = 1  
        go to 740   
C       
C       PROCESS > REACTIVE PENALTIES    : ISLNSW = 2
C       
  320   islnsw = 2  
  330   continue
C       
        ksw = 1 
        assign 370 to inext 
  340   read (inp,190,end=750) buf  
        card = buf(1:1) 
        write (outbuf,100) buf(1:80)
        call prtout(1)  
        inptls=1
        if (card.ne.'B') go to 370  
        inptls = 0  
        if (ksw.ne.1) go to 340 
        idat = idat + 1 
C       
        if (idat .gt. MXQPEN) then
            write (errbuf(1),350) MXQPEN
  350       format('0 MORE THAN ',i4,' REACTIVE PENALTY BUSSES.',   
     1    ' REMAINDER IGNORED. ')   
            call prterx ('W',1) 
            idat=MXQPEN 
            ksw = 2 
        else
            read (buf,360,err=720) namdat(idat),bsedat(idat)
  360       format(bz, 6x, a8, f4.0)  
        endif   
        go to 340   
C       
  370   if (idat.eq.0) then 
              write (errbuf(1),380) 
  380         format('0 NO BUS DATA FOLLOWS "REACTIVE PENALTIES" ', 
     1    'TEXT.')  
              call prterx ('W',1)   
              inptsw = 0
        endif   
        go to 740   
C       
C       Process > ECONOMIC_DISPATCH     : ISLNSW = 3
C       
  390   islnsw = 3  
        ksw = 1 
        assign 430 to inext 
  400   read (inp,190,end=750) buf  
        card = buf(1:1) 
        write (outbuf,100) buf(1:80)
        call prtout(1)  
        inptls=1
        if (card.ne.'B') go to 430
        inptls = 0
        if (ksw.ne.1) go to 400
        idat = idat + 1
C
        if (idat.gt.100) then
            write (errbuf(1),410)
  410       format('0 MORE THAN 100 DISPATCH BUSES SPECIFIED. ',
     &             'THE REMAINDER WILL BE IGNORED. ')
            call prterx ('W',1)
            idat=100
            ksw = 2 
        else
            read (buf,420,err=720) namdat(idat),bsedat(idat),   
     1        (data(j,idat),j=1,5)  
  420       format(bz, 6x, a8, f4.0, 6x, 2f6.1, 6x, 3e9.3)
        endif   
C       
        go to 400   
  430   if (idat.eq.0) then 
              write (errbuf(1),440) 
  440         format('0 NO BUS DATA FOLLOWS "DISPATCH BUSES" ', 
     1       'TEXT.')   
              call prterx ('W',1)   
              islnsw = 0
        endif   
        go to 740   
C       
C       Process > INCLUDE optim busses  : ISLNSW = 4
C       
  450   islnsw = 4  
        go to 470   
C       
C       Process > EXCLUDE optim busses  : ISLNSW = 5
C       
  460   islnsw = 5  
C       
  470   ksw = 1 
        assign 510 to inext 
  480   read (inp,190,end=750) buf  
        card = buf(1:1) 
        write (outbuf,100) buf(1:80)
        call prtout(1)  
        inptls=1
        if (card.ne.'B') go to 510
        inptls = 0
        if (ksw.ne.1) go to 480
        idat = idat + 1
C
        if (idat.gt.100) then
            write (errbuf(1),490)
  490       format('0 MORE THAN 100 INCLUDE/EXCLUDE BUSSES SPECIFIED.',
     1             ' THE REMAINDER WILL BE IGNORED. ')
            call prterx ('W',1)
            idat=100
            ksw = 2 
        else
            read (buf,500,err=720) namdat(idat),bsedat(idat)
  500       format(bz, 6x, a8, f4.0)
        endif
        go to 480
C
  510   if (idat.eq.0) then
              write (errbuf(1),520)
  520         format('0 NO BUS DATA FOLLOWS "INCLUDE/EXCLUDE BUSES" ',
     1               'TEXT.')
              call prterx ('W',1)
              islnsw = 0
        endif   
        go to 740   
C       
C       Process > SENSITIVITIES       : ISLNSW = 6  
C       
C       > SENSITIVITY, LTC = ON, AI_CONTROL= ON <   
C                            OFF,            OFF
  530   islnsw = 6  
        go to 740   
C       
C       > INCLUDE_CON < 
C       
  590   inpold = inp
        inp = 30
        call opnfil (inp,word(2),ierror)
        go to 740   
C       
C   > LTC = OFF 
C           ON  
C           ON_NV   
C           ON_NPS  
C           ON_DCONLY   
C       
  600     continue  
        if (word(2) .eq. 'OFF') then
C                                              ** OFF **
           iopton(16) = 0   
        else if (word(2) .eq. 'ON') then
C                                              ** ON ** 
           iopton(16) = 2   
        else if (word(2) .eq. 'ONNV' ) then 
C                                              ** ON_NV **  
           iopton(16) = 1   
        else if (word(2) .eq. 'ONNPS' ) then
C                                              ** ON_NPS ** 
           iopton(16) = 3   
        else if (word(2) .eq. 'ONDCONLY' ) then 
C                                              ** ON_DCONLY **  
           iopton(16) = 4   
        else
           write (errbuf(1)(1:120),160)word(2)(1:10),buf(1:70)  
           call prterx ('W',1)  
        endif   
        islnsw = 0  
        go to 740   
C       
C    > AI_CONTROL= ON,OFF  )
C       
  610   continue
        if (word(2).eq.'OFF') then  
C                                               ** OFF **   
           iopton(17) = 0   
        else if ( word(2).eq.'CON') then
C                                               ** CON **   
           iopton(17) = 1   
        else if ( word(2).eq.'MON') then
C                                               ** MON **   
           iopton(17) = 2   
        else
           write (errbuf(1)(1:120),160) word(2)(1:10),buf(1:70) 
           call prterx ('W',1)  
        endif   
C       
C          READ NEXT RECORD.... 
           islnsw = 0   
           go to 740
C       
C  > SOL_ITER / DECOUPLED = nnn / NEWTON = nnn / OPTIM = nnn
C       
  620   do 630 i=2,nwrd-1,2 
C       
        jsln = ickdic( word(i),soldic  ,lsoli)  
C       
        if(jsln.gt.0) then  
C       
           isub = itepnt(jsln)  
           if(isub.lt.0) then   
              option(-isub) = rval(word(i+1))   
           else 
              iopton(isub) = rval(word(i+1))
           endif
C       
        else
           write (errbuf(1)(1:120),160) word(i)(1:10),buf(1:70) 
           call prterx ('W',1)  
        endif   
  630   continue
        islnsw = 0  
        go to 740   
C       
C    > TOLERANCE / BUS = nn / AREA = nn / TX = nn / Q = nn / OPCUT = nn 
C       
  640   do 650 i=2,nwrd-1,2 
C       
        jsln = ickdic( word(i),toldic,ltol )
C       
C       
        if(jsln.gt.0) then  
C       
           isub = tolpnt(jsln)  
           if(isub.lt.0) then   
              option(-isub) = rval(word(i+1))   
           else 
              iopton(isub) = rval(word(i+1))
           endif
C       
        else
           write (errbuf(1)(1:120),160) word(i)(1:10),buf(1:70) 
           call prterx ('W',1)  
        endif   
  650   continue
        islnsw = 0  
        go to 740   
C       
C       
C  > LIMITS / QRES = nn / PHA = nn / DEL_P_MIN = nn / DEL_P_MAX = nn
C           / DEL_ANG = nn           DEL_V_MIN = nn / DEL_V_MAX = nn
C           / DEL_VOLT = nn          DEL_T_MIN = nn / DEL_T_MAX = nn
C       
  660   do 670 i=2,nwrd-1,2 
C       
        jsln = ickdic( word(i),limdic,llim )
        if(jsln.gt.0) then  
C       
           isub = limpnt(jsln)  
           if(isub.lt.0) then   
              option(-isub) = rval(word(i+1))   
           else 
              iopton(isub) = rval(word(i+1))
           endif
C       
        else
           write (errbuf(1)(1:120),160) word(i)(1:10),buf(1:70) 
           call prterx ('W',1)  
        endif   
  670   continue
        islnsw = 0  
        go to 740   
C       
C       
C       
C       
C       > DEBUG / TX = OFF / BUS = OFF / AI = OFF / DCMODEL = OFF   
C                      ON          ON         ON              ON
C               /OPT = OFF / OPV = OFF / OPP = OFF/ 
C                      ON          ON          ON   
C       
  680   do 690 i=2,nwrd-1,2 
C       
        jsln = ickdic( word(i),debdic,ldeb )
        if(jsln.ne.0) then  
C       
           isub = debpnt(jsln)  
           if (word(i+1).eq.'OFF') iopton(isub) = 0 
           if (word(i+1).eq.'ON') iopton(isub) = 1  
        else
           write (errbuf(1)(1:120),160) word(i)(1:10),buf(1:70) 
           call prterx ('W',1)  
        endif   
C       
  690   continue
        islnsw = 0  
        go to 740   
C       
C       
C       
  700   do 710 i = 2, nwrd-1, 2 
C       
C          > MISC_CNTRL,   VFLATstart=ON,   
C                          RELAXfactor=1.0, 
C                          XRGHOSTnode=ON,  
C                          VOLTAGERelax=OFF,
C                          SAVEBE=OFF,  
C                          DClp=OFF,
C                          X_BUS=BPA,   
C                          NUMVSTEPS=##,
C                          TSTART=##,
C                          T_CONTROL=AUTO 
C                                    MANUAL 
C                          PHASE_SHIFTER_BIAS = BPA
C                                               WSCC
C                          BRIDGE_CURRENT_RATING = ON
C                                                  OFF
C
C
        jsln = ickdic( word(i), condic, lcon )
        if (jsln.gt.0) then  
C       
           isub = conpnt(jsln)  
C       
C          RELAXfactor  
C       
           if ( jsln .eq. 2 ) then  
              if(isub.lt.0) then
                 option(-isub) = rval(word(i+1))
              endif 
C       
C          X_BUS factor 
C       
           else if ( jsln .eq. 7 ) then 
              if (word(i+1) .eq. 'BPA') then
                 kspare(24) = 0 
              else if (word(i+1) .eq. 'WSCC') then  
                 kspare(24) = 1 
              else if (word(i+1) .eq. 'VMAX') then  
                 kspare(24) = 2 
              endif 
C       
C          NUMVSTEPS
C       
           else if ( jsln .eq. 8 ) then 
              iopton(39) = rval(word(i+1))  
C       
C          TSTART   
C       
           else if ( jsln .eq. 9 .and. 
     &               index (word(i), '500') .eq. 0) then 
              option(40) = rval(word(i+1))  
C       
C          TSTART500
C       
           else if ( jsln .eq. 9 ) then 
              option(35) = rval(word(i+1))  
C       
C          ITER_SUMMARY 
C       
           else if ( jsln .eq. 11 ) then
              if (word(i+1) .eq. 'OFF') iopton(22) = 0  
              if (word(i+1) .eq. 'ON') iopton(22) = 1   
C       
C          PHASE_SHIFTER_BIAS 
C       
           else if ( jsln .eq. 12 ) then 
              if (word(i+1) .eq. 'BPA') then
                 iopton(21) = 0 
              else if (word(i+1) .eq. 'WSCC') then  
                 iopton(21) = 1 
              endif 
C       
C          BRIDGE_CURRENT_RATING = ON
C                                  OFF
C       
           else if ( jsln .eq. 13 ) then 
              if (word(i+1) .eq. 'ON') then
                 iopton(23) = 0 
              else if (word(i+1) .eq. 'OFF') then  
                 iopton(23) = 1
              endif 
C       
C          All other parameters 
C       
           else 
              if(word(i+1).eq.'OFF') iopton(isub) = 0   
              if(word(i+1).eq.'ON') iopton(isub) = 1
           endif
        else
           write (errbuf(1)(1:120),160) word(i)(1:10),buf(1:70) 
           call prterx ('W',1)  
C       
        endif   
  710   continue
        inptls = 0  
        go to 740   
C       
C       PROCESS TRACE BUSES FOR SOLUTION HISTORY : ISLNSW = 7   
C       
10700   islnsw = 7  
C       
10710   ksw = 1 
        assign 10750 to inext   
10720   read (inp,190,end=750) buf  
        card = buf(1:1) 
        write (outbuf,100) buf(1:80)
        call prtout(1)  
        inptls=1
        if (card.ne.'B') go to 10750
        inptls = 0  
        if (ksw.ne.1) go to 10720   
        idat = idat + 1 
C       
        if (idat.gt.50) then
            write (errbuf(1),10730)
10730   format('0 MORE THAN 50 BUSES FOR "TRACE_BUS" SPECIFIED.',
     1         ' THE REMAINDER WILL BE IGNORED. ')
            call prterx ('W',1)
            idat=50
            ksw = 2
        else
            read (buf,10740,err=720) namdat(idat),bsedat(idat)  
10740   format(bz, 6x, a8, f4.0)  
        endif   
        go to 10720 
C       
10750   if (idat.eq.0) then 
           write (errbuf(1),10760)  
10760      format('0 NO BUS DATA FOLLOWS "TRACE_BUS" TEXT.')
           call prterx ('W',1)  
           islnsw = 0   
        endif   
        go to 740   
C       
C       Process > TRACE_LTC's for solution history : ISLNSW = 8 
C       
10800   islnsw = 8  
C       
10810   ksw = 1 
        assign 10850 to inext   
10820   read (inp,190,end=750) buf  
        card = buf(1:1) 
        write (outbuf,100) buf(1:80)
        call prtout(1)  
        inptls=1
        if (card.ne.'R') go to 10850
        inptls = 0  
        if (ksw.ne.1) go to 10820   
        idat = idat + 2 
C       
        if (idat.gt.40) then
            write (errbuf(1),10830)
10830       format('0 MORE THAN 20 LTC`S FOR "TRACE_LTC" SPECIFIED.',
     1             ' THE REMAINDER WILL BE IGNORED. ')
            call prterx ('W',1)
            idat=40
            ksw = 2
        else
            read (buf,10840,err=720) namdat(idat-1),bsedat(idat-1), 
     1         namdat(idat),bsedat(idat)
10840       format(bz, 6x, a8, f4.0, 1x, a8, f4.0)   
        endif   
        go to 10820 
C       
10850   if (idat.eq.0) then 
           write (errbuf(1),10860)  
10860      format('0 NO LTC DATA FOLLOWS "TRACE_LTC" TEXT.')
           call prterx ('W',1)  
           islnsw = 0   
        endif   
        go to 740   
C       
C       Process > BX = LOCK, read following "BX" records
C       
10920   numlck = 0  
10930   read (inp, 10940, end=11020) buf
10940   format (a)  
        card = buf(1:1) 
        write (outbuf, 100) buf(1:80)   
        call prtout (1) 
        
        if (card .eq. '.') then 
           go to 10930  
        else if (card .eq. 'B') then
           read (buf, 10950) bus1, base1, xinit, vmax, vmin 
10950      format (bz, t7, a8, f4.0, t35, f4.0, t58, 2f4.3)  
        
           nb = find_bus (bus1, base1)
           if (nb .le. 0) then  
              write (errbuf(1), 10960) bus1, base1  
10960         format (' LOCKED BX bus (', a8, f6.1, 
     1                  ') is not in system.')  
              call prterx ('W', 1)  
           else 
              kt = inp2opt(nb)
              ntyp = ntypu(kt)                     
              ntbx = ptrtbx(nb)
              if (ntbx .eq. 0) then
                 call typno (type, ntyp)
                 write (errbuf(1), 10970) bus1, base1, 'B'//type
10970            format ('> LOCKED BX bus (', a8, f6.1, 
     1               ') is improper type (', a2, ')')   
                 call prterx ('W', 1)   
              else if (tbx(1,ntbx) .ne. 5) then
                 if (tbx(8,ntbx) .gt. 0 .and.
     1               tbx(8,ntbx) .ne. tbx(2,ntbx)) then
                    call typno (type, ntyp)
                    write (errbuf(1), 11000) 'B'//type, bus1, base1
11000               format ('> LOCKED ', a, ' bus (', a8, f6.1,
     1                      ') controls a remote bus.')
                    call prterx ('W', 1)
                    tbx(8,ntbx) = 0
                 endif  
                 if (numlck .le. MAXXDT) then   
                    numlck = numlck + 1 
                    nxlock(numlck) = nb 
                    if (vmin .eq. 0.0) vmin = vlimn(kt)   
                    if (vmax .eq. 0.0) vmax = vlimx(kt)   
                    vxlock(1,numlck) = vmin 
                    vxlock(2,numlck) = vmax 
                    bxlock(numlck) = xinit  
                    lxloc = tbx(5,ntbx)
C       
C                   Function XDSCRT computes closest discrete value.
C       
                    disc = xdscrt (lxloc, nb, xinit, b1, b2)
                    binit(numlck) = disc
C       
C                   Change XDATA to nearest discrete state. 
C       
                    busdta(6,nb) = disc 
                    oldxdt = xdata(5,lxloc) + xdata(6,lxloc)
                    xdata(5,lxloc) = amin1 (disc, 0.0)  
                    xdata(6,lxloc) = amax1 (disc, 0.0)  
                    badj = xdata(5,lxloc) + xdata(6,lxloc) - oldxdt 
                    bkku(kt) = bkku(kt) + badj/bmva   
                    tbx(7,ntbx) = 4
                 else   
                    write (errbuf(1), 10990) MAXXDT 
10990               format (' More than ', i3, ' LOCKED BX buses ') 
                    call prterx ('W', 1)
                 endif  
              else  
                 call typno (type, ntyp)
                 write (errbuf(1), 11010) bus1, base1, 'B'//type
11010            format ('> LOCKED BX bus (', a8, f6.1, 
     1               ') is improper type (', a2, ')')   
                 call prterx ('W', 1)   
              endif 
           endif
           go to 10930  
        endif   
        write (outbuf, 11012)   
11012   format (t2, 'Bus', t22, 'Initial discrete shunt',   
     1          t46, '  Voltage limits')
        call prtout (1) 
        write (outbuf, 11014)   
11014   format (t22, ' Specified  Actual',  
     1          t46, '   Vmin    Vmax') 
        call prtout (1) 
        do 11018 i = 1, numlck  
           write (outbuf, 11016) bus(nxlock(i)), base(nxlock(i)),   
     1        binit(i), bxlock(i), vxlock(1,i), vxlock(2,i) 
11016      format (t2, a8, f7.1, t22, 2f8.1, t46, 2f8.3)
           call prtout (1)  
11018   continue
        go to 11030 
C       
C       Process > SVC, read following "B" records   
C       
20920   numsvc = 0  
20930   read (inp, 10940, end=21050) buf
        error = 0   
        card = buf(1:1) 
        write (outbuf, 100) buf(1:80)   
        call prtout (1) 
        
        if (card .eq. '.') then 
           go to 20930  
        else if (card .eq. 'B') then
           read (buf, 20950) bus1, base1
20950      format (bz, t7, a8, f4.0)  
        
           nb = find_bus (bus1, base1)
           if (nb .le. 0) then  
              write (errbuf(1), 20960) bus1, base1  
20960         format (' SVC bus (', a8, f6.1, ') is not in system.')
              call prterx ('W', 1)  
           else 
              kt = inp2opt(nb)
              ntyp = ntypu(kt)                     
              ntbx = ptrtbx(nb)
              if (ntbx .eq. 0) then
                 call typno (type, ntyp)
                 write (errbuf(1), 20990) bus1, base1, 'B'//type
20990            format ('> SVC bus (', a8, f6.1,   
     1               ') is improper type (', a2, ')')   
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
              else if (tbx(1,ntbx) .eq. 3) then
                 if (tbx(8,ntbx) .gt. 0 .and.  
     1               tbx(8,ntbx) .ne. tbx(2,ntbx)) then   
                    call typno (type, ntyp) 
                    write (errbuf(1), 21000) 'B'//type, bus1, base1 
21000               format ('> SVC ', a, ' bus (', a8, f6.1,
     1                      ') controls a remote bus.')
                    call prterx ('W', 1)
                 endif  
C       
C                Search for valid continuation bus. 
C       
                 found = .false.
                 ncb = kbsdta(15,nb)
                 do while (ncb .gt. 0 .and. .not. found)
                    call getchr(1,cbtyp,kbctbl(8,ncb))   
                    call getchr(2,cbkyr,kbctbl(9,ncb))   
                    call getchr(3,cbown,kbctbl(10,ncb))  
                    if (cbtyp .eq. 'X' .and. 
     1                  cbkyr .eq. '*I') then
                       found = .true.
                    endif
                    ncb = bctbl_nxt(ncb)
                 enddo
                 if (.not.found) then   
                    write (errbuf(1), 21004)
21004               format (' SVC bus missing following continuation ',
     &                      'bus record: ')
                    write (errbuf(2), 21006) buf(7:19)
21006               format (' +X    ', a, '*I')
                    if (is_batch .eq. 0) then
                       call prterx ('E',2)
                    else
                       call prterx ('F',2)
                    endif
                    error = 1   
                 endif  
        
                 if (numsvc .le. MAXXDT) then   
                    numsvc = numsvc + 1 
                    svc(1,numsvc) = nb 
                    svc(2,numsvc) = 2    ! Normal SVC state
                    svc(3,numsvc) = ncb
                    svc(4,numsvc) = ntbx   
                    do 21007 i = 5,12   
21007               svc(i,numsvc) = 0.0 
                    vmin = 0.0  
                    vmax = 0.0  
                    call scan (buf(20:), word, nwrd)
                    do 21018 i = 1, nwrd, 2 
                       if (word(i) .eq. 'BMIN') then
                          last = lastch (word(i+1))
                          write (fmt, '(''(f'', i2, ''.0)'')') last
                          read (word(i+1),fmt,err=21014) svc(5,numsvc)
                          svc(5,numsvc) = svc(5,numsvc) / bmva  
                       else if (word(i) .eq. 'BMAX') then   
                          last = lastch (word(i+1))
                          write (fmt, '(''(f'', i2, ''.0)'')') last
                          read (word(i+1),fmt,err=21014) svc(6,numsvc)
                          svc(6,numsvc) = svc(6,numsvc) / bmva  
                       else if (word(i) .eq. 'VMIN') then   
                          last = lastch (word(i+1))
                          write (fmt, '(''(f'', i2, ''.0)'')') last
                          read (word(i+1),fmt,err=21014) svc(9,numsvc)
                       else if (word(i) .eq. 'VMAX') then   
                          last = lastch (word(i+1))
                          write (fmt, '(''(f'', i2, ''.0)'')') last
                          read (word(i+1),fmt,err=21014)svc(10,numsvc)
                       else 
                          l = lastch(word(i))   
                          write (errbuf(1), 21010) word(i)(1:l) 
21010                     format('0 Unrecognized keyword (', a, 
     1                       ') in svc bus record') 
                          write (errbuf(2), 21012) buf(1:80)
21012                     format('0 (', a80, ')')   
                          if (is_batch .eq. 0) then
                             call prterx ('E',2)
                          else
                             call prterx ('F',2)
                          endif
                          error = 1 
                       endif
                       go to 21018  
21014                  l = lastch(word(i+1))
                       write (errbuf(1), 21016) word(i+1)   
21016                  format('0 Illegal data in numeric field (', a,   
     1                       ') in SVC bus record') 
                       write (errbuf(2), 21012) buf(1:80)   
                       if (is_batch .eq. 0) then
                          call prterx ('E',2)
                       else
                          call prterx ('F',2)
                       endif
                       error = 1
21018               continue
        
                    if (svc(5,numsvc) .ge. 0.0 .or. 
     1                  svc(6,numsvc) .le. 0.0) then
        
        
                       write (errbuf(1), 21020) svc(5,numsvc) * bmva,   
     1                    svc(6,numsvc) * bmva  
21020                  format(' Illegal SVC current settings B_min (',  
     1                    f6.1, ' ) < 0.0 < B_max (', f6.1, 
     2                    ') in SVC bus record')
                       write (errbuf(2), 21012) buf(1:80)   
                       if (is_batch .eq. 0) then
                          call prterx ('E',2)
                       else
                          call prterx ('F',2)
                       endif
                       error = 1
                    endif   
        
                    if (svc(9,numsvc) .eq. 0.0) 
     1                 svc(9,numsvc) = vlimn(kt)  
                    if (svc(10,numsvc) .eq. 0.0)
     1                 svc(10,numsvc) = vlimx(kt) 
        
                    if (svc(9,numsvc) .le. 0.0  .or.
     1                  svc(10,numsvc) .le. 0.0 .or.
     2                  svc(9,numsvc) .ge. svc(10,numsvc)) then 
        
                       write (errbuf(1), 21022) svc(7,numsvc),  
     1                    svc(8,numsvc) 
21022                  format(' Illegal SVC voltage settings V_min (',  
     1                    f6.3, ' ) < V_max (', f6.3,   
     2                    ') in SVC bus record')
                       write (errbuf(2), 21012) buf(1:80)   
                       if (is_batch .eq. 0) then
                          call prterx ('E',2)
                       else
                          call prterx ('F',2)
                       endif
                       error = 1
                    endif   
                    if (error .eq. 0) then  
C       
C                      Compute equivalent currents and shunts.  
C       
                       svc(7,numsvc) = svc(5,numsvc) * svc(10,numsvc)   
                       svc(8,numsvc) = svc(6,numsvc) * svc(9,numsvc)
                       svc(11,numsvc) = (svc(8,numsvc) - svc(7,numsvc)) 
     1                                / (svc(9,numsvc) - svc(10,numsvc))
                       vref(numsvc) = svc(10,numsvc) * svc(8,numsvc)
     1                             - svc(9,numsvc) * svc(7,numsvc)  
                       vref(numsvc) = vref(numsvc)  
     1                              / (svc(8,numsvc) - svc(7,numsvc))   
                       svc(12,numsvc) = -vref(numsvc) * svc(11,numsvc)  
C       
C                      Remove existing +X data from Alpha tables.
C       
                       bkku(kt) = bkku(kt) -bctbl(5,ncb) / bmva   
                       ineti(kt) = ineti(kt) -bctbl(3,ncb) / bmva   
                       svc(13,numsvc) = bkku(kt)   
                       svc(14,numsvc) = -ineti(kt)

C                      Change +X data and Alpha data to SVC state 2.  

                       bctbl(5,ncb) = svc(11,numsvc) * bmva 
                       bctbl(3,ncb) = -svc(12,numsvc) * bmva
                       bkku(kt) = bkku(kt) +bctbl(5,ncb) / bmva   
                       ineti(kt) = ineti(kt) +bctbl(3,ncb) / bmva   
                       tbx(7,ntbx) = 1 
                    else
                       numsvc = numsvc - 1  
                    endif   
                 else   
                    write (errbuf(1), 21030) MAXXDT 
21030               format (' More than ', i3, ' SVC buses ')   
                    if (is_batch .eq. 0) then
                       call prterx ('E',1)
                    else
                       call prterx ('F',1)
                    endif
                 endif  
              else  
                 call typno (type, ntyp)
                 write (errbuf(1), 21040) bus1, base1, 'B'//type
21040            format ('> SVC bus (', a8, f6.1,   
     1               ') is improper type (', a2, ')')   
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
              endif 
           endif
           go to 20930  
        endif   
21050   write (outbuf, 21052)   
21052   format (t2, 'Bus',  
     1          t22, ' Bmin     Bmax',  
     2          t40, ' Vmin     Vmax',  
     3          t58, ' Imin     Imax',  
     4          t76, ' Bref     Iref     Vref') 
        call prtout (1) 
        write (outbuf, 21060)   
21060   format (t22, '(MVAR)   (MVAR)', 
     1          t40, '(p.u.)   (p.u.)', 
     2          t58, '(MVAR)   (MVAR)', 
     3          t76, '(MVAR)   (MVAR)   (p.u.)')
        call prtout (1) 
        do 21080 i = 1, numsvc  
           j = svc(1,i)
           write (outbuf, 21070) bus(j), base(j),   
     1        svc(5,i) * bmva, svc(6,i) * bmva, svc(9,i), svc(10,i),
     2        svc(7,i) * bmva, svc(8,i) * bmva, svc(11,i) * bmva,   
     3        svc(12,i) * bmva, vref(i) 
21070      format (t2, a8, f7.1, t22, f6.1, f9.1, t40, f6.3, f9.3,  
     1        t58, 2f8.1, t76, 2f8.1, f8.3) 
           call prtout (1)  
21080   continue
        go to 11030 
        
11020   buf = '( END ) SINPUT'  
        card = buf(1:1) 
        
11030   islnsw = 0  
        inptls = 1  
        go to 740   
C       
  720   write (errbuf(1),730) buf(1:80) 
  730   format('0 ILLEGAL CHARACTER DATA IN FIELD: (',a80,')')  
        call prterx ('W',1) 
        go to inext (210,370,430,510,740,750,10750,10850)   
C       
C       READ NEXT RECORD
C       
  740   if (islnsw.ne.0) go to 760  
        if (inptls.eq.0) then   
           read (inp,190,end=750) buf   
           write (outbuf,100) buf(1:80) 
           call prtout(1)   
           inptls=1 
        endif   
        go to 110   
C       
  750   if (inp.eq.inpold) then 
           buf='( END ) SINPUT' 
           card=buf(1:1)
           inptls = 1   
        else
           inp=inpold   
           inptls = 0   
        endif   
        go to inext (210,370,430,510,740,750,10750,10850)   
  760   continue
        return  
      end   

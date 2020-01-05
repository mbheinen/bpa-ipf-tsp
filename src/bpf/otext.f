C    @(#)otext.f	20.9 5/27/99
        subroutine otext
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/com006.inc'
        include 'ipfinc/coment.inc'
        include 'ipfinc/cont.inc'
        include 'ipfinc/dflrat.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/comm_mode.inc'
        include 'ipfinc/zbdata.inc'
        include 'ipfinc/miscfile.inc' 

        common /common_mode_only/ common_mode_only
        logical common_mode_only 

        character word(40)*60, capital*60, linefeed*1
        logical readnx, baseok, finished
        integer error
        external kmprat,swprat
C
C       CONTROL : LEVEL 1 DICTIONARY 
C
        linefeed = char(10)
        inpold = inp
        kerrsw = 0

        kase1(10) = 5   ! default  EXPAND_NET option

c       Initialize Common Mode counters here to allow the data from multiple
c       COMMON_MODE  commands/files to be appended
c
        num_comm_mode = 0
        num_changes = 0
        num_types = 0
        common_mode_only = .false.

        call space (2)
C
C       READNX:  .TRUE. -  "BUF" PROCESSED. READ NEXT CARD
C                .FALSE. - "BUF" NOT PROCESSED. DO NOT READ NEXT CARD
C
        readnx = .true.
        write (outbuf,110) buf(1:80)
  110   format (' OUTAGE TEXT (',a,')')
        call prtout(1)
        go to 620
C
  120   card = buf(1:1)
C
C       COMMENT CARD
C
        if (card .eq.'.') then
           readnx = .true.
           go to 620
C
C          HEADER CARD
C
        else if (card .eq. 'H') then
           outbuf = buf(2: )
           call comlod(2)
           readnx = .true.
           go to 620
C
C          COMMENT CARD
C
        else if (card .eq. 'C') then
           if (ncom .gt. MAXCMT) then
              write (errbuf(1),122) MAXCMT
  122         format(' TOO MANY COMMENTS ..MAX=',i5,
     1               '. FOLLOWING RECORD SKIPPED')
              write (errbuf(2),124) buf(1:80)
  124         format(' (',a,')')
              call prterx ('W',2)
           else
              ncom = ncom + 1
              com(ncom) = buf(2:80)
           endif
           readnx = .true.
           go to 620
C
C          PROGRAM CONTROL CARD
C
        else if (index('/(',card) .ne. 0) then
           go to 660
C
C          MEANINGLESS CONTROL RECORD
C
        else if (card .ne. '>') then
           write (errbuf(1),180) buf(1:80)
  180      format('0 UNRECOGNIZABLE TEXT: (',a80,')')
           call prterx ('W',1)
           readnx = .true.
           go to 620
C
        endif
C
C       PROCESS ">" CONTROL.
C
        call uscan (buf, word, nwrd, '#', ',= ' // linefeed)
c
c       Uppercase all words and remove any underscore characters
c       from all words except file names.
c
        iwrd = 1
        finished = .false.
        do while (iwrd .lt. nwrd .and. .not. finished)
           word(iwrd) = capital (word(iwrd))
           do while (index(word(iwrd), '_') .ne. 0)
              i = index (word(iwrd), '_')
              word(iwrd)(i:) = word(iwrd)(i+1:)
           enddo
           if (word(iwrd) .eq. 'OLDBASE' .or.
     &         word(iwrd)(1:7) .eq. 'INCLUDE' .or.
     &         word(iwrd)(1:10) .eq. 'COMMONMODE') then
              finished = .true.
           else
              iwrd = iwrd + 1
           endif
        enddo
C
C       SEARCH DICTIONARY
C
        do i = 1,lctl
           l = index(dict(i),'*')-1
           if (l .le. 0) l = len(dict(i))
           if (index(word(1),dict(i)(1:l)) .ne. 0) go to 210
        enddo
        write (errbuf(1)(1:120),390) word(1)(1:10),buf(1:70)
        call prterx ('W',1)
        kerrsw = 1
        readnx = .true.
        go to 620
C
C                 1    2    3    4    5    6    7    8    9   10  
C               ANA, RED, DEB, DEF, EXP, GEN, INC, LOW, MIN, NOS,
  210   go to  (220, 550, 240, 250, 310, 330, 340, 350, 360, 370,
C
C          11   12   13   14   15   16   17   18    19   20   21   22
c         OUT,OUTP,OUTS,OVER, PHA, REA,REAL, RED, RELX,RELB, SET, SOL,
     &    410, 500, 510, 420, 520, 530, 540, 550, 570,  580, 590, 600,
c         TOL, OLD, COM, TRA
c          23   24   25   26
     &    610, 402,10610,10620) i
C
C       > ANALYSIS <
C
  220   if (word(2) .eq. 'OFF') kase1(18) = 0
        if (word(2) .eq. 'ON') kase1(18) = 1
        readnx = .true.
        if (nwrd .lt. 3) go to 620
        if (word(3) .eq. 'MINLOADING') then
           case1(15) = ftn_atof (word(4))
           tpc = 0.01*case1(15)
        else
           last1 = lastch (word(3))
           last2 = min0 (lastch (buf), 60)
           write (errbuf(1), 390) word(3)(1:last1), buf(1:last2)
           call prterx ('W',1)
        endif
        go to 620
C
C       > DEBUG C
C
  240   if (word(2) .eq. 'OFF') kase1(8) = 0
        if (word(2) .eq. 'ON') kase1(8) = 1
        readnx = .true.
        go to 620
C
C       > DEFAULT RATINGS
C
  250   read (inp,260,end=290) buf
  260   format(a)
        write (outbuf,110) buf(1:80)
        call prtout(1)
        readnx = .false.
        if (buf(1:2) .ne. ' ') go to 300
        nrat = nrat+1
        if (nrat .gt. 50) then
           write (errbuf(1),270) buf(1:80)
  270      format ('MORE THAN 50 DEFAULT RATINGS (',a,')')
           call prterx ('W',1)
           kerrsw = 1
           go to 250
        endif
        read (buf,280) zrat(1,nrat),zrat(2,nrat),zrat(3,nrat)
  280   format (bz,3x,f4.0,2x,f4.0,2x,f4.0)
        go to 250
  290   if (inp .eq. inpold) then
           buf = '( END ) OTEXT'
           card = buf(1:1)
        else
           readnx = .true.
           inp = inpold
        endif
  300   nrat = min0(nrat,50)
        call qiksrt (1,nrat,kmprat,swprat)
        go to 620
C
C       > EXPAND_NETWORK <
C
  310   kase1(10) = ftn_atof (word(2))
  320   readnx = .true.
        go to 620
C
C       > GEN_OUTAGE <
C
  330   if (word(2) .eq. 'NONE') then
           kase1(24) = 0
        else
           kase1(24) = ftn_atof (word(2))
        endif
        readnx = .true.
        go to 620
C
C       > INCLUDE_CON <
C
  340   inpold = inp
        inp = lunscr1
        ierror = 0
        call opnfil(inp, word(nwrd), ierror)
        if (ierror .ne. 0) inp = inpold
        readnx = .true.
        go to 620
C
C       > LOW_VOLTAGE_SCREEN <
C
  350   case1(16) = ftn_atof (word(2))
        tps = 0.01*case1(16)
        readnx = .true.
        go to 620
C
C       > MIN_EQUIVALENT_Y
C
  360   case1(31) = ftn_atof (word(2))
        readnx = .true.
        go to 620
C
C       > NO_SOLUTION <
C
  370   do 400 i = 2,nwrd-1,2
        if (word(i) .eq. 'ANGLE') then
           case1(13) = ftn_atof (word(i+1))
           psm = case1(13)
        else if (word(i) .eq. 'DELTAV') then
           case1(14) = ftn_atof (word(i+1))
           vcrit = case1(14)
        else
           last1 = lastch (word(i))
           last2 = min0 (lastch (buf), 60)
           write (errbuf(1), 390) word(3)(1:last1), buf(1:last2)
  390      format('0 Unrecognized KEYWORD (', a, ') in TEXT: (', a, 
     &')')
           call prterx ('W',1)
        endif
  400   continue
        readnx = .true.
        go to 620
C
C       > OLD_BASE <
C
  402   jobreq(2) = 'OLD_BASE'
        do i = 1, nwrd
          last = lastch (word(i))
          write (*, 20000) i, word(i)(1:last)
20000     format (' word(', i2, ') = (', a, ')')
        enddo
        obasnm = word(nwrd)
        crun1(1) = ' '
        call gtbase (obasnm,crun1(1),baseok)
        if (.not.baseok) then
           jobreq(1) = 'QUIT'
           kerrsw = 1
        else
           jobreq(2) = ' '
        endif
        readnx = .true.
        go to 620
C
C       > OUTAGE <
C
  410   cntngn = .true.
        go to 430
C
C       > OVERLOAD <
 
  420   loadck = .true.
 
  430   vmin = 0.0
        vmax = 9999.0
        i = 1
  440   i = i+1
        if (i .gt. nwrd) go to 490
        if (index(word(i),'ZONE') .ne. 0) then
           do j = i+1,nwrd
              if (index(word(j),'BASE') .ne. 0) goto 460
              if (word(j) .eq. '00') word(j) = ' '
              if (index(word(1),'OVERLOAD') .ne. 0) then
                 no = no + 1
                 zno(no) = word(j)
              else
                 nc = nc + 1
                 znc(nc) = word(j)
              endif
           enddo
           i = j
           go to 440
  460      i = j-1
           go to 440
        else if (index(word(i),'BASE') .ne. 0) then
           do j = i+1,nwrd,2
              if (index(word(j),'ZONE') .ne. 0) go to 480
              vmin = ftn_atof (word(j))
              if (j+1 .gt. nwrd) go to 490
              if (index(word(j+1),'ZONE') .ne. 0) go to 480
              vmax = ftn_atof (word(j+1))
              if (j .ge. nwrd) go to 490
           enddo
           i = j
           go to 490
  480      i = j-1
        else
           write (errbuf(1)(1:120),390) word(i)(1:10),buf(1:70)
           call prterx ('W',1)
        endif
  490   continue
        if (index(word(1),'OUTAGE') .ne. 0) then
           vcl = amin1(vmin,vmax)
           vch = amax1(vmin,vmax)
        else
           vol = amin1(vmin,vmax)
           voh = amax1(vmin,vmax)
        endif
        readnx = .true.
        go to 620
C
C       > OUTPUT_PLTDTA <
C
  500   if (word(2) .eq. 'OFF') kase1(25) = 0
        if (word(2) .eq. 'ON') kase1(25) = 1
        readnx = .true.
        go to 620
C
C       > OUTPUT_SORT <
C
  510   do i = 2,nwrd
           if (word(i)(1:3) .eq. 'OUT') then
              kase1(3) = 0
           else if (word(i)(1:3) .eq. 'OVE') then
              kase1(3) = 1
           else if (word(i)(1:3) .eq. 'BOT') then
              kase1(3)=2
           else if (word(i)(1:3) .eq. 'OWN') then
              kase1(5) = 1
           else
              last1 = lastch (word(i))
              last2 = min0 (lastch (buf), 60)
              write (errbuf(1), 390) word(i)(1:last1), buf(1:last2)
              call prterx ('W',1)
           endif
        enddo
        readnx = .true.
        go to 620
C
C       > PHASE_SHIFTER <
C
  520   if (index(word(2),'POWER') .ne. 0) kase1(1) = 0
        if (index(word(2),'ANGLE') .ne. 0) kase1(1) = 1
        readnx = .true.
        go to 620
C
C       > REACTIVE_SOLUTION <
C
  530   kase1(2) = 0
        if (word(2) .eq. 'ON') kase1(2) = 0
        if (word(2) .eq. 'OFF') kase1(2) = 1
        irect = kase1(2)
        readnx = .true.
        go to 620
C
C       > REALLOCATE <
C
  540   if (word(2) .eq. 'NONE') kase1(23) = 0
        if (word(2) .eq. 'LOAD') kase1(23) = 1
        if (word(2) .eq. 'LOADGEN') kase1(23) = 2
        readnx = .true.
        go to 620
C
C       > REDUCTION < or > REDUCTION_DEBUG <
C
  550   if (index(word(1),'REDUCTIOND') .ne. 0) go to 560
        iequiv = 1
        kase1(21) = 0
        if (word(2) .eq. 'NONE') kase1(21) = 0
        if (word(2) .eq. 'SIMPLE') kase1(21) = 1
        if (word(2) .eq. 'OPTIMAL') kase1(21) = 2
        if (kase1(21) .eq. 0) iequiv = 0
        if (nwrd .gt. 2) then
           if (word(3) .eq. 'REI') then
              if (word(4) .eq. 'OFF') kase1(22) = 0
              if (word(4) .eq. 'ON') kase1(22) = 1
           else
              last1 = lastch (word(3))
              last2 = min0 (lastch (buf), 60)
              write (errbuf(1), 390) word(3)(1:last1), buf(1:last2)
              call prterx ('W',1)
           endif
        endif
        readnx = .true.
        if (word(2) .eq. 'NONE')iequiv = 0
        go to 620
C
C       > REDUCTION_DEBUG <
C
  560   if (word(2) .eq. 'NONE') kase1(30) = 0
        if (word(2) .eq. 'OFF') kase1(30) = 0
        if (word(2) .eq. 'MINOR') kase1(30) = 1
        if (word(2) .eq. 'ON') kase1(30) = 1
        if (word(2) .eq. 'MAJOR') kase1(30) = 2
        readnx = .true.
        go to 620
C
C       > RELAX_BR_RATE <
C
  570   if (word(2) .eq. 'ON') kase1(6) = 0
        if (word(2) .eq. 'OFF') then
           kase1(6) = 1
           ilinbc = 1
        endif
        if (nwrd .gt. 2) then
           if (word(3) .eq. 'PERCENT') then
              case1(17) = ftn_atof (word(4))
              olinbc = 0.01*case1(17)
           else
              last1 = lastch (word(3))
              last2 = min0 (lastch (buf), 60)
              write (errbuf(1), 390) word(3)(1:last1), buf(1:last2)
              call prterx ('W',1)
           endif
        endif
        readnx = .true.
        go to 620
C
C       RELIABILITY <
C
  580   if (word(2) .eq. 'OFF') kase1(9) = 0
        if (word(2) .eq. 'SINGLEAC') kase1(9) = 1
        if (word(2) .eq. 'SINGLEDC') kase1(9) = 2
        if (word(2) .eq. 'DOUBLEDC') kase1(9) = 3
        if (word(2) .eq. 'BUS') kase1(9) = 4
        readnx = .true.
        go to 620
C
C       >SET RATINGS <
C
  590   if (word(2) .eq. 'NOMINAL') kase1(35) = 0
        if (word(2) .eq. 'SUMMER') kase1(35) = 1
        if (word(2) .eq. 'WINTER') kase1(35) = 2
        if (nwrd .gt. 2) then
           if (word(3) .eq. 'FILE') then
              inpold = inp
              inp = lunscr1
              ierror = 0
              call opnfil(inp,word(4),ierror)
           else
              last1 = lastch (word(3))
              last2 = min0 (lastch (buf), 60)
              write (errbuf(1), 390) word(3)(1:last1), buf(1:last2)
              call prterx ('W',1)
           endif
        endif
        call chgrat
        if (index (buf,'( END ) CHGRAT') .ne. 0) then
           if (inp .eq. inpold) go to 660
           inp = inpold
           readnx = .true.
        else
           readnx = .false.
        endif
        go to 620
C
C       > SOLUTION_ITERATION <
C
  600   if (word(2) .eq. 'FIXED') then
           kase1(7) = 1
           npass = 3
           icon2 = 1
           kase1(11) = ftn_atof (word(3))
           if (kase1(11) .ne. 0) npass = kase1(11)
        else if (word(2) .eq. 'TOTOL') then
           kase1(7) = 0
           kase1(11) = ftn_atof (word(3))
           npass = kase1(11)
        else
           last1 = lastch (word(3))
           last2 = min0 (lastch (buf), 60)
           write (errbuf(1), 390) word(3)(1:last1), buf(1:last2)
           call prterx ('W',1)
        endif
        readnx = .true.
        go to 620
C
C       > TOLERANCE <
C
  610   case1(12) = ftn_atof (word(2))
        tol = case1(12)
        readnx = .true.
        go to 620
C
C       > COMMON_MODE <
C
10610   common_mode_only = (word(1)(1:14) .eq. 'COMMONMODEONLY')
        iwrd = 2
        if ( .not. cntngn ) then
           write (errbuf(1),612) 
  612      format(' The "> OUTAGE" option must preceed the ',
     &            '"> COMMON_MODE" option ')
           write (errbuf(2),124) buf(1:80)
  614      format(' (',a,')')
           call prterx('A',2)
           call erexit()
        endif

        if ((word(iwrd) .eq. 'FILE' .or. word(iwrd) .eq. 'file')
     &     .and. word(iwrd+1) .ne. '*') then
           inpold = inp
           inp = lunscr1
           ierror = 0
           call opnfil (inp, word(nwrd), ierror)
        endif
        call get_cmde(ierror)
        readnx = .false.
        go to 620
C
C       > TRENDING = OFF <
C
10620   trending = .false.
        if (word(2) .eq. 'ON') trending = .true.
        readnx = .true.
        go to 620
C
C       READ NEXT RECORD
C
  620   if (readnx) then
           read (inp,630,end=640) buf
  630      format(a)
           write (outbuf,110) buf(1:80)
           call prtout(1)
           readnx = .false.
        endif
        go to 120
  640   if (inp .eq. inpold) then
           buf = '( END ) OTEXT'
           card = buf(1:1)
        else
           inp = inpold
           readnx = .true.
           go to 620
        endif
  660   continue
        if ( num_comm_mode .gt. 0 ) call cmde_sum(error)
        return
        end

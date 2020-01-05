C    %W% %G%
        subroutine cmde2_txt (inpfil)
        integer inpfil
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

        character word(40)*60, cnvtwrd*60, tempc*60, linefeed*1
        logical readloop, baseok, finished
        integer error
        external kmprat,swprat
C
C       ***  CONTROL : LEVEL 1 DICTIONARY ***
C
C       PROCESS INPUT TEXT
C
        linefeed = char(10)
        inpold = inpfil
        kerrsw = 0

        common_mode_only = .false.

        call space (2)
C
C       READNX:  .TRUE. -  "BUF" PROCESSED. READ NEXT CARD
C                .FALSE. - "BUF" NOT PROCESSED. DO NOT READ NEXT CARD
C
        readloop = .true.
        write (outbuf, 10110) buf(1:80)
10110   format (' OUTAGE TEXT (',a,')')
        call prtout(1)

        do while (readloop)
          read (inpfil, fmt='(a)', end=110) buf
          go to 130
  110     if (inpfil .eq. inpold) then
            buf = '( END ) '
          else
            inpfil = inpold
            read (inpfil, fmt='(a)', end=120) buf
            go to 130
  120       buf = '( END ) '
          endif
  130     continue
          write (outbuf, 10110) buf(1:80)
          card = buf(1:1)
C
C         COMMENT CARD
C
          if (card .eq. '.') then
            go to 620
C
C         HEADER CARD
C
          else if (card .eq. 'H') then
            outbuf = buf(2: )
            call comlod(2)
            go to 620
C
C         COMMENT CARD
C
          else if (card .eq. 'C') then
            if (ncom .gt. MAXCMT) then
              write (errbuf(1), 10120) MAXCMT
10120         format(' TOO MANY COMMENTS ..MAX=',i5,
     1               '. FOLLOWING RECORD SKIPPED')
              write (errbuf(2), 10130) buf(1:80)
10130         format(' (', a, ')')
              call prterx ('W',2)
            else
              ncom = ncom + 1
              com(ncom) = buf(2:80)
            endif
            go to 620
C
C         PROGRAM CONTROL CARD
C
          else if (card .eq. '/') then
            go to 620

          else if (card .eq. '(') then
            call uscan (buf, word, nwrd, '/(', ',= ' // linefeed)
            tempc = cnvtwrd (word(1))
            if (tempc .eq. 'END') go to 660
            go to 620
C
C         MEANINGLESS CONTROL RECORD
C
          else if (card .ne. '>') then
            write (errbuf(1), 10140) buf(1:80)
10140       format('0 UNRECOGNIZABLE TEXT: (',a80,')')
            call prterx ('W',1)
            go to 620
          endif
C
C         PROCESS ">" CONTROL.
C
          call uscan (buf, word, nwrd, '#', ',= ' // linefeed)
c
c         Uppercase all words and remove any underscore characters
c         from all words except file names.
c
          iwrd = 1
          finished = .false.
          do while (iwrd .lt. nwrd .and. .not. finished)
            tempc = word(iwrd)
            word(iwrd) = cnvtwrd (tempc)
            if (word(iwrd)(1:7) .eq. 'INCLUDE' .or.
     &          word(iwrd)(1:10) .eq. 'COMMONMODE') then
              finished = .true.
            else
              iwrd = iwrd + 1
            endif
          enddo
C
C         SEARCH DICTIONARY
C
          do i = 1,lctl
            l = index(dict(i),'*')-1
            if (l .le. 0) l = len(dict(i))
            if (index(word(1), dict(i)(1:l)) .ne. 0) go to 210
          enddo
          write (errbuf(1)(1:120), 10150) word(1)(1:10),buf(1:70)
10150     format('UNIDENTIFIED KEYWORD (', a, ') IN TEXT (', a, ')')
          call prterx ('W',1)
          kerrsw=1
          go to 620
C
C                   1    2    3    4    5    6    7    8    9   10  
C                 ANA, RED, DEB, DEF, EXP, GEN, INC, LOW, MIN, NOS,
  210     go to  (220, 620, 240, 250, 620, 620, 340, 350, 620, 620,
C
C                  11   12   13   14   15   16   17   18    19   20   
c                 OUT,OUTP,OUTS,OVER, PHA, REA,REAL, RED, RELX,RELB, 
     &            410, 500, 510, 420, 620, 620, 620, 620, 570,  620, 
c                 TOL, OLD, COM, SET, SOL
c                  21   22   23   24   25
     &            620, 620, 620, 620, 620) i
C
C         > ANALYSIS <
C
  220     if (word(2) .eq. 'OFF') kase1(18) = 0
          if (word(2) .eq. 'ON') kase1(18) = 1
          if (nwrd .ge. 3) then
            if (word(3) .eq. 'MINLOADING') then
              case1(15) = rval (word(4))
              tpc = 0.01*case1(15)
             endif
          endif
          go to 620
C
C         > DEBUG C
C
  240     if (word(2) .eq. 'OFF') kase1(8) = 0
          if (word(2) .eq. 'ON') kase1(8) = 1
          go to 620
C
C         > DEFAULT RATINGS
C
  250     finished = .false.
          do while (.not. finished)
            read (inpfil, fmt='(a)', end=260) buf
            go to 280
  260       if (inpfil .eq. inpold) then
              buf = '( END ) OTEXT'
            else
              inpfil = inpold
              read (inpfil, fmt='(a)', end=270) buf
              go to 280
  270         buf = '( END ) OTEXT'
            endif
  280       continue
            write (outbuf, 10110) buf(1:80)
            call prtout(1)
            if (buf(1:2) .eq. ' ') then
              nrat = nrat+1
              if (nrat .gt. 50) then
                write (errbuf(1), 10160) buf(1:80)
10160           format ('MORE THAN 50 DEFAULT RATINGS (', a, ')')
                call prterx ('W',1)
                kerrsw=1
              else
               read (buf, 10170) zrat(1,nrat), zrat(2,nrat),
     &           zrat(3,nrat)
10170          format (bz,3x,f4.0,2x,f4.0,2x,f4.0)
              endif
            else
              finished = .true.
            endif
          enddo
          nrat = min0(nrat,50)
          call qiksrt (1,nrat,kmprat,swprat)
          go to 620
C
C         > INCLUDE_CON <
C
  340     inpold = inpfil
          inpfil = lunscr1
          ierror = 0
          call opnfil(inpfil, word(nwrd), ierror)
          if (ierror .ne. 0) inpfil = inpold
          go to 620
C
C         > LOW_VOLTAGE_SCREEN <
C
  350     case1(16) = rval (word(2))
          tps = 0.01*case1(16)
          go to 620
C
C         > OUTAGE <
C
  410     cntngn = .true.
          go to 430
C
C         > OVERLOAD <
 
  420     loadck = .true.
 
  430     vmin = 0.0
          vmax = 9999.0
          i = 2
          do while (i .le. nwrd)
            if (index(word(i)(1:4), 'ZONE') .ne. 0) then
              do j=i+1,nwrd
                if (index(word(j)(1:4), 'BASE') .ne. 0) then
                  i = j - 1
                  go to 490
                endif
                if (word(j) .eq. '00') word(j)=' '
                if (index(word(1), 'OVERLOAD') .ne. 0) then
                  no = no + 1
                  zno(no) = word(j)
                else
                  nc = nc + 1
                  znc(nc) = word(j)
                endif
              enddo
              i = nwrd 
            else if (index(word(i)(1:4), 'BASE') .ne. 0) then
              do j=i+1,nwrd,2
                if (index(word(j)(1:4), 'ZONE') .ne. 0) then
                  i = j - 1
                  go to 490
                endif
                vmin = rval (word(j))
                if (j+1 .gt. nwrd) go to 490
                if (index(word(j+1)(1:4), 'ZONE') .ne. 0) then
                  i = j 
                  go to 490
                endif
                vmax = rval (word(j+1))
                if (j .ge. nwrd) go to 490
              enddo
              i = nwrd
            else
              write (errbuf(1)(1:120), 10180) word(i)(1:10),buf(1:70)
10180         format('UNIDENTIFIED KEYWORD (',a,') IN TEXT (',a,')')
              call prterx ('W',1)
            endif
  490       i = i + 1
          enddo
          if (index(word(1), 'OUTAGE') .ne. 0) then
            vcl = amin1(vmin,vmax)
            vch = amax1(vmin,vmax)
          else
            vol = amin1(vmin,vmax)
            voh = amax1(vmin,vmax)
          endif
          go to 620
C
C         > OUTPUT_PLTDTA <
C
  500     if (word(2) .eq. 'OFF') kase1(25)=0
          if (word(2) .eq. 'ON') kase1(25)=1
          go to 620
C
C         > OUTPUT_SORT <
C
  510     do i = 2,nwrd
            if (word(i)(1:3) .eq. 'OUT') then
              kase1(3) = 0
            else if (word(i)(1:3) .eq. 'OVE') then
              kase1(3) = 1
            else if (word(i)(1:3) .eq. 'BOT') then
              kase1(3)=2
            else if (word(i)(1:3) .eq. 'OWN') then
              kase1(5) = 1
            else
              write (errbuf(1)(1:120), 10180) word(i)(1:10), buf(1:70)
              call prterx ('W',1)
            endif
          enddo
          go to 620
C
C         > RELAX_BR_RATE <
C
  570     if (word(2) .eq. 'ON') kase1(6)=0
          if (word(2) .eq. 'OFF')then
            kase1(6)=1
            ilinbc=1
          endif
          if (nwrd .gt. 2) then
            if (word(3) .eq. 'PERCENT') then
              case1(17) = rval (word(4))
              olinbc=0.01*case1(17)
            else
              write (errbuf(1)(1:120), 10180) word(3)(1:10), buf(1:70)
              call prterx ('W',1)
            endif
          endif
          go to 620

  620     continue
        enddo
  660   continue
        return
        end

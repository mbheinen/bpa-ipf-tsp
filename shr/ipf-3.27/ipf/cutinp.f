C    @(#)cutinp.f	20.12 8/30/00
        subroutine cutinp
C
        include 'ipfinc/parametr.inc'
        include 'ipfinc/cut.inc'
        include 'ipfinc/coment.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/cut2.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/com004.inc'

        character word(200)*30, capital*30, bigbuf*512, ljstfy*80, 
     &            tempc*10
        logical baseok, readnx, endrcd
C
C       ***  CONTROL : LEVEL 1 DICTIONARY ***
C
C       Input Commands                            INPTSW
C
C       > This space for rent <
C       > WSCC <
C       > DEBUG <
C       > SAVE_ZONES aa,bb, ... SAVE BASES ... <     1
C       > SAVE_BASES aaa.a,bbb.b, ... <              1
C       > SAVE_BUSES <                               2
C       > INCLUDE_BUSES <                            3
C       > EXCLUDE_BUSES <                            4
C       > PI_BACK_BUSES <                            5
C       > CUT_BRANCHES <                             6
C       > INCLUDE_COMMAND <
C
C       READNX:  .TRUE. -  "BUF" PROCESSED; READ NEXT CARD
C                .FALSE.-  "BUF" NOT PROCESSED; DO NOT READ NEXT CARD
        idat = 0
        idat2 = 0
        inptsw = 0
C
C       PROCESS INPUT TEXT
C
        if (init.eq.0) then
           init=1
           inpold=inp
           kerrsw=0
           readnx = .false.
           call space (2)
C
           write (outbuf,100) buf(1:80)
  100      format (' CUTTING COMMANDS: (',a,')')
           call prtout(1)
        endif
        go to 560
C
  110   card = buf(1:1)
C
C       COMMENT CARD
C
        if (card.eq.'.') then
           readnx=.true.
           go to 560
C
C          HEADER CARD
C
        else if (card.eq.'H') then
           outbuf = buf(2: )
           call comlod(2)
           readnx=.true.
           go to 560
C
C          COMMENT CARD
C
        else if (card.eq.'C' .and. buf(1:3) .ne. 'CUT') then
           if(ncom.gt.MAXCMT) then
              write (errbuf(1),120) MAXCMT,buf(1:80)
  120         format(' TOO MANY COMMENTS ..  MAX=',i5,1x,a,' RECORD ',
     1       'SKIPPED')
              call prterx ('W',1)
           else
              ncom = ncom + 1
              com(ncom) = buf(2:80)
           endif
           readnx=.true.
           go to 560
C
C          PROGRAM CONTROL CARD
C
        else if (card .eq. '(' .or. card .eq. '/') then
           if(index(buf,'CUTTING').eq.0) then
              go to 590
           else
              readnx=.true.
              go to 560
           endif
C
C          MEANINGLESS CONTROL RECORD
C
        else if (card .ne. '>') then
           write (errbuf(1),130) buf(1:80)
  130      format('0 UNRECOGNIZABLE TEXT: (',a80,')')
           call prterx ('W',1)
           readnx=.true.
           go to 560
        endif
C
C       PROCESS ">" CONTROL.
C
        bigbuf = buf
        endrcd = .false.
        readnx = .true.
        last = lastch (bigbuf)
        do while (bigbuf(last:last) .eq. '-') 
           read (inp, 132, end=134) buf   
  132      format (a) 
           call space (1) 
           write (outbuf,100) buf(1:80)   
           call prtout (1)
           if (buf(1:1) .eq. ' ') then
             bigbuf(last:) = ljstfy (buf)
           else if (buf(1:1) .ne. '>') then
             bigbuf(last:) = ljstfy (buf)
           endif
           last = lastch (bigbuf)
           buf = bigbuf   
        enddo
        go to 136
  134   if (inp .eq. inpold) then
           buf = '( END ) CUTINP'
           card = buf(1:1)
        else
           inp = inpold
           readnx = .true.
        endif
  136   continue
        
        call uscan (bigbuf, word, nwrd, '&', ' ,') 

C       CONVERT TO All capitals
 
        do 140 i=1,nwrd
           word(i)=capital (word(i))
  140   continue
 
C       SEARCH DICTIONARY
 
        do while (index (word(1), '_') .ne. 0)
           tempc = word(1)
           i = index (tempc, '_')
           word(1)(i:) = tempc(i+1:)
        enddo
        do i=1,lctl
           l = index(dict(i),'*')-1
           if (l.le.0) l=len(dict(i))
           if (index(word(1),dict(i)(1:l)).ne.0) go to 170
        enddo
 
        write (errbuf(1)(1:120),160) word(1)(1:10),buf(1:70)
  160   format('UNIDENTIFIED KEYWORD (',a,') IN TEXT (',a,')')
        call prterx ('W',1)
        kerrsw=1
        readnx = .true.
        go to 560
 
  170   continue
 
C                 1    2   3     4    5    6    7    8    9   10
C               xxx WSCC  DEB SZON SBAS SBUS IBUS EBUS PIBK CUTB
        go to  (200, 190, 180, 360, 360, 250, 260, 270, 280, 220,
 
C         11
C       INCC
     1   350) i
 
C       > DEBUG C
 
  180   kdebug = 1
        readnx=.true.
        go to 560
 
 
C       > WSCC
 
  190   nwscc = 1
        readnx=.true.
        go to 560
 
C
C       > For rent <
C
  200   continue
        readnx = .true.
        go to 560
C
C       > CUT_BRANCHES <
C
  220   inptsw = 6
  222   read (inp,300,end=242) buf
        card=buf(1:1)
        readnx=.false.
        write (outbuf,100) buf(1:80)
        call prtout(1)
        if (index ('LET',card) .eq. 0) go to 560
        if (idat+2 .gt. MAX_CUT_DATA-2) then
           write (errbuf(1),230) MAX_CUT_DATA/2
  230      format('More than ', i5, 
     &       ' "CUT_BRANCH" records. Remainder ignored.')
           write (errbuf(2),320) buf(1:80)
           call prterx ('F',2)
           kerrsw=1
           idat = 100
        else
           read (buf,240) odata(idat+1),busdt(idat+1),basedt(idat+1),
     1                    busdt(idat+2),basedt(idat+2)
  240      format(3x,a3,a8,f4.0,1x,a8,f4.0)
           idat = idat + 2
        endif
        go to 222
  242   if(inp.eq.inpold) then
           buf='( END ) CUTINP'
           card=buf(1:1)
        else
           inp=inpold
           readnx=.true.
        endif
        go to 560
C
C       >SAVE BUSES<
C
  250   inptsw=2
        go to 290
C
C       >INCLUDE BUSES<
C
  260   inptsw=3
        go to 290
C
C       >EXCLUDE BUSES<
C
  270   inptsw=4
        go to 290
C
C       >PI_BACK BUSES <
C
  280   inptsw = 5
C
C       DECODE BUSES
C
  290   read (inp,300,end=340) buf
  300   format(a)
        card=buf(1:1)
        readnx=.false.
        write (outbuf,100) buf(1:80)
        call prtout(1)
        if (card.ne.'B') go to 560
        idat=idat+1
        if (idat .gt. MAX_CUT_DATA) then
           write (errbuf(1),310) MAX_CUT_DATA
  310      format('More than ', i5, 
     &       ' Bus text records. Remainder ignored.')
           write (errbuf(2),320) buf(1:80)
  320      format(12x,'(',a,')')
           call prterx ('W',2)
           kerrsw=1
           idat=100
        else
           read (buf,330) busdt(idat),basedt(idat)
  330      format(6x,a8,f4.0)
        endif
        go to 290
C
  340   if(inp.eq.inpold) then
           buf='( END ) CUTINP'
           card=buf(1:1)
           go to 590
        else
           readnx=.true.
           inp=inpold
        endif
        go to 560
C
C       >INCLUDE_CON<
C
  350   inpold=inp
        inp=23
        call opnfil(inp,word(2),ierror)
        readnx=.true.
        go to 560
C
C       > SAVE ZONES ... ,SAVE BASES ... <
C       > SAVE BASES ... <
C
  360   inptsw = 1
        readnx = .true.

        i = 1
        do while (i .le. nwrd)
           if (i .le. nwrd .and. index(word(i),'ZONE') .ne. 0) then
              j = i + 1
              do while (j .le. nwrd)
                 if (index(word(j),'BASE') .eq. 0) then
                    idat=idat+1
                    if (idat .gt. MAX_ZONE_DATA) then
                       write (errbuf(1),410) MAX_ZONE_DATA
  410                  format('More than ', i4, 
     &                   ' SAVED ZONES submitted.')
                       call prterx ('W',1)
                       idat = MAX_ZONE_DATA
                    else
                       if (word(j)(3: ) .ne. ' ') then
                          last = lastch (word(j))
                          write (errbuf(1),420) word(j)(1:last)
  420                     format('0 zone "', a, '" has more than two cha
     &racters. Only first two characters accepted.')
                          call prterx ('W',1)
                       endif
                       zdata(idat)=word(j)(1:2)
                    endif
                    i = j + 1
                 else
                    i = j
                    j = nwrd 
                 endif
                 j = j + 1
              enddo
           else if (i .le. nwrd .and. index(word(i),'BASE') .eq. 0) then
              write (errbuf(1)(1:120),160) word(i)(1:10),buf(1:70)
              call prterx ('W',1)
           endif

           if (i .le. nwrd .and. index(word(i), 'BASE') .ne. 0) then
              do j = i+1, nwrd
                 idat2=idat2+1
                 if (idat2 .gt. MAX_CUT_DATA) then
                    write (errbuf(1),460) MAX_CUT_DATA
  460               format(' More than ', i5, 
     &                ' Saved Base Kv''s submitted.')
                    call prterx ('W',1)
                    idat=100
                 else
                    basedt(idat2)=rval(word(j))
                 endif
              enddo
              i = nwrd + 1
           else if (i .le. nwrd .and. index(word(i),'ZONE') .eq. 0) then
              write (errbuf(1)(1:120),160) word(i)(1:10),buf(1:70)
              call prterx ('W',1)
           endif
        enddo
C
C       CHECK FOR EXTRACTED DATA
C
        if (idat + idat2 .eq. 0) then
           write (errbuf(1),540) buf(1:80)
           call prterx ('W',1)
  540      format('0 No ZONE/BASE data extracted from text:(', a80,')')
        endif
        go to 560
C
C       READ NEXT RECORD
C
  560   if (inptsw.ne.0) go to 590
        if (readnx) then
           read (inp,300,end=580) buf
  570      format(a)
           write (outbuf,100) buf(1:80)
           call prtout(1)
           readnx=.false.
        endif
        go to 110
  580   if (inp.eq.inpold) then
           buf='( END ) CUTINP'
           card=buf(1:1)
        else
           inp=inpold
           readnx = .true.
           go to 560
        endif
  590   return
      end

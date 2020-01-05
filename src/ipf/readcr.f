C    @(#)readcr.f	20.4 2/13/96
      subroutine readcr 
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/com011.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/dflrat.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/red6.inc'

        common/init/init

        character word(100)*30, capital*30, bigbuf*512, comprs*512
        logical finished
C
C       INPTLS = 0 -  "BUF" PROCESSED. READ NEXT CARD
C                1 - "BUF" NOT PROCESSED. DO NOT READ NEXT CARD
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
           call space (2)
           write (outbuf,100) buf(1:80)
  100      format (' REDUCTION TEXT (',a,')')
           call prtout(1)
        endif
        go to 600
C
  110   card = buf(1:1)
C
C       COMMENT CARD
C
        if (card.eq.'.') then
           inptls=0
           go to 600
C
C          HEADER CARD
C
        else if (card.eq.'H') then
           outbuf = buf(2: )
           call comlod(2)
           inptls=0
           go to 600
C
C          COMMENT CARD
C
        else if (card.eq.'C') then
           if(ncom.gt.MAXCMT) then
              write (errbuf(1),120) MAXCMT
  120         format(' TOO MANY COMMENTS ..MAX=',i5,
     1               '. FOLLOWING RECORD SKIPPED')
              write (errbuf(2),122) buf(1:80)
  122         format(' (',a,')')
              call prterx ('W',2)
           else
              ncom = ncom + 1
              com(ncom) = buf(2:80)
           endif
           inptls=0
           go to 600
C
C          PROGRAM CONTROL CARD
C
        else if (index('/(',card).ne.0) then
           if(index(buf,'REDUCTION').eq.0) then
              go to 630
           else
              inptls=0
              go to 600
           endif
C
C          MEANINGLESS CONTROL RECORD
C
        else if (card.ne.'>') then
           write (errbuf(1),130) buf(1:80)
  130      format('0 UNRECOGNIZABLE TEXT: (',a80,')')
           call prterx ('W',1)
           inptls=0
           go to 600
        endif
C
C       PROCESS ">" CONTROL.
C
        call scan(buf,word,nwrd)
 
C       CONVERT TO All capitals
 
        do 140 i=1,nwrd
           word(i)=capital (word(i))
  140   continue
 
C       SEARCH DICTIONARY
 
        do 150 i=1,lctl
           l = index(dict(i),'*')-1
           if (l.le.0) l=len(dict(i))
           if (index(word(1),dict(i)(1:l)).ne.0) go to 160
  150   continue
 
        write (errbuf(1)(1:120),200) word(1)(1:10),buf(1:70)
        call prterx ('W',1)
        kerrsw=1
        inptls = 0
        go to 600
 
  160   continue
C                 1   2   3   4    5    6   7   8   9  10
C               DEB,ELI,EXC,INCB,INCC,KEE,MIN,OPT,RET,REI
        go to  (170,180,270,260, 360, 370,380,390,400,580,
 
C                11    12   13   14  15   16   17   18   19
C               SAVB,SAVB,SAVZ,ULT, COH, SAVA ENVE STA  CHA
     1          250, 410, 410, 230, 280, 352, 162, 164, 166) i                              

C       > ENVELOPE BUSES = BE

  162   kase1(36) = 0
        if (word(2).eq.'BE') kase1(36)=1
        inptls=0
        go to 600

C       > STARTING_VOLTAGES = FLAT
C                             HOT
  164   kase1(35) = 0
        if (word(2).eq.'HOT') kase1(35)=1
        inptls=0
        go to 600

C       > CHANGE_BUS_TYPES, B = BE, BQ = BE, BX = BE, BG = BE
C
C       Check for and concatenate continuation records.
C       
  166   bigbuf = comprs (buf)  
        finished = .false.
        do while (.not. finished)
           last = lastch (bigbuf) 
           if (bigbuf(last:last) .eq. '-') then   
              read (inp, fmt='(a)', end=167) buf
              call space (1)  
              write (outbuf,100) buf(1:80)
              call prtout (1) 
              bigbuf(last:) = comprs(buf) 
           else
              finished = .true.
           endif
        enddo
  167   call chge_type (bigbuf)
        inptls=0
        go to 600
                                                                                
C       > DEBUG C
 
  170   chase1(33) = word(2)(1:1)
        if (word(2).eq.'OFF') kase1(33)=0
        if (word(2).eq.'NONE') kase1(33)=0
        if (word(2).eq.'MINOR') kase1(33)=1
        if (word(2).eq.'MAJOR') kase1(33)=2
        if (word(2).eq.'ORDERING') kase1(33)=3
        inptls=0
        go to 600
 
C       > ELIM_NODE <
 
  180   j = 0
  190   j = j + 2
        if (j.gt.nwrd) go to 220
        if (word(j).eq.'GEN') then
           if (word(j+1).eq.'CURRENT') then
              kase1(2)=0
           else if (word(j+1).eq.'ADMITT') then
              kase1(2)=1
           else if (word(j+1).eq.'REI') then
              kase1(2)=2
              if (word(j+2).eq.'PMIN') then
                chase1(35) = word(j+3)
                j = j + 2
              endif
           else
              write (errbuf(1)(1:120),200) word(j+1)(1:10),buf(1:70)
  200         format('UNIDENTIFIED KEYWORD(',a,') IN TEXT(',a,')')
              call prterx ('W',1)
           endif
           write (chase1(2),202) kase1(2)
  202      format(i1)
        else if (word(j).eq.'LOAD') then
           if (word(j+1).eq.'CURRENT') then
              kase1(4)=0
           else if (word(j+1).eq.'ADMITT') then
              kase1(4)=1
           else if (word(j+1).eq.'REI') then
              kase1(4)=2
           else
              write (errbuf(1)(1:120),200) word(j+1)(1:10),buf(1:70)
              call prterx ('W',1)
           endif
           write (chase1(4),202) kase1(4)
        else if (word(j).eq.'SHUNTY') then
           if (word(j+1).eq.'CURRENT') then
              kase1(6)=0
           else if (word(j+1).eq.'ADMITT') then
              kase1(6)=1
           else if (word(j+1).eq.'REI') then
              kase1(6)=2
           else
              write (errbuf(1)(1:120),200) word(j+1)(1:10),buf(1:70)
              call prterx ('W',1)
           endif
           write (chase1(6),202) kase1(6)
        else
           write (errbuf(1)(1:120),200) word(j)(1:10),buf(1:70)
           call prterx ('W',1)
        endif
  210   continue
        go to 190
  220   continue
        inptls=0
        go to 600
C
C       > ULTI_MODE <
C
  230   i = 2
        do 240 j=i,nwrd,2
        if (word(j).eq.'GEN') then
           if(word(j+1).eq.'POWER')then
              kase1(3)=0
           else if (word(j+1).eq.'CURRENT') then
              kase1(3)=1
           else if (word(j+1).eq.'ADMITT') then
              kase1(3)=2
           else
              write (errbuf(1)(1:120),200) word(j+1)(1:10),buf(1:70)
              call prterx ('W',1)
           endif
           write (chase1(3),202) kase1(3)
        else if (word(j).eq.'LOAD') then
           if(word(j+1).eq.'POWER')then
              kase1(5)=0
           else if (word(j+1).eq.'CURRENT') then
              kase1(5)=1
           else if (word(j+1).eq.'ADMITT') then
              kase1(5)=2
           else
              write (errbuf(1)(1:120),200) word(j+1)(1:10),buf(1:70)
              call prterx ('W',1)
           endif
           write (chase1(5),202) kase1(5)
        else if (word(j).eq.'SHUNTY') then
           if(word(j+1).eq.'POWER')then
              kase1(7)=0
           else if (word(j+1).eq.'CURRENT') then
              kase1(7)=1
           else if (word(j+1).eq.'ADMITT') then
              kase1(7)=2
           else
              write (errbuf(1)(1:120),200) word(j+1)(1:10),buf(1:70)
              call prterx ('W',1)
           endif
           write (chase1(7),202) kase1(7)
        else
           write (errbuf(1)(1:120),200) word(j)(1:10),buf(1:70)
           call prterx ('W',1)
        endif
  240   continue
        inptls=0
        go to 600
C
C       > SAVE BUSES <
C
  250   inptsw = 2
        go to 290
C
C       > INCLUDE BUSES <
C
  260   inptsw = 3
        go to 290
C
C       > EXCLUDE BUSES <
C
  270   inptsw = 4
        go to 290
C
C       > COHERENCY <
C
  280   inptsw = 5
        idat = 1
        busdt(idat) = word(2)
        basedt(idat) = rval (word(3))
C
C       Decode type "B" bus data
C
  290   read (inp,300,end=350) buf
  300   format(a)
        card=buf(1:1)
        inptls=1
        write (outbuf,100) buf(1:80)
        call prtout(1)
        if (card.ne.'B') go to 600
        idat=idat+1
        if (idat.gt.MAX_RECORD) then
           write (errbuf(1),310) MAX_RECORD
  310      format('MORE THAN ', i4, ' BUS TEXT RECORDS. REMAINDER',
     1    ' IGNORED.')
           write (errbuf(1),320) buf(1:80)
  320      format(12x,'(',a,')')
           call prterx ('W',2)
           kerrsw=1
           idat=MAX_RECORD
        else if (inptsw.ne.5) then
           read (buf,330) busdt(idat),basedt(idat)
  330      format(bz, 6x, a8, f4.0)
        else
           read (buf,340) zdata(idat),busdt(idat),basedt(idat)
  340      format(bz, 3x, a2, 1x, a8, f4.0)
        endif
        go to 290
C
  350   if(inp.eq.inpold) then
           buf='( END ) READCR'
           card=buf(1:1)
           go to 630
        else
           inptls=0
           inp=inpold
        endif
        go to 600
C
C       > SAVE AREAS <
C
  352   inptsw = 6
        sang = 1.0
        write (chase1(34),353) sang
  353   format (f10.5)
  354   read (inp,355,end=350) buf
  355   format(a)
        card=buf(1:1)
        inptls=1
        write (outbuf,100) buf(1:80)
        call prtout(1)
        if (card.ne.'A') go to 600
        idat=idat+1
        if (idat.gt.MAX_RECORD) then
           write (errbuf(1),356) MAX_RECORD
  356      format('MORE THAN ', i4, ' AREA TEXT RECORDS. REMAINDER',
     1    ' IGNORED.')
           write (errbuf(1),320) buf(1:80)
           call prterx ('W',2)
           kerrsw=1
           idat=MAX_RECORD
        endif
        adata(idat) = buf(4:13)
        go to 354
C
C       > INCLUDE_CON <
C
  360   inpold=inp
        inp=23
        call opnfil(inp,word(2),ierror)
        inptls=0
        go to 600
C
C       > KEEP_AI_SYSTEM <
C
  370   if (word(2) .eq. 'ON') then
           kase1(20) = 1
           write (chase1(20),202) kase1(20)
        else if (word(2) .eq. 'OFF') then
           kase1(20) = 0
           chase1(20) = '0'
        endif
        inptls=0
        go to 600
C
C       > MIN_EQUIVALENT)Y <
C
  380   case1(1) = rval (word(2))
        chase1(1)=word(2)
        inptls=0
        go to 600
C
C       > OPTIMAL_REDUC <
C
  390   if(word(2).eq.'ON') then
           kase1(24)=1
           chase1(24)='1'
        else
           kase1(24)=0
           chase1(24)='0'
        endif
        inptls=0
        go to 600
C
C       > RETAIN_GEN <
C
  400   if(word(2).eq.'OFF')then
           chase1(11)=' '
           kase1(11)=0
        else
           chase1(11)='1'
           kase1(11)=1
        endif
        if (word(3).eq.'PMIN') then
           chase1(35) = word(4)
        else if (nwrd.ge.4) then
           write (errbuf(1),200) word(3)(1:10),buf(1:70)
           call prterx ('W',1)
        endif
        inptls=0
        go to 600
C
C       > SAVE ZONES ...,SAVE BASES ... <
C
  410   inptsw = 1
        inptls = 0
C
C       CHECK FOR CONTINUATION RECORDS (ONE MAXIMUM,
C       "," IN COLUMN 1)
C
        if (index(buf,'<').eq.0) then
           read (inp,610,end=414) buf
           write (outbuf,100) buf(1:80)
           call prtout(1)
           if (buf(1:1).eq.'+') then
              call scan(buf(2:),word(nwrd+1),jwrd)
              do 412 i = 1,jwrd
  412         word(i+nwrd) = capital (word(i+nwrd))
              nwrd = nwrd + jwrd
           else
              inptls=1
           endif
        endif
        go to 416
  414   if(inp.eq.inpold) then
           buf='( END ) READCR'
           card=buf(1:1)
        else
           inp=inpold
           inptls=0
        endif
  416   i=0
  420   i=i+1
        if (i.gt.nwrd) go to 550
        if (index(word(i),'ZONE').ne.0) then
           do 460 j=i+1,nwrd
           if (index(word(j),'BASE').ne.0) go to 470
           idat=idat+1
           if(idat.gt.MAX_RECORD) then
              write (errbuf(1),430) MAX_RECORD
  430         format('MORE THAN ', i4, ' SAVED ZONES SUBMITTED.')
              call prterx ('W',1)
              idat=MAX_RECORD
           else
              if (word(j)(3: ).eq.' ') go to 450
              write (errbuf(1),440) word(j)
  440         format('0 ZONE"',a10,'" HAS MORE THAN TWO CHARACTERS.',
     1        ' ONLY FIRST TWO CHARACTERS ACCEPTED.')
              call prterx ('W',1)
  450         zdata(idat)=word(j)(1:2)
           endif
  460      continue
           go to 550
  470      i=j-1
           go to 420
        else if(index(word(i),'BASE').ne.0) then
           do 490 j=i+1,nwrd
           idat2=idat2+1
           if(idat2.gt.MAX_RECORD) then
              write (errbuf(1),480) MAX_RECORD
  480         format('MORE THAN ', i4, ' SAVED BASE KV''S SUBMITTED.')
              call prterx ('W',1)
              idat2=MAX_RECORD
           else
              basedt(idat2)=rval(word(j))
           endif
  490      continue
           go to 550
        else
           write (errbuf(1)(1:120),200) word(i)(1:10),buf(1:70)
           call prterx ('W',1)
        endif
C
C       CHECK FOR EXTRACTED DATA
C
  550   if(idat+idat2.gt.0) go to 570
        write (errbuf(1),560) buf(1:80)
        call prterx ('W',1)
  560   format('0 NO ZONE/BASE DATA EXTRACTED FROM TEXT:(',a80,')')
  570   continue
        go to 600
C
C       > REI_CLUSTERS <
C
  580   do 590 i=2,nwrd-1,2
        if(index(word(i),'VOLT').ne.0) then
           chase1(13)=word(i+1)
           case1(13)=rval(word(i+1))
        else if (index(word(i),'ANGLE').ne.0) then
           chase1(15)=word(i+1)
           case1(15)=rval(word(i+1))
        else
           write (errbuf(1)(1:120),200) word(i)(1:10),buf(1:70)
           call prterx ('W',1)
        endif
  590   continue
        inptls=0
        go to 600
C
C       READ NEXT RECORD
C
  600   if (inptsw.ne.0) go to 630
        if (inptls.eq.0) then
           read (inp,610,end=620) buf
  610      format(a)
           write (outbuf,100) buf(1:80)
           call prtout(1)
           inptls=1
        endif
        go to 110
  620   if (inp.eq.inpold) then
           buf='( END ) READCR'
           card=buf(1:1)
        else
           inp=inpold
           inptls = 0
           go to 600
        endif
  630   return
        end

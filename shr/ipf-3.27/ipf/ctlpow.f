C    @(#)ctlpow.f	20.14 2/28/00
      subroutine ctlpow
 
C     Read '/' control records for powerflow..
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/com002.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/pfdata.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/zonlst.inc'
      include 'ipfinc/usranl.inc'
 
      common /is_batch / is_batch

      common /dependency/ dependency_flag
      integer dependency_flag

        character jobnme*10, word(100)*60, temfil*60, sortdt(4)*19, 
     &            bigbuf*512, tempc*120, ljstfy*120, cnvtwrd*60, 
     &            capital*60, type*10, filename*60, carriagecontrol*1
        integer ctyp, teminp, status, scrfil, open_file, firstxstr, 
     &          save_type, findstr, error, chkerr
        logical rdnext, mislvl, done, eof, com_last, opened

        equivalence (jobnme, jobreq(1))

        data sortdt / 'OWN*', 'BUS*', 'ZONE*', 'AREA' /
 
        mislvl = .true.
        rdnext = .true.
        eof = .false.
        com_last = .false.
        carriagecontrol = char(13)
 
   10   lastrcd = index (inrcd, carriagecontrol)
        if (lastrcd .gt. 0) inrcd(lastrcd:) = ' '
        ctyp = findstr('HC/(.',inrcd(1:1))
 
        if ( ctyp .eq. 0 ) then
           if (com_last) then
              rdnext = .true.
           else
C                                      DATA RCD, RETURN *****
              rdnext = .false.
              jobreq(1) = 'DATA'
           endif
        else if (ctyp .eq. 4) then
C                                       '(' CONTROL
           rdnext = .false.
           jobreq(1) = cnvtwrd (word(1))
           com_last = .false.
        else if ( ctyp .eq. 5 ) then
C                                      "." COMMENT  ****
           rdnext = .true.
           mislvl = .false.
           com_last = .false.
        else if ( ctyp .eq. 1 ) then
C                                      HEADER COMMENT  ****
           outbuf = inrcd(2:)
           call comlod(2)
           rdnext = .true.
           mislvl = .false.
           com_last = .false.
        else if ( ctyp .eq. 2 ) then
C                                      'C' User case comment....
           rdnext = .true.
           mislvl = .false.
           com_last = .false.
           if (ncom .lt. MAXCMT) then
              ncom = ncom + 1
              com(ncom) = inrcd(2:80)
           else
              write (errbuf(1),12) MAXCMT
   12         format(' TOO MANY COMMENTS ..MAX=',i5,
     1               '. FOLLOWING RECORD SKIPPED')
              write (errbuf(2),14) inrcd(1:80)
   14         format(' (',a,')')
              call prterx ('W',2)
           endif
 
        else if (ctyp .eq. 3 ) then
C                              '/' SCANABLE CONTROL ******
           com_last = .false.
           bigbuf = inrcd
C
C          Check for and concatenate continuation records. Maximum
C          is 120.
C
           last = lastch (bigbuf)
           do while (bigbuf(last:last) .eq. '-') 
              read (inp,135,end=106) tempc
  135         format(a)
              lastrcd = index (inrcd, carriagecontrol)
              if (lastrcd .gt. 0) inrcd(lastrcd:) = ' '
              tempc = ljstfy (tempc)
              bigbuf(last:) = ' ' // tempc
              last = lastch (bigbuf)
              inrcd = bigbuf
           enddo
           go to 108
  106      eof = .true.
  108      continue
           call uscan(bigbuf(2:), word, nwrd, '|', ', =')
           if (nwrd .gt. 100) then
              write (errbuf(1), 10109) bigbuf(2:62)
10109         format('Overflow scanning text: (', a,')')
              write (errbuf(2), 10110) nwrd, 100
10110         format('Actual/limit is ', i2, '/', i2, 
     &           ' Some data may be lost')
              call prterx ('W',2)
           endif 
           iwrd = 0
  110      iwrd = iwrd + 1

           jobsw = ickdic(word(iwrd),ctldic,lctl)
           if (jobsw .gt. 0) goto 150
 
           if ( iwrd .lt. nwrd ) goto 110
 
           if (jobreq(1) .eq. 'POWER_FLOW') then
              write (errbuf(1),130) inrcd(1:80)
  130         format('0 UNRECOGNIZABLE TEXT: (',a80,')')
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
           else
              rdnext = .false.
           endif
 
C          READ NEXT RECORD....
           go to 2000
 
  150      continue
           if (nwrd .eq. 1) mislvl = .false.
 
C                  1   2   3   4   5   6    7    8   9   10  11
C                 OLB,NWB,NWK,PIN,FIN,POUT,FOUT,PAN,FAN,AIL,OIL,
           go to (1300,300,400,500,540,600,640,700,740,850,900,
 
C          12   13   14   15   16   17  18  19  20  21  22   23
C         DBUG,ERRS,VLIM,LTC, SOL, TOL,AIC,BRD,RPT,MVA,OVLD,TRA,
     &   1850,1100,1200,450,1800,1830,800,200,750,650,350,1000,
 
C          24  25   26   27   28   29   30   31   32   33   34
C         REB,LIM, CTRL,END, STOP,DATA,HEAD,COMM,CHAN,INCL,NEXT
     &   250,1840,1860,1900,1900,1900,1700,1710,1870,1400,1500,
 
C         35   36   37   38   39   40  41  42  43  44  45   46
C        MRGE,REDU,OUTA,BUSD,SOLN,LINE,TX,CHE,SENN,LINS,EXTR,SORT,
     &   1910,1960,1980, 260,1990,360,365,420,1992,1992,1994,2100,
 
C         47  48   49   50   51   52    53    54     55    56
C        LOSS,GEND,HOTC,ANAL,USRA,%LOAD,EPRI,AGC,  SAVE, LOAD LOAD
     &   1992,1992,1862, 680, 672,10862, 170,1992,10300,10170,10180,

C         57    58
C        INIT,  CASED
     &   10190, 10200) jobsw
 
  170   continue
C
C       / EPRI_1964, FILE = <filespec>            : JOBSW = 53
C
        call getepr
C
C       DECIPHER NEXT RECORD....
C
        mislvl = .true.
        rdnext = .true.
        goto 10

10170   continue
C
C       / LOAD_PTI, FILE = <filespec>            : JOBSW = 55
C
        jobreq(4) = 'LOAD_PTI'

        rdnext = .false.
        mislvl = .false.
        goto 2000

10180   continue
C
C       / LOAD_GE, FILE = <filespec>            : JOBSW = 56
C
        jobreq(4) = 'LOAD_GE'

        rdnext = .false.
        mislvl = .false.
        goto 2000
C
C       / INIT_DEF                              : JOBSW = 57
C
10190   numusr = 0
        numdef(1) = 0
        numtxt(1) = 0
        usrdbg(1) = 0
        usrfil(1) = ' '
        numdef(2) = 0
        numtxt(2) = 0
        usrdbg(2) = 0
        usrfil(2) = ' '

        lunusr = 25
        inquire (unit=lunusr, opened=opened)
        if (.not. opened) then
C
C       Note: VAX/VMS limits sequential record size to 8191 words.
C       This restriction require the arrays to be written in
C       200-record segments.
C
        status = open_file(lunusr, ' ', 'U', 'W', ios)
        if (status .ne. 0) then
           write (errbuf(1), 104) ios, lunusr
  104      format (' Error No. ', i3, 
     1        ' opening scratch file on logical unit',i3)
           call prterx ('W',1)
           endif
        else
           rewind lunusr
        endif
 
C       READ NEXT RECORD....
        go to 2000
C
C       CASE_DEPENDENCY = NO  (default)
C                         YES
C                                                 : JOBSW = 59
10200   rdnext = .true.
        iwrd = iwrd + 1
        if (word(iwrd) .eq. '=') iwrd = iwrd + 1
        if (word(iwrd)(1:1) .eq. 'Y' .or.
     &      word(iwrd)(1:1) .eq. 'y') then
          dependency_flag = 1
          iercnt  = chkerr('F') + chkerr('A')
          if (iercnt .gt. 0) then
C
C           Skip current case
C
            last = min0 (60, lastch(inrcd))
            write (errbuf(1), 10202) chase1(1), inrcd(1:last)
10202          format (' Curent case (', a, 
     &') is skipped by /CASE_DEPENDENCY option and fatal errors')

            do while (inrcd(1:1) .ne. '(' .and. inrcd(1:1) .ne. '[')
               write (errbuf(1), 10204) inrcd(1:last)
10204          format (' Skipping command (', a, ')')
               call prterx ('W', 1)
               card = inrcd(1:1)
C
C              Examine next input record
C
               read (inp, fmt='(a)', end=10206) inrcd
               card = inrcd(1:1)
            enddo

            go to 10208
 
10206       inrcd = '(STOP)'
            card = inrcd(1:1)

10208       rdnext = .false.

          endif
        else
          dependency_flag = 0
        endif 

C       READ NEXT RECORD....

        mislvl = .false.
        goto 2000
C
C       BRANCH_DATA, FILE = <filespec>, DATE = myy
C                                                 : JOBSW = 19
 200    iwrd = iwrd + 1
        do while (iwrd .lt. nwrd ) 
           if (firstxstr (word(iwrd),'FILE') .gt. 0) then
              iwrd = iwrd + 1
              brdnam = word(iwrd)
              mislvl = .false.
 
           else if (firstxstr(word(iwrd),'DATE') .gt. 0 ) then
              iwrd = iwrd + 1
              read (word(iwrd),220) crun1(1),krun1(2)
  220         format( a1,i2 )
              crun1(2) = word(iwrd)(2:3)
              jobreq(3) = 'BUILD_BASE'
              mislvl = .false.
           endif
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD....
        goto 2000
 
C       / BUS_DATA, FILE = <filespec> 
 
  260   continue
        iwrd = iwrd + 1
        if (iwrd .lt. nwrd) then
           if (firstxstr(word(iwrd), 'FILE') .gt. 0) then
              iwrd = iwrd + 1
              bsdnam = word(iwrd)
              mislvl = .false.
           endif
        endif
 
C       READ NEXT RECORD...
        go to 2000
 
C      / REBUILD = OFF 
C                   ON                            : JOBSW = 24
 
250     continue
        iwrd=iwrd+1
 
        if (firstxstr(word(iwrd),'OFF') .gt. 0) then
C                                             ** OFF **
           kspare(1) = 0
           mislvl = .false.
 
        else if (firstxstr(word(iwrd),'ON') .gt. 0) then
C                                             ** ON **
           mislvl = .false.
           kspare(1) = 1
        endif
 
C       READ NEXT RECORD...
        goto 2000
 
C       / NEW_BASE,   FILE = [dir]filename.typ      : JOBSW = 2
 
  300   continue
 
        iwrd = iwrd + 1
        if ( iwrd .lt. nwrd ) then
           if (firstxstr(word(iwrd),'FILE') .gt. 0) then
              iwrd = iwrd + 1
              mislvl = .false.
              nbasnm = word(iwrd)
              last = lastch(nbasnm)
              status = open_file (datao, nbasnm(1:last), 'U', 'W', 
     &                            iostat)
              kspare(14) = 0
              if (status .eq. 1) then
                 write (errbuf(1), 301) nbasnm(1:last)
  301            format(' file ', a, 
     &              ' has write protection--request ignored')
                 if (is_batch .eq. 0) then
                    call prterx ('W',1)
                 else
                    call prterx ('F',1)
                 endif
              else if (status .eq. 2) then
                 write (errbuf(1), 302) nbasnm(1:last)
  302            format(' file ', a, 
     &              ' could not be opened--request ignored')
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
              else if (status .gt. 0) then
                 write (errbuf(1), 303) nbasnm(1:last), iostat
  303            format(' failure on opening file ', a,
     &              ' error code =', i2)
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
              else
                 kspare(14) = 1
                 rewind datao
                 write (datao) pfdata, end, dte, ist, ist,
     1                         prgvsn,usrnam,count
c*****            backspace datao     ! Invalid for UnixWare
                 rewind datao
              endif
           endif
        endif
 
C       READ NEXT RECORD....
        go to 2000

C       / SAVE_FILE, TYPE = WSCC_BINARY, FILE = filename : JOBSW = 55
C       / SAVE_FILE, TYPE = WSCC_ASCII,  FILE = filename
C       / SAVE_FILE, TYPE = CHANGES,  FILE = filename 
C       / SAVE_FILE, TYPE = NEW_BASE,  FILE = filename 
C                    CASE = casename
C       / SAVE_FILE, TYPE = NETWORK_DATA, FILE = filename 
C                    SIZE = 120, RATINGS = EXTENDED,
C                    DIALECT = BPA

10300   iwrd = iwrd + 1
        done = .false.
        save_type = 0
        do while (iwrd .lt. nwrd .and. .not. done)
           word(iwrd) = capital(word(iwrd))
           if (word(iwrd) .eq. 'TYPE') then
              iwrd = iwrd + 1
              if (iwrd .lt. nwrd) then
                 word(iwrd) = capital(word(iwrd))
                 if (word(iwrd)(1:4) .eq. 'WSCC') then
                    type = word(iwrd)
                    save_type = 1
c                 else if (word(iwrd)(1:7) .eq. 'CHANGES') then
c                    type = word(iwrd)
c                    save_type = 1
c                 else if (word(iwrd)(1:7) .eq. 'NETWORK') then
c                    type = word(iwrd)
c                    save_type = 1
                 else if (word(iwrd)(1:8) .eq. 'NEW_BASE') then
                    type = word(iwrd)
                    save_type = 1
                 else
                    write (errbuf(1), 10310) word(iwrd)
10310               format(' Unrecognized keyword (', a, ')')
                    write (errbuf(2), 10320) inrcd(1:60)
10320               format (' (', a, ')')
                    call prterx ('W', 2)
                 endif
              else
                 write (errbuf(1), 10330)
10330            format(' Incomplete /SAVE_FILE command ')
                 write (errbuf(2), 10320) inrcd(1:60)
                 call prterx ('W', 2)
                 done = .true.
              endif
           else if (word(iwrd) .eq. 'FILE') then
              iwrd = iwrd + 1
              if (iwrd .le. nwrd) then
                 if (save_type .eq. 1) then
                    save_type = 2
                    filename = word(iwrd)
                    done = .true.
                 else
                    write (errbuf(1), 10310) word(iwrd)
                    write (errbuf(2), 10320) inrcd(1:60)
                    call prterx ('W', 2)
                 endif
              else
                 write (errbuf(1), 10330)
                 write (errbuf(2), 10320) inrcd(1:60)
                 call prterx ('W', 2)
              endif
           else
              write (errbuf(1), 10310) word(iwrd)
              write (errbuf(2), 10320) inrcd(1:60)
              call prterx ('W', 2)
              done = .true.
           endif
           iwrd = iwrd + 1
        enddo
        if (save_type .eq. 2) then
           mislvl = .false.
           if (type(1:6) .eq. 'WSCC_A') then
              wscc_aname = filename
           else if (type(1:6) .eq. 'WSCC_B') then
              wscc_bname = filename
           else if (type(1:8) .eq. 'NEW_BASE') then
              mislvl = .false.
              scrfil = datao
              nbasnm = filename
              last = lastch(nbasnm)
              status = open_file (datao, nbasnm(1:last), 'U', 'W', 
     &                            iostat)
              if (status .eq. 0) then
                 rewind datao
                 write (datao) pfdata, end, dte, ist, ist,
     &                         prgvsn,usrnam,count
c*****           backspace datao     ! Invalid for UnixWare
                 rewind datao
                 kspare(14) = 1
              else
                 kspare(14) = 0
                 go to 10340
              endif
           endif
           go to 10380

10340      if (status .eq. 1) then
              last = lastch(filename)
              write (errbuf(1), 10341) filename(1:last)
10341         format(' file ', a, 
     &           ' has write protection--request ignored')
              if (is_batch .eq. 0) then
                 call prterx ('W',1)
              else
                 call prterx ('F',1)
              endif
           else if (status .eq. 2) then
              last = lastch(filename)
              write (errbuf(1), 10342) filename(1:last)
10342         format(' file ', a, 
     &           ' could not be opened--request ignored')
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
           else if (status .gt. 0) then
              last = lastch(filename)
              write (errbuf(1), 10343) filename(1:last), iostat
10343         format(' failure on opening file ', a,
     &           ' error code =', i2)
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
           endif
10380      continue

        else
           mislvl = .true.
        endif
 
C       READ NEXT RECORD....
        go to 2000
 
C       / OVERLD_RPT     / TX = nn / LINE = nn   : JOBSW = 22
 
350     iwrd=iwrd+1
        do while (iwrd .lt. nwrd) 
           word(iwrd) = cnvtwrd(word(iwrd))
           if (word(iwrd) .eq. 'TX') then
              mislvl = .false.
              iwrd=iwrd+1
              spare(31)=rval(word(iwrd))
           else if (word(iwrd) .eq. 'LINE') then
              mislvl = .false.
              iwrd=iwrd+1
              spare(30)=rval(word(iwrd))
           endif
           iwrd=iwrd+1
        enddo
 
C       READ NEXT RECORD...
        goto 2000
 
C       / LINE_EFF,LOADING=nn,OWNERS=ABC,DEF,XXX   :JOBSW = 40
 
  360   continue
        iwrd = iwrd+1
        do while (iwrd .lt. nwrd)
           if (firstxstr(word(iwrd),'LOAD*') .gt. 0) then
              mislvl = .false.
              iwrd = iwrd+1
              spare(29)=rval(word(iwrd))
           else if (firstxstr(word(iwrd),'OWN*') .gt. 0) then
              mislvl = .false.
              iwrd = iwrd + 1
              do while (iwrd .le. nwrd .and. 
     &           (firstxstr(word(iwrd),'LOAD*') .gt. 0))
                 leffan = leffan + 1
                 effan(leffan)=word(iwrd)
                 iwrd = iwrd + 1
              enddo
           endif
           iwrd = iwrd+1
        enddo

C       READ NEXT RECORD...
        go to 2000
 
C       / TX_EFF,TOTAL_LOSS=nn,CORE_LOSS=nn,OWNERS=ABC,.. :JOBSW=41
 
  365   continue
        iwrd = iwrd + 1
        do while (iwrd .lt. nwrd) 
           if (ickdic(word(iwrd),effdic,numtxf) .eq. 1) then
              mislvl = .false.
              iwrd = iwrd+1
              spare(27) = rval(word(iwrd))
           else if (ickdic(word(iwrd),effdic,numtxf) .eq. 2) then
              mislvl = .false.
              iwrd = iwrd+1
              spare(28) = rval(word(iwrd))
           else if (ickdic(word(iwrd),effdic,numtxf) .eq. 3) then
              mislvl = .false.
              iwrd = iwrd+1
              do while (iwrd .le. nwrd .and.
     &                  ickdic(word(i),effdic,numtxf) .eq. 0)
                 txefan = txefan+1
                 xefan(txefan)=word(iwrd)
                 iwrd = iwrd + 1
              enddo
           endif
           iwrd = iwrd+1
        enddo
 
C       READ NEXT RECORD
        go to 2000
 
C       / NETWORK_DATA, FILE = [dir]filename.typ, RX_CHECK = ON,OFF
C                                                 : JOBSW = 3
  400   iwrd = iwrd + 1
        bsbrnm  = ' '
        do while ( iwrd .lt. nwrd ) 
           if (firstxstr(word(iwrd),'FILE') .gt. 0) then
              mislvl = .false.
              iwrd=iwrd+1
              if (word(iwrd) .ne. '*') bsbrnm  = word(iwrd)
           else if (firstxstr(word(iwrd),'RXCHECK') .gt. 0) then
              mislvl = .false.
              iwrd=iwrd+1
              word(iwrd) = cnvtwrd(word(iwrd))
              if (word(iwrd) .eq. 'ON') then
                 kspare(18) = 1
              else if (word(iwrd) .eq. 'OFF') then
                 kspare(18) = 0
              endif
           endif
           iwrd=iwrd+1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       / CHECK, RX_CHECK = ON,OFF
C                                    : JOBSW = 42
  420   iwrd = iwrd + 1
        do while ( iwrd .lt. nwrd ) 
           if (firstxstr(word(iwrd),'RXCHECK') .gt. 0) then
              mislvl = .false.
              iwrd=iwrd+1
              word(iwrd) = cnvtwrd(word(iwrd))
              if (word(iwrd) .eq. 'ON') then
                 kspare(18) = 1
              else if (word(iwrd) .eq. 'OFF') then
                 kspare(18) = 0
              endif
           endif
           iwrd=iwrd+1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       / LTC = OFF                               : JOBSW = 15
C               ON     
C               ON_NV  
C               ON_NPS  
C               ON_DCONLY 
 
  450   iwrd=iwrd+1
        do while (iwrd .le. nwrd)
           word(iwrd) = cnvtwrd(word(iwrd))
           if (word(iwrd) .eq. 'OFF') then
C                                              ** OFF **
              mislvl = .false.
              iopton(16) = 0
           else if (word(iwrd) .eq. 'ON') then
C                                              ** ON **
              mislvl = .false.
              iopton(16) = 2
           else if (word(iwrd) .eq. 'ONNV' ) then
C                                              ** ON_NV **
              mislvl = .false.
              iopton(16) = 1
           else if (word(iwrd) .eq. 'ONNPS' ) then
C                                              ** ON_NPS **
              mislvl = .false.
              iopton(16) = 3
           else if (word(iwrd) .eq. 'ONDCONLY' ) then
C                                              ** ON_DCONLY **
              mislvl = .false.
              iopton(16) = 4
           endif
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD
        goto 2000
 
C       /PINPUT / FULL   , ERRORS = NO_LIST         : JOBSW = 4
C                 NONE              LIST
C                 ZONES = AA,BB,CC,DD,EE,FF,...,XX
C                 ZONES = ALL, FULL, or NONE
 
  500   iwrd = iwrd + 1
        do while (iwrd .le. nwrd )
           lisopt = ickdic(word(iwrd),lstopt,lopt)
           if (lisopt .eq. 0 ) then
           else if (lisopt .eq. 1) then
              mislvl = .false.
C                                               ** NONE **
              kspare(4) = 0
           else if ( lisopt .eq. 3 ) then
              mislvl = .false.
C                                               ** FULL **
              kspare(4) = 2
           else if (lisopt .eq. 2) then
              mislvl = .false.
C                                               ** ZONES **
              iwrd = iwrd + 1
              done = .false.
              do while (iwrd .le. nwrd .and. .not. done) 
                 word(iwrd) = cnvtwrd(word(iwrd))
                 if (word(iwrd) .eq. 'NONE') then
                    kspare(4) = 0
                    done = .true.
                 else if (word(iwrd) .eq. 'FULL') then
                    kspare(4) = 2
                    done = .true.
                 else if (word(iwrd) .eq. 'ALL') then
                    kspare(4) = 2
                    done = .true.
                 else if (word(iwrd) .eq. '*') then
                    kspare(4) = 2
                    done = .true.
                 else
C                                               ** PARTIAL ZONES **
                    kspare(4) = 1
                    do while (iwrd .le. nwrd .and. .not. done)
                       if (firstxstr(word(iwrd),'ERROR') .ne. 0) then
                          iwrd = iwrd - 1
                          done = .true.
                       else
                          npzdta  = npzdta  + 1
                          pzdlst (npzdta ) = word(iwrd)
                          iwrd = iwrd + 1
                       endif
                    enddo
                 endif
                 iwrd = iwrd + 1
              enddo

           else if (lisopt .eq. 4) then
              mislvl = .false.
C                                               ** ERRORS **
              kspare(3) = 10*(kspare(3)/10)
              iwrd = iwrd + 1
              if (iwrd .le. nwrd) then
                 if (cnvtwrd (word(iwrd)) .eq. 'LIST') then
                    kspare(3) = kspare(3) + 1
                 endif
              endif
           
           endif
           iwrd = iwrd + 1

        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       /FINPUT / FULL   , ERRORS = NO_LIST         : JOBSW = 5
C                 NONE              LIST
C                 ZONES = AA,BB,CC,DD,EE,FF,...,XX
C                 ZONES = ALL, FULL, or NONE
 
  540   if (kspare(16) .eq. -1) then
C                                        NO FICHE REQUESTED
           kspare(5)=0
           mislvl = .false.
           iwrd = nwrd 
        endif
        iwrd = iwrd + 1
 
        do while (iwrd .le. nwrd )
           lisopt = ickdic(word(iwrd),lstopt,lopt)
           if (lisopt .eq. 0 ) then
           else if (lisopt .eq. 1) then
              mislvl = .false.
C                                               ** NONE **
              kspare(5) = 0
           else if ( lisopt .eq. 3 ) then
              mislvl = .false.
C                                               ** FULL **
              kspare(5) = 2
           else if (lisopt .eq. 2) then
              mislvl = .false.
C                                               ** ZONES **
              iwrd = iwrd + 1
              done = .false.
              do while (iwrd .le. nwrd .and. .not. done) 
                 word(iwrd) = cnvtwrd(word(iwrd))
                 if (word(iwrd) .eq. 'NONE') then
                    kspare(5) = 0
                    done = .true.
                 else if (word(iwrd) .eq. 'FULL') then
                    kspare(5) = 2
                    done = .true.
                 else if (word(iwrd) .eq. 'ALL') then
                    kspare(5) = 2
                    done = .true.
                 else if (word(iwrd) .eq. '*') then
                    kspare(5) = 2
                    done = .true.
                 else
C                                               ** PARTIAL ZONES **
                    kspare(5) = 1
                    do while (iwrd .le. nwrd .and. .not. done)
                       if (firstxstr(word(iwrd),'ERROR') .ne. 0) then
                          iwrd = iwrd - 1
                          done = .true.
                       else
                          nfzdta  = nfzdta  + 1
                          fzdlst (nfzdta ) = word(iwrd)
                          iwrd = iwrd + 1
                       endif
                    enddo
                 endif
                 iwrd = iwrd + 1
              enddo

           else if (lisopt .eq. 4) then
              mislvl = .false.
C                                               ** ERRORS **
              kspare(3) = mod(kspare(3),10)
              iwrd = iwrd + 1
              if (iwrd .le. nwrd) then
                 if (cnvtwrd (word(iwrd)) .eq. 'LIST') then
                    kspare(3) = kspare(3) + 10
                 endif
              endif
           
           endif
           iwrd = iwrd + 1

        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       POUTPUT / FULL   ,FAILED_SOL = FULL_LIST    : JOBSW = 6
C                                      PART_LIST
C                 NONE                 NO_LIST
C                 ZONES = AA,BB,CC,DD,EE,FF,...,XX
C                 ZONES = ALL, FULL, or NONE
 
  600   iwrd = iwrd + 1
        do while (iwrd .le. nwrd )
           lisopt = ickdic(word(iwrd),lstopt,lopt)
           if (lisopt .eq. 0 ) then
              iwrd = iwrd + 1
           else if (lisopt .eq. 1) then
              mislvl = .false.
C                                               ** NONE **
              kspare(6) = 0
              iwrd = iwrd + 1
           else if ( lisopt .eq. 3 ) then
              mislvl = .false.
C                                               ** FULL **
              kspare(6) = 2
              iwrd = iwrd + 1
           else if (lisopt .eq. 2) then
              mislvl = .false.
C                                               ** ZONES **
              iwrd = iwrd + 1
              done = .false.
              do while (iwrd .le. nwrd .and. .not. done) 
                 word(iwrd) = cnvtwrd(word(iwrd))
                 if (word(iwrd) .eq. 'NONE') then
                    kspare(6) = 0
                    done = .true.
                    iwrd = iwrd + 1
                 else if (word(iwrd) .eq. 'FULL') then
                    kspare(6) = 2
                    done = .true.
                    iwrd = iwrd + 1
                 else if (word(iwrd) .eq. 'ALL') then
                    kspare(6) = 2
                    done = .true.
                    iwrd = iwrd + 1
                 else if (word(iwrd) .eq. '*') then
                    kspare(6) = 2
                    done = .true.
                    iwrd = iwrd + 1
                 else
C                                               ** PARTIAL ZONES **
                    kspare(6) = 1
                    do while (iwrd .le. nwrd .and. .not. done)
                       if (firstxstr(word(iwrd),'FAIL') .ne. 0) then
                          done = .true.
                       else
                          npzout = npzout + 1
                          pzolst (npzout) = word(iwrd)
                          iwrd = iwrd + 1
                       endif
                    enddo
                 endif
              enddo

           else if (lisopt .eq. 5) then
              mislvl = .false.
C                                               ** FAILED **
              kspare(10) = 10*(kspare(10)/10)
              iwrd = iwrd + 1
              if (iwrd .le. nwrd) then
                 if (firstxstr (word(iwrd),'FULL') .ne. 0) then
                    kspare(10) = kspare(10) + 1
                 else if (firstxstr (word(iwrd),'ZONE') .ne. 0) then
                    kspare(10) = kspare(10) + 2
                 else if (firstxstr (word(iwrd),'PART') .ne. 0) then
                    kspare(10) = kspare(10) + 2
                 endif
                 iwrd = iwrd + 1
              endif

           endif

        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       /FOUTPUT / FULL   ,FAILED_SOL = FULL_LIST    : JOBSW = 7
C                                       PART_LIST
C                  NONE                 NO_LIST
C                  ZONES = AA,BB,CC,DD,EE,FF,...,XX
C                  ZONES = ALL, FULL, or NONE
 
  640   if (kspare(16) .eq. -1) then
C                                        NO FICHE REQUESTED
           kspare(7)=0
           mislvl = .false.
           iwrd = nwrd 
        endif
        iwrd = iwrd + 1
        do while (iwrd .le. nwrd )
           lisopt = ickdic(word(iwrd),lstopt,lopt)
           if (lisopt .eq. 0 ) then
           else if (lisopt .eq. 1) then
              mislvl = .false.
C                                               ** NONE **
              kspare(7) = 0
              iwrd = iwrd + 1
           else if ( lisopt .eq. 3 ) then
              mislvl = .false.
C                                               ** FULL **
              kspare(7) = 2
              iwrd = iwrd + 1
           else if (lisopt .eq. 2) then
              mislvl = .false.
C                                               ** ZONES **
              iwrd = iwrd + 1
              done = .false.
              do while (iwrd .le. nwrd .and. .not. done) 
                 word(iwrd) = cnvtwrd(word(iwrd))
                 if (word(iwrd) .eq. 'NONE') then
                    kspare(7) = 0
                    done = .true.
                    iwrd = iwrd + 1
                 else if (word(iwrd) .eq. 'FULL') then
                    kspare(7) = 2
                    done = .true.
                    iwrd = iwrd + 1
                 else if (word(iwrd) .eq. 'ALL') then
                    kspare(7) = 2
                    done = .true.
                    iwrd = iwrd + 1
                 else if (word(iwrd) .eq. '*') then
                    kspare(7) = 2
                    done = .true.
                    iwrd = iwrd + 1
                 else
C                                               ** PARTIAL ZONES **
                    kspare(7) = 1
                    do while (iwrd .le. nwrd .and. .not. done)
                       if (firstxstr(word(iwrd),'FAIL') .ne. 0) then
                          done = .true.
                       else
                          nfzout = nfzout + 1
                          fzolst (nfzout) = word(iwrd)
                          iwrd = iwrd + 1
                       endif
                    enddo
                 endif
              enddo

           else if (lisopt .eq. 5) then
              mislvl = .false.
C                                               ** FAILED **
              kspare(10) = mod(kspare(10),10)
              iwrd = iwrd + 1
              if (iwrd .le. nwrd) then
                 if (firstxstr (word(iwrd),'FULL') .ne. 0) then
                    kspare(10) = kspare(10) + 10
                 else if (firstxstr (word(iwrd),'ZONE') .ne. 0) then
                    kspare(10) = kspare(10) + 20
                 else if (firstxstr (word(iwrd),'PART') .ne. 0) then
                    kspare(10) = kspare(10) + 20
                 endif
                 iwrd = iwrd + 1
              endif

           endif

        enddo
 
C       READ NEXT RECORD...
        goto 2000
 
C      / MVA_BASE = number                        : JOBSW = 21
 
  650   iwrd=iwrd+1
        if (iwrd .le. nwrd) then
           bmva = rval(word(iwrd))
           mislvl = .false.
        endif
 
C       READ NEXT RECORD...
        goto 2000
 
C
C       /USER_ANALYSIS, FILE = *
C
  672   call getusr
 
        if ( incsw .eq.1 ) then
           outbuf = '      ' // inrcd
        else
           outbuf = ' ' // inrcd
        endif
        call prtout(1)
        mislvl = .true.
        goto 10
C
C       / ANALYSIS_SELECT 
C       > FICHE, ZONES=<zone1,...>, AREAS=<area1,...>,
C                OWNERS=<owner1,...>
C       > PAPER, ZONES=<zone1,...>, AREAS=<area1,...>,
C                OWNERS=<owner1,...>
C       > UNSCH       <  (1) - Buses With Unscheduled Reactive
C       > LOSSOWN     <  (2) - Total System Generations and Loads by
C                              Owner
C       > SYSTEMZONE  <  (3) - System Generations,Loads,Losses and
C                              Shunts by Zones
C       > UVOV        <  (4) - Undervoltage-Overvoltage Buses
C       > LINELOAD    <  (5) - Transmission Lines Loaded Above xxx.x%
C                              of Ratings
C       > TRANLOAD    <  (6) - Transformers Loaded Above xxx.x% of
C                              Ratings
C       > TRANEX      <  (7) - Transformers Excited Above xxx.x% over Ta
C       > XSYSTEMLOSS <  (8) - Transmission System Losses
C       > BPALOADS    <  (9) - BPA Industrial Loads
C       > DCSYSTEM    < (10) - DC System
C       > SHUNTSUM    < (11) - Shunt Reactive Summary
C       > SUMLTC      < (12) - Summary of LTC Transformers
C       > SUMPHASE    < (13) - Summary of Phase-shifters
C       > SUM%VAR     < (14) - Summary of %Var-controlled buses
C                       (14) - Summary of AGC Scheme
C       > SUMBX       < (15) - Summary of Type BX buses
C       > SUMRANI     < (16) - Summary of Adjustable Var compensation
C       > SERIESCOMP  < (17) - Transmission Lines Containing Series
C                              Compensation
C       > BUS         < (18) - Bus Quantities
C       > SPIN        < (19) - Spinning Reserves
C       > LINEEFF     < (20) - Transmission Line Efficiency Analysis
C                              (Lines Loaded Above xxx.x % of Nominal
C                               Ratings.)
C       > USERANAL    < (21) - (User-defined Analysis)
C       > TRANEFF     < (22) - Transformer Total Losses Above xx.xx %
C                              of Nominal Rating.)
C       > TRANLOSS    < (23) - Transformer Core Losses Above xx.xx % of


C                              Nominal Ratings.
  680   continue
C
C       Read next record
C
        do 681 i = 1,23
           aflist(i) = 0
           aplist(i) = 0
           apsort(i) = 0
           apsize(i) = 132
           apfile(i) = ' '
  681   continue
 
        done = .false.
        do while (.not. done .and. .not. eof)
           read (inp,135,end=682) inrcd
           lastrcd = index (inrcd, carriagecontrol)
           if (lastrcd .gt. 0) inrcd(lastrcd:) = ' '
 
           ctyp = findstr('HC/(.>',inrcd(1:1))
           if ( ctyp .gt. 0 .and. ctyp .lt. 5) then
              if ( incsw .eq. 1 ) then
                 outbuf = '      ' // inrcd
              else
                 outbuf = ' ' // inrcd
              endif
              call prtout(1)
              mislvl = .true.
              done = .true.
 
           else if ( ctyp .eq. 5 ) then
              mislvl = .false.
           else if (ctyp .eq. 6) then
              if ( incsw .eq.1 ) then
                 outbuf = '      ' // inrcd
              else
                 outbuf = ' ' // inrcd
              endif
              call prtout(1)
              call uscan(inrcd(2:), word, nwrd, '|', ', =<>')
              word(1) = cnvtwrd(word(1))
              if (word(1) .eq. 'FICHE' .or. word(1) .eq. 'PAPER') then
C
C                > FICHE, ZONES=<zone1,...>, AREAS=<area1,...>,
C                         OWNERS=<owner1,...>
C
C                > PAPER, ZONES=<zone1,...>, AREAS=<area1,...>,
C                         OWNERS=<owner1,...>
C
                 if (word(1) .eq. 'FICHE') then
                    kspare(9) = 1
                 else
                    kspare(8) = 1
                 endif
                 iwrd = 2
                 do while (iwrd .le. nwrd) 
                    lisopt = ickdic(word(iwrd),anropt,lan2)

                    if (lisopt .eq. 0) then
                       write (errbuf(1), 689) word(iwrd)
                       write (errbuf(2), 690) inrcd(1:80)
                       call prterx ('W',2)

                    else if ( lisopt .eq. 2 ) then
                       mislvl = .false.
C
C                      ** PARTIAL ZONES **
C
                       iwrd = iwrd + 1
                       do while (iwrd .le. nwrd) 
                          if (word(1) .eq. 'FICHE') then
                             nfzanl = nfzanl + 1
                             fzalst (nfzanl) = word(iwrd)
                          else
                             npzanl = npzanl + 1
                             pzalst (npzanl) = word(iwrd)
                          endif
                          iwrd = iwrd + 1
                       enddo
 
                    else if ( lisopt .eq. 3 ) then
                       mislvl = .false.
C
C                      ** PARTIAL OWNERS **
C
                       iwrd = iwrd + 1
                       do while (iwrd .le. nwrd) 
                          if (word(1) .eq. 'FICHE') then
                             nfoanl = nfoanl + 1
                             foalst(nfoanl) = word(iwrd)
                          else
                             npoanl = npoanl + 1
                             poalst(npoanl) = word(iwrd)
                          endif
                          iwrd = iwrd + 1
                       enddo
 
                    else if ( lisopt .eq. 4 ) then
                       mislvl = .false.
C
C                      ** PARTIAL AREAS **
C
                       iwrd = iwrd + 1
                       do while (iwrd .le. nwrd) 
                          if (word(1) .eq. 'FICHE') then
                             nfaanl = nfaanl + 1
                             faalst(nfaanl) = word(iwrd)
                          else
                             npaanl = npaanl + 1
                             paalst(npaanl) = word(iwrd)
                          endif
                          iwrd = iwrd + 1
                       enddo
                    endif
                 enddo
              else
                 iwrd = 1
                 lisopt = ickdic(word(iwrd),selanl,lansel)
                 if (lisopt .ne. 0) then
                    mislvl = .false.
                    if (nwrd .eq. 1) then
                       aflist(lisopt) = 1
                       aplist(lisopt) = 1
                    else
C
C                      Search for > LINE_EFF, SORT=VOLTAGE,
C                                             OUTPUT=file_name,
C                                             REPORT_WIDTH=size,
C                                             F, P
C
                       iwrd = iwrd + 1
                       do while (iwrd .le. nwrd)
                          word(iwrd) = cnvtwrd(word(iwrd))
                          if (word(iwrd)(1:4) .eq. 'SORT') then
                             iwrd = iwrd + 1
                             if (word(iwrd)(1:2) .eq. 'VO') then
                                apsort(lisopt) = 1
                             else if (word(iwrd)(1:2) .eq. 'OW') then
                                 apsort(lisopt) = 2
                             else if (word(iwrd)(1:2) .eq. 'ZO') then
                                apsort(lisopt) = 3
                             endif
                             iwrd = iwrd + 1
                             word(iwrd) = ' '
                          else if (word(iwrd)(1:3) .eq. 'OUT') then
                             iwrd = iwrd + 1
                             apfile(lisopt) = word(iwrd)
                             word(iwrd) = ' '
                          else if (word(iwrd)(1:3) .eq. 'REP') then
                             iwrd = iwrd + 1
                             if (word(iwrd) .ne. ' ') then
                                do while (lastch(word(iwrd)) .lt. 3) 
                                   tempc = ' ' // word(iwrd)
                                   word(iwrd) = tempc
                                enddo
                             endif
                             read (word(iwrd), '(i3)') apsize(lisopt)
                             word(iwrd) = ' '
                          else if (word(iwrd)(1:1) .eq. 'F') then
                             aflist(lisopt) = 1
                          else if (word(iwrd)(1:1) .eq. 'P') then
                             aplist(lisopt) = 1
                          endif
                          iwrd = iwrd + 1
                       enddo
                       if (aflist(lisopt) .eq. 0 .and.
     1                     aplist(lisopt) .eq. 0) then
                          aflist(lisopt) = 1
                          aplist(lisopt) = 1
                       endif
                    endif
                 else
                    write (errbuf(1),689) word(iwrd)
  689               format ('Unrecognized option (', a,
     1                 ') on record:.')
                    write (errbuf(2), 690) inrcd(1:80)
  690               format (' Text (', a, ')')
                    call prterx ('W',2)
                 endif
              endif
           endif
           iwrd = iwrd + 1
        enddo
        go to 683
  682   eof = .true.
        go to 2000
  683   continue
        go to 10
 
C       / PANALYSIS / LEVEL= 4, ZONES = AA,BB,..,XX )    : JOBSW = 8
C                              OWNERS = ABC,DEF,XXX )
C                                                
  700   iwrd = iwrd + 1
        do while (iwrd .lt. nwrd )
           lisopt = ickdic(word(iwrd),anropt,lan2)
           if (lisopt .eq. 0 ) then
           else if (lisopt .eq. 1) then
              mislvl = .false.
C                                               ** LEVEL **
              iwrd = iwrd + 1
              kspare(8) = rval(word(iwrd))
              if (kspare(8) .eq. 1) then
                 aplist(1) = 1
                 do i = 2,23
                    aplist(i) = 0
                 enddo
              else if (kspare(8) .eq. 2) then
                 do i = 1,17
                    aplist(i) = 1
                 enddo
                 do i = 18,23
                    aplist(i) = 0
                 enddo
              else if (kspare(8) .eq. 3) then
                 do i = 1,18
                    aplist(i) = 1
                 enddo
                 do i = 19,23
                    aplist(i) = 0
                 enddo
              else if (kspare(8) .eq. 4) then
                 do i = 1, 23
                    aplist(i) = 1
                 enddo
              endif
 
           else if ( lisopt .eq. 2 ) then
C                                        ** PARTIAL ZONES **
              mislvl = .false.
              iwrd = iwrd + 1
              do while (iwrd .le. nwrd) 
                 npzanl = npzanl + 1
                 pzalst (npzanl) = word(iwrd)
                 iwrd = iwrd + 1
              enddo
 
           else
C                                       ** PARTIAL OWNERS **
              mislvl = .false.
              iwrd = iwrd + 1
              do while (iwrd .le. nwrd)
                 npoanl = npoanl + 1
                 poalst (npoanl) = word(iwrd)
                 iwrd = iwrd + 1
              enddo
           endif
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       / FANALYSIS / LEVEL= 4, ZONES = AA,BB,..,XX )
C                               OWNERS = ABC,DEF,XXX )
 
  740   if (kspare(16) .eq. -1) then
C                   NO FICHE REQUESTED, TURN OFF
           kspare(9)=0
           mislvl = .false.
           iwrd = nwrd
        endif
        iwrd = iwrd + 1
        do while (iwrd .lt. nwrd )
           lisopt = ickdic(word(iwrd),anropt,lan2)
           if (lisopt .eq. 0 ) then
           else if (lisopt .eq. 1) then
              mislvl = .false.
C                                               ** LEVEL **
              iwrd = iwrd + 1
              kspare(9) = rval(word(iwrd))
              if (kspare(9) .eq. 1) then
                 aflist(1) = 1
                 do i = 2,23
                    aflist(i) = 0
                 enddo
              else if (kspare(9) .eq. 2) then
                 do i = 1,17
                    aflist(i) = 1
                 enddo
                 do i = 18,23
                    aflist(i) = 0
                 enddo
              else if (kspare(9) .eq. 3) then
                 do i = 1,18
                    aflist(i) = 1
                 enddo
                 do i = 19,23
                    aflist(i) = 0
                 enddo
              else if (kspare(9) .eq. 4) then
                 do i = 1, 23
                    aflist(i) = 1
                 enddo
              endif
 
           else if ( lisopt .eq. 2 ) then
C                                        ** PARTIAL ZONES **
              mislvl = .false.
              iwrd = iwrd + 1
              do while (iwrd .le. nwrd) 
                 nfzanl = nfzanl + 1
                 fzalst (nfzanl) = word(iwrd)
                 iwrd = iwrd + 1
              enddo
 
           else
C                                       ** PARTIAL OWNERS **
              mislvl = .false.
              iwrd = iwrd + 1
              do while (iwrd .le. nwrd)
                 nfoanl = nfoanl + 1
                 foalst (nfoanl) = word(iwrd)
                 iwrd = iwrd + 1
              enddo
           endif
           iwrd = iwrd + 1
        enddo

C       READ NEXT RECORD....
        go to 2000
 
C       / RPT_SORT = BUS                            : JOBSW = 20
C                    ZONE 
C                    AREA 
C                    OWNER
 
  750   iwrd = iwrd + 1
        if (cnvtwrd (word(iwrd)) .eq. 'BUS') then
C                                              ** BUS **
           kspare(11) = 0
           mislvl = .false.
 
        else if (cnvtwrd (word(iwrd)) .eq. 'ZONE') then
C                                              ** ZONE **
           kspare(11) = 1
           mislvl = .false.
 
        else if (cnvtwrd (word(iwrd)) .eq. 'AREA') then
C                                              ** AREA **
           kspare(11) = 2
           mislvl = .false.
 
        else if (cnvtwrd (word(iwrd)(1:3)) .eq. 'OWN') then
C                                              ** OWNER **
           kspare(11) = 3
           mislvl = .false.
 
        endif
 
C       READ NEXT RECORD...
        goto 2000
 
C       / AI_CONTROL= ON )                        : JOBSW = 18
C                     OFF)  
 
  800   iwrd = iwrd + 1
        do while (iwrd .le. nwrd ) 
           lisopt = ickdic(word(iwrd),aicdic,laic)
           if ( lisopt .eq. 0 ) then
           else if ( lisopt .eq. 1 ) then
              mislvl = .false.
C                                               ** OFF **
              iopton(17) = 0
           else if ( lisopt .eq. 2 ) then
              mislvl = .false.
C                                               ** CON **
              iopton(17) = 1
           else if ( lisopt .eq. 3 ) then
              mislvl = .false.
C                                               ** MON **
              iopton(17) = 2
           endif
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C      / AI_LIST= TIE_LINE                          : JOBSW = 10
C               MATRIX
C               FULL
C               NONE
 
 850    iwrd = iwrd + 1
        do while (iwrd .le. nwrd ) 
           lisopt = ickdic(word(iwrd),aildic,lail)
 
           if ( lisopt .eq. 0) then
 
           else if (lisopt .eq. 1) then
              mislvl = .false.
C                                               ** NONE **
              kspare(12) = 0
 
           else if ( lisopt .eq. 2) then
              mislvl = .false.
C                                               ** TIE_LINE **
              kspare(12) = 2
 
           else if ( lisopt .eq. 3 ) then
              mislvl = .false.
C                                               ** MATRIX **
              kspare(12) = 1
 
           else if ( lisopt .eq. 4 ) then
              mislvl = .false.
C                                               ** FULL **
              kspare(12) = 3
 
           endif
           iwrd = iwrd + 1
 
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       / OI_LIST = NONE                           : JOBSW = 11
C                   TIE_LINE
C                   MATRIX
C                   FULL
 
  900   iwrd = iwrd + 1
        do while (iwrd .le. nwrd ) 
           lisopt = ickdic(word(iwrd),aildic,lail)
           if (lisopt .eq. 0 ) then
           else if (lisopt .eq. 1) then
              mislvl = .false.
C                                               ** NONE **
              kspare(13) = 0
 
           else if ( lisopt .eq. 2 ) then
              mislvl = .false.
C                                               ** TIE_LINE **
              kspare(13) = 2
 
           else if ( lisopt .eq. 3 ) then
              mislvl = .false.
C                                               ** MATRIX **
              kspare(13) = 1
 
           else if ( lisopt .eq. 4 ) then
              mislvl = .false.
C                                               ** FULL **
              kspare(13) = 3
 
           endif
           iwrd = iwrd + 1
        enddo 
 
C       READ NEXT RECORD....
        go to 2000
 
C       /TRACE, X_REF = OFF, AUTO = OFF, Y_MATRIX = OFF, REORDER = OFF
C                       ON          ON              ON             PART

C                                                                  FULL

C               OUTPUT = OFF, MERGE = OFF, CHANGE   = OFF
C                        ON           ON              ON
C
C                                                 : JOBSW = 23
 
 1000   iwrd = iwrd + 1
        do while (iwrd .lt. nwrd ) 
           iopt = ickdic(word(iwrd),traced,ltrac)
           if ( iopt .gt. 0 ) then
 
              iwrd = iwrd + 1
              if (firstxstr(word(iwrd),'ON') .gt. 0 .or.
     1            firstxstr(word(iwrd),'PART') .gt. 0) then
C
                 kase1(trapnt(iopt))=1
 
              else if (firstxstr(word(iwrd),'OFF') .gt. 0) then
                 kase1(trapnt(iopt))=0
 
              else if (firstxstr(word(iwrd),'FULL') .gt. 0) then
                 kase1(40)=2
              endif
              mislvl = .false.
           endif
           iwrd = iwrd + 1 
        enddo
 
C       READ NEXT RECORD....
        go to 2000
C
C       The following logic was disabled 19 February 1987 by WLP. 
C       Error messages are now replaced in subroutine PRTERX with 
C       an explicit severity code {IWEFA} parameter in place of 
C       the former error number.
C
C       / ERRORSET errornum,sever/errornum,sever/...
C                                                 : JOBSW = 13
 1100   mislvl = .false.
 
C       READ NEXT RECORD....
        go to 2000
 
C       / VOLTAGE_LIMITS                         : JOBSW = 14
C
 1200   continue
 
        call glblim   ! Obtain global voltage limits
        mislvl = .false.
 
C       READ NEXT RECORD....
        go to 2000
 
C       / OLD_BASE FILE = <filename>, CASE = <casename>, REBUILD = ON
C                                                                  OFF
C                                                 : JOBSW = 1
 1300   iwrd = iwrd + 1
        do while (iwrd .lt. nwrd ) 
           if (firstxstr(word(iwrd),'FILE') .gt. 0) then
              jobreq(2) = 'OLD_BASE'
              iwrd = iwrd + 1
              mislvl = .false.
              if (cnvtwrd (word(iwrd)) .ne. '*') obasnm = word(iwrd)
 
              if (obasnm  .eq.  nbasnm) then
                 close(unit=datao)
                 kspare(14) = 0
              endif
 
           else if (firstxstr(word(iwrd),'CASE*') .gt. 0) then
              iwrd = iwrd + 1
              mislvl = .false.
 
              if (cnvtwrd (word(iwrd)) .ne. '*') then
                 crun1(3) = word(iwrd)
              else
                 crun1(3) = ' '
              endif
 
           else if (firstxstr(word(iwrd),'REB*') .gt. 0) then
              kspare(1)=1
              mislvl = .false.
           endif
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       / INCLUDE_CON, FILE = <filename>           :  JOBSW = 33
 
 1400   iwrd=iwrd+1
        if (iwrd .lt. nwrd) then
           if (firstxstr(word(iwrd),'FILE*') .gt. 0) iwrd=iwrd+1
           mislvl = .false.
           temfil=inpnm
           teminp=inp
           inpnm=word(iwrd)
           inp = 30
           error=0
           call opnfil (inp,inpnm,error)
           if (error .ne. 0) then
              inpnm=temfil
              inp=teminp
           else
              incsw = 1
           endif
 
        else
           write (errbuf(1),1410)
 1410      format ('File name missing from "INCLUDE_CON" command.')
           if (is_batch .eq. 0) then
              call prterx ('E',1)
           else
              call prterx ('F',1)
           endif
        endif
 
C       READ NEXT RECORD....
        goto 2000
 
C       /NEXTCASE, CASEID = <casename>, PROJECTID = <project name> 
C
C       Process this command as if (POWERFLOW, CASEID=....
 
 1500   rdnext = .false.
        inrcd(1:1) = '('
 
C       READ NEXT RECORD....
        goto 2000
 
c       /HEADER
c
 1700   call comlod(1)

C       READ NEXT RECORD....
        goto 2000

c       /COMMENT
c
 1710   do i = 1, MAXCMT
           com(i) = ' '
        end do
        ncom = 0

C       READ NEXT RECORD....
        goto 2000


C       /SOL_ITER, DECOUPLED = nnn, NEWTON = nnn, OPTIM = nnn
C
C                                                 : JOBSW = 16
 1800   iwrd = iwrd + 1
        do while (iwrd .lt. nwrd) 
 
           jsln = ickdic( word(iwrd),soldic  ,lsoli)
 
           if (jsln .gt. 0) then
              mislvl = .false.
 
              iwrd = iwrd + 1
              isub = itepnt(jsln)
 
              if (isub .lt. 0) then
                 option(-isub) = rval(word(iwrd))
              else
                 iopton(isub) = rval(word(iwrd))
              endif
           endif
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       /TOLERANCE, BUS=nn, AREA=nn, TX=nn, Q=nn, OPCUT = nn
C
C                                                 : JOBSW = 17
 1830   iwrd = iwrd + 1
        do while (iwrd .lt. nwrd) 
 
           jsln = ickdic( word(iwrd),toldic,ltol )
 
           if (jsln .gt. 0) then
 
              mislvl = .false.
              iwrd = iwrd + 1
              isub = tolpnt(jsln)
 
              if (isub .lt. 0) then
                 option(-isub) = rval(word(iwrd))
              else
                 iopton(isub) = rval(word(iwrd))
              endif
           endif
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       / LIMITS, QRES=nn, PHA=nn, DEL_P_MIN=nn, DEL_P_MAX=nn
C                 DEL_ANG=nn,      DEL_V_MIN=nn, DEL_V_MAX=nn
C                 DEL_VOLT=nn,     DEL_T_MIN=nn, DEL_T_MAX=nn
 
C                                           : JOBSW = 25
1840    iwrd = iwrd + 1
        do while (iwrd .lt. nwrd) 
           jsln = ickdic( word(iwrd),limdic,llim )
           if (jsln .gt. 0) then
 
              iwrd = iwrd + 1
              mislvl = .false.
              isub = limpnt(jsln)
 
              if (isub .lt. 0) then
                 option(-isub) = rval(word(iwrd))
              else
                 iopton(isub) = rval(word(iwrd))
              endif
           endif
           iwrd = iwrd + 1 
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       / DEBUG, TX = OFF, BUS = OFF, AI = OFF, DCMODEL = OFF
C                     ON         ON        ON             ON
C                OPT = OFF, OPV = OFF, OPP = OFF
C                      ON         ON         ON
C
C                                                 : JOBSW = 12
 1850   iwrd = iwrd + 1
        do while (iwrd .lt. nwrd) 
           jsln = ickdic( word(iwrd),debdic,ldeb )
 
           if (jsln .ne. 0) then
              isub = debpnt(jsln)
              iopton(isub) = 1
              mislvl = .false.
           endif
           
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       / MIS_CONTROLS, VFLATstart=ON, RELAXfactor=1.0, 
C                       XRGHOSTnode=ON, VOLTAGERelax=OFF, 
C                       SAVEBE=OFF
C                                                 : JOBSW = 26
 1860   iwrd = iwrd + 1
        do while ( iwrd .lt. nwrd ) 
          jsln = ickdic( word(iwrd),condic,lcon )
 
          if (jsln .gt. 0) then
 
             iwrd = iwrd + 1
             isub = conpnt(jsln)
C                                 RELAXfactor
 
             if ( jsln .eq. 2 ) then
                if (isub .lt. 0) then
                   option(-isub) = rval(word(iwrd))
                endif
C                               all other parameters
 
             else
                iopton(isub) = firstxstr( word(iwrd),'ON')
             endif
 
             mislvl = .false.
 
           endif
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       / CHANGE_BUS_TYPES, ...
 
 1862   continue
        rdnext = .false.
 
C       READ NEXT RECORD....
        go to 2000
 
C       / %LOAD_CHANGE
 
10862   continue
        rdnext = .false.
 
        go to 2000
 
C       / CHANGES, FILE = <filespec> 
 
 1870   if (firstxstr (word(1), 'CHANGESYS') .ne. 0) then
           write (errbuf(1),130) inrcd(1:80)
           if (is_batch .eq. 0) then
              call prterx ('E',1)
           else
              call prterx ('F',1)
           endif
           rdnext = .true.
 
        else
           rdnext = .false.
           chgnam = ' '
 
           iwrd = iwrd + 1
 
           do while (iwrd .lt. nwrd ) 
              if (firstxstr(word(iwrd),'FILE') .gt. 0) then
                 iwrd = iwrd + 1
                 mislvl = .false.
                 chgnam = word(iwrd)
              if ( chgnam .eq. '*' ) chgnam = ' '
              endif
              iwrd = iwrd + 1
           enddo
 
        endif
 
C       READ NEXT RECORD....
        go to 2000
 
C       ( DATA )                                  : JOBSW = 29
C       ( STOP )                                  : JOBSW = 28
C       ( END )                                   : JOBSW = 27
 
 1900   continue
        mislvl = .false.
        rdnext = .false.
        jobreq(1) = cnvtwrd (word(iwrd))
 
C       READ NEXT RECORD....
 
        go to 2000
 
C       / MERGE_OLD_BASE, SUBSYSTEM_ID = <mmm>, 
C                         OLD_BASE_FILE = <FILE_NAME>,
C                         CASE = <CASE_NAME> 
C       / MERGE_NEW_BASE, SUBSYSTEM_ID = <mmm>,
C                         BRANCH_DATA_FILE = <FILE_NAME>,
C                         DATE = <MYY>,R = <M>,
C                         BUS_DATA_FILE = <FILE_NAME> 
 1910   rdnext = .false.
        jobreq(1) = cnvtwrd (word(1))
 
        iwrd = iwrd + 1
        do while (iwrd .lt. nwrd ) 

           if (firstxstr(word(iwrd),'SUBSYS*') .gt. 0) then
              iwrd = iwrd + 1
              if (iwrd .lt. nwrd) then
                 if (firstxstr(word(iwrd),'ID') .gt. 0) iwrd=iwrd+1
              endif
C                                             store the subsys_id
              cspare(38) = word(iwrd)
              mislvl = .false.

           else if (firstxstr(word(iwrd),'OLDB*') .gt. 0) then
               mislvl = .false.
               iwrd = iwrd + 1
               if (iwrd .le. nwrd ) then
                  crun1(3) = ' '
                  obasnm = word(iwrd)
                  if ( obasnm .eq. nbasnm ) then
                     close ( unit = datao )
                     kspare(14) = 0
                  endif
               endif
C
           else if (firstxstr(word(iwrd),'CASE*') .gt. 0) then
              iwrd = iwrd + 1
              if (word(iwrd) .eq. '*')  word(iwrd) = ' '
              crun1(3) = word(iwrd)
C
           else if (firstxstr(word(iwrd),'REB*') .gt. 0) then
              iwrd = iwrd + 1
              kspare(1) = 1
           
           else if (firstxstr(word(iwrd),'BRA*') .gt. 0) then
              iwrd = iwrd + 1
              brdnam = word(iwrd)
              mislvl = .false.
           else if (firstxstr(word(iwrd),'DATE') .gt. 0) then
              iwrd = iwrd + 1
              read (word(iwrd),220) crun1(1),krun1(2)
              crun1(2) = word(iwrd)(2:3)
C ***
C ***         Determine WINTER/SUMMER extended ratings from date.
C ***
              if (word(iwrd)(1:1) .eq. '1') then
                 kspare(15) = 1
              else if (word(iwrd)(1:1) .eq. '8') then
                 kspare(15) = 2
              endif
              mislvl = .false.

           else if (firstxstr(word(iwrd),'R') .gt. 0) then
C ***
C ***         Determine WINTER/SUMMER extended ratings from
C ***         R = 1 (Winter peak)
C ***         R = 2 (Extra heavy winter peak)
C ***         R = 3 (Moderate cold winter peak)
C ***         R = 4 (Spring peak)
C ***         R = 8 (Summer peak)
C ***
              iwrd = iwrd + 1
              if (findstr ('12348', word(iwrd)(1:1)) .ne. 0) then
                 read (word(iwrd)(1:1), '(i1)') kspare(15)
                 mislvl = .false.
              else
                 mislvl = .true.
              endif

           else if (firstxstr(word(iwrd),'BUSD*') .gt. 0) then
              iwrd = iwrd + 1
              bsdnam = word(iwrd)
              mislvl = .false.
           
           endif
           iwrd = iwrd + 1
        enddo

        if (firstxstr(word(1),'MERGEOLD*') .gt. 0) then
C
C          Check conclusion of /MERGE_OLD_BASE
C
           if (obasnm .eq. ' ') then
              write (errbuf(1),1922)
 1922         format (' Missing old_base file for base-merge ')
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
           else
              jobreq(2) = 'OLD_BASE'
              jobreq(3) = 'BUILD_BASE'
           endif
C
        else if (firstxstr(word(1),'MERGENEW*') .gt. 0) then
C
C          Check conclusion of /MERGE_NEW_BASE
C
           jobreq(3) = 'BUILD_BASE'

        endif
 
C       READ NEXT RECORD....
        go to 2000
 
 1960   continue
 
C       / REDUCTion 
 
        rdnext = .false.
        jobreq(1) = 'REDUCTION'
        jobreq(3) = ' '
 
C       READ NEXT RECORD....
        go to 2000
 
 1980   continue
 
C       / OUTAGESimualtion 
 
        rdnext = .false.
        jobreq(1) = 'OUTAGE_SIM'
        jobreq(3) = ' '
 
C       READ NEXT RECORD....
        go to 2000

C       / SOLUTION 
 
 1990   rdnext = .false.
        jobreq(1) = 'SOLUTION'
        jobreq(3) = ' '
 
C       READ NEXT RECORD....
        go to 2000
 
C       / GEN_DROP,
C       / BUS_SENSITIVITY,
C       / LINE_SENSITIVITY,
C       / LOSS_SENSITIVITY,
C       / CHANGE_BUS_TYPES, or
C       / AGC
 
 1992   rdnext = .false.
        jobreq(1) = 'SOLUTION'
        jobreq(3) = ' '
 
C       READ NEXT RECORD....
        go to 2000
 
C       / EXTENDED_RATINGS = WINTER               : JOBSW = 45
C                            SUMMER
 
 1994   rdnext = .true.
        iwrd = iwrd + 1
        do while (iwrd .le. nwrd ) 
           if (firstxstr(word(iwrd),'WIN*') .gt. 0) then
              mislvl = .false.
              kspare(15) = 1
           else if (firstxstr(word(iwrd),'SUM*') .gt. 0) then
              mislvl = .false.
              kspare(15) = 2
           endif
           iwrd = iwrd + 1
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C                                                        : JOBSW = 46
C       / SORT_ANALYSIS = OVERLOAD = <OWNER>, OVERVOLTAGE = <OWNER>
C                                    <BUS>                  <BUS>
C                                    <ZONE>                 <ZONE>
C                                    <AREA>                 <AREA>
 2100   rdnext = .true.
        iwrd = iwrd + 1
        do while ( iwrd .le. nwrd-1) 
 
           if (firstxstr(word(iwrd),'OVERL*') .gt. 0) then
              mislvl = .false.
              i = ickdic(word(iwrd+1),sortdt,4)
              kspare(21) = mod(kspare(21),10) + 10 * i
           else if (firstxstr(word(iwrd),'OVERV*') .gt. 0) then
              mislvl = .false.
              i = ickdic(word(iwrd+1),sortdt,4)
              kspare(21) = kspare(21)/10*10 * i
           endif
           iwrd = iwrd + 2
        enddo
 
C       READ NEXT RECORD....
        go to 2000
 
C       END PROCESSING FOR EACH RECORD.
 
      endif
 
 2000 if ( rdnext ) then
         if (mislvl) then
            errbuf(1) = '0 Missing parameter on control record:'
            errbuf(2) = '          (' // inrcd(:100) // ')'
            if (is_batch .eq. 0) then
               call prterx ('E',2)
            else
               call prterx ('F',2)
            endif
         endif
 
C        Read-next-record
 
         if (.not. eof) then
            read (inp,135,end=2002) inrcd
            lastrcd = index (inrcd, carriagecontrol)
            if (lastrcd .gt. 0) inrcd(lastrcd:) = ' '
 
            ctyp = findstr('HC/(.',inrcd(1:1))
            if ( ctyp .gt. 0 .and. ctyp .lt. 5) then
               if ( incsw .eq.1 ) then
                  outbuf = '      ' // inrcd
               else
                  outbuf = ' ' // inrcd
               endif
               call prtout(1)
               mislvl = .true.
               goto 10
 
            else if ( ctyp .eq. 5 ) then
               mislvl = .false.
               goto 2000

            else
               go to 9000
            endif
 2002       eof = .true. 
         endif
      else
         go to 9000
      endif

      if (eof) then
 
C        Check to see if end-of-file. if true is it in "include_con"
C        file? If also true, reset to original file name and go on.
 
         if (incsw .eq. 1) then
            close( unit = inp )
            inpnm=temfil
            inp=teminp
            incsw = 0
            mislvl = .false.
            eof = .false.
            goto 2000
         endif
 
      endif
      inrcd = '( STOP - EOF )'
      jobreq(1) = 'STOP'
 
C     STOP-PROCESSING-CONTROL-RECORDS
 
 9000 continue
      return
 
      end

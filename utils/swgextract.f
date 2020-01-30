      program swgextract
C     -  Program to convert binary stability data into card image 
C     -  format for use by other swing programs.
C     -  
C  Revs:
C    Apr/19/89  -  DEM
C      Changed user input to abandon obsolete screen formatting
C    Sep/04/92  -  DEM
C      Renamed program to indicate its true function.  The old name
C        of SWGAUX normally refers to the tabular listing file produced
C        in the plotting phase of the Stab Program
C      Added proper formatting of KV of load-netted bus for < 10. kv
C     -  
      character  data*100, idisk*5,odisk*5,idirec*12,odirec*12
      character  space*1,iname*9,oname*9,itype*3,otype*3
      character infile*40, outfile*40 
      character changech*60, crtout*60 
      logical changes, havinfl, havoutfl
      logical always, never 
      data always /.true./, never /.false./
      character assumdir*13
      data assumdir / 'WSCCBASE_DIR:' /
C
C     -   Map variables into the DATA buffer....
      equivalence (data(1:1),idisk),(data(6:6),idirec)
      equivalence (data(18:18),iname),(data(27:27),itype)
      equivalence (data(30:30),odisk),(data(35:35),odirec)
      equivalence (data(47:47),oname),(data(56:56),otype)
C
      dimension impure(32)
C
      character*32 ofspc, nfspc
C
      dimension basekv(100),machin(100),lnetc(600),dccard(60)
C
      character*80 machin,dccard,lshed,lsrcp,lrelay,lrrod,lrepdt
      character*10 swdata,savout,swdate,pfdate,datx
      character*8 lnetc
C
      integer lrd,lrr,lsc,ldar,ldz,lrep,ls,ln, mac, bunch
C     -
      dimension lshed(100), lsrcp(100), lrelay(100), lrrod(100)
      dimension lnetn(600), bkvnet(600)
      dimension lrepdt(100)
C     -
      dimension key(10), keycol(10)
      character keych(10)*1, keymod(10)*1 
      data keych / 10*'Y' /
      data key / 10*1 /
C
C     -   Map variables into the MACHINE buffer...
      equivalence (machin,dccard),(machin,lshed)
      equivalence (machin,lsrcp),(machin,lrelay),(machin,lrrod)
      equivalence (machin,lrepdt)
C
      character*10 relay,remote,series,capac,load,repr
      character*10 shed,net,machn,direct,currnt,blnk10
      character*10 dev1,dev2
C
      data
     $   relay /'RELAY     '/,remote/'REMOTE    '/,series/'SERIES    '/,
     $   capac /'CAPACITOR '/,load  /'LOAD      '/,repr  /'REPRESENT '/,
     $   shed  /'SHEDDING  '/,net   /'NETTING   '/,machn /'MACHINE   '/,
     $   direct/'DIRECT    '/,currnt/'CURRENT   '/,blnk10/'          '/
C
C         ******************************************************
C         *                                                    *
C         *                     S W A U X                      *
C         *                                                    *
C         *   PURPOSE --                                       *
C         *      THIS PROGRAM HAS TWO PURPOSES.                *
C         *                                                    *
C         *      1) CONVERT THE SWING SDATAI FILE FROM BINARY  *
C         *      TO BCD SO IT MAY BE LOADED ON TAPE.           *
C         *                                                    *
C         *      2) SELECTIVELY DUMP DATA CARD IMAGES FROM     *
C         *      SDATAI FOR INPUT TO THE SWING.                *
C         *                                                    *
C         *   FILES --                                         *
C         *      TAPE1 IS THE ORIGINAL SDATAI FILE             *
C         *      TAPE2 IS THE CONVERSION OUTPUT FILE           *
C         *                                                    *
C         *      INPUT KEYS FROM TERMINAL, OR DATA CARD        *
C         *                                                    *
C         *   AUTHOR --                                        *
C         *      GARY RIGGS                                    *
C         *      METHODS DEVELOPMENT, EOGA                     *
C         *                                                    *
C         ******************************************************
C     -
C     -   -   Meaning of key subscripts:
C     -   1 for local relays
C     -   2 for remote relays
C     -   3 for series caps 
C     -   4, 5 & 6 for load rep 
C     -   7 for load shedding
C     -   8 for load netting
C     -   9 for machines (includes windings, exciters & governors)
C     -  10 for DC data (includes margins and modulations)
C     -
 10   format (a)               ! General format for most card image data
      space=' '
      havinfl = .false.
      havoutfl = .false.
      open (unit=5,status='OLD',name='SYS$INPUT:',readonly,shared)
      open (unit=6,status='NEW',name='SYS$OUTPUT:')
C  -  -  Prompt for input file
      call clear_screen
      call jump_line(1)
      write (6,'(A,10X,A)') '+','< STABILITY DATA EXTRACTION >' 
      write (6,'(3X)') 
      iopenerr = 1
      do while (iopenerr .ne. 0)
         write (6,'(3X,A,$)') 'File to extract from? '
         read (5,'(A)') infile 
         open (unit=1, status='OLD', name=infile, readonly, 
     +      form='UNFORMATTED',iostat=iopenerr)      ! Binary input file
         if (iopenerr .ne. 0) then 
            write (6,'(3X,2A)') 'Couldn''t open input file.  ',
     +        'Try again.' 
         else
            havinfl = .true.
         endif               
      enddo
C  -  -  Prompt for output file
      iopenerr = 1
      do while (iopenerr .ne. 0)
      write (6,'(3X,A,$)') 'File to hold card images? '
      read (5,'(A)') outfile 
      open (unit=2, status='NEW', name=outfile, iostat=iopenerr,
     1    carriagecontrol='LIST')              ! Card-image output file
         if (iopenerr .ne. 0)  then
            write (6,'(3X,2A)') 'Couldn''t open output file.  ',
     +        'Try again.'
         else
            havoutfl = .true.
         endif
      enddo 
C     -
C     -    Now have files - update file portion of screen
      call clear_screen
      call jump_line (1)
      write (6,'(A,10X,A)') '+','< STABILITY DATA EXTRACTION >' 
      write (6,'(3X)') 
      write (6,'(3X)') 
      write (6,'(3X,2A)') 'Binary input file:       ',infile
      write (6,'(3X)') 
      write (6,'(3X,2A)') 'Card image output file:  ',outfile
      write (6,'(3X)') 
      write (6,'(3X)') 
C     -    
C     -    Display fixed part of options menu 
         call jump_line(9)
         call clear_lines(3)
         write (6,'(2A)') '+',
     +    'Extract Options (Y or N for each, <CR> to update or accept)'
         write (6,'(1X,A)') 
     +      'Options:  |Gens|Relays|Remote|Load|Load|Load|Series|DC |'
         write (6,'(1X,A)') 
     +      '          |    |      |Relays|Rep |Net |Shed|Caps  |   |'
C           < 3 5 7 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 
C     -    
C     -    Set columns for Options keys to be written and read from
           keycol(9) = 15
           keycol(1) = 20
           keycol(2) = 27
           keycol(4) = 33
           keycol(8) = 38
           keycol(7) = 43
           keycol(3) = 49
           keycol(10) = 55
           keycol(5) = 0
           keycol(6) = 0
C     -    
C     -    Loop to show and accept extract options
      changes = .true.
      do while (changes)
         call jump_line(12)
         call clear_lines(2)
C        -
C        -   Display current options 
         crtout = '+Current:'
         do la = 1,10
            kolumn = keycol(la) 
            if (kolumn .ne. 0) crtout(kolumn:kolumn) = keych(la)
         enddo
         write (6,'(A)') crtout 
C        -    Prompt line for user changes 
         write (6,'(1X,A,$)') 'Changes: '
         read (5,'(A)') changech
         if (changech .eq. ' ') then 
            changes = .false.
         else
C           -    Process changes 
            kshift = 10
            do la = 1,10                   ! Fetch overwritten charcters
               kolumn = keycol(la) - kshift
               if (kolumn .gt. 0) keymod(la) = changech(kolumn:kolumn) 
               if (keymod(la) .eq. 'Y') then         ! Check for changes
                  key(la) = 1 
                  keych(la) = 'Y' 
               elseif (keymod(la) .eq. 'N') then 
                  key(la) = 0
                  keych(la) = 'N' 
               else 
                  continue 
               endif                        ! End indiv change checking
            enddo                         ! End loop for each key
         endif                         ! End of test for any changes
      enddo                        ! End of loop to update options I/O
C     -
C     -   Find out if all selections blank
C     -   This signals a full dump by setting all keys
      keysum=0
      do 55 i=1,10
 55      keysum=keysum+key(i)        ! Add number of options requested
      if(keysum.ne.0) go to 70
      do 60 i=1,10             ! If all options blank then use them all 
 60      key(i)=1
 70   continue
C     -
C               ***************    HEADER CARD     *******************
C     -
      read (1,end=99)  swdata, savout, swdate, pfdate    
                       !  Fetch name is stability file and creation date
      pfdate = ' '
      if (always) write (2,101) swdata, savout, swdate(1:9), pfdate
 101                      format (4a11)
      read (1,end=99)   ibxyz, (basekv(ii),ii=1,ibxyz)
                            ! Fetch number of base kV's and list of them
      if (never) write (2,102) ibxyz, (basekv(ii),ii=1,ibxyz)
 102                      format (i6,(6f6.1))

C               **************   LOCAL RELAY DATA  *******************
C     -
      read (1,end=99) dev1,dev2,datx,lrd    ! Fetch relay header and 
                                            ! number of relays
      if (dev1.eq.relay.and.dev2.eq.blnk10) go to 202
      write (2,201) relay,blnk10,dev1,dev2
 201     format (' BAD HEADER -- NEED '2a10', got '2A10)
 202  continue
      if (key(1) .eq. 1) write (2,103) relay,blnk10,datx,lrd
 103                      format (3a10,i6)
      if (lrd.eq.0) go to 680     ! Relays are stored directly in their
                                  ! card image format
      if (lrd.le.100) go to 640
      read (1,end=99)  (lrelay(j),j=1,100)   ! Get 100 relays in 
                                             ! first batch
      if (key( 1).eq.1) then
         write (2,10) (lrelay(j)(1:last_char(lrelay(j),80)),j=1,100)
      endif
  640 continue
      ipart = mod(lrd,100)
      read (1,end=99)  (lrelay(ii),ii=1,ipart)  ! Get rest of relays
      if (key( 1).eq.1) then
         write (2,10) (lrelay(ii)(1:last_char(lrelay(ii),80)),
     +     ii=1,ipart)
      endif
C
C               *************   REMOTE RELAY DATA   ******************
C     -
 680  read (1,end=99)  dev1,dev2,datx,lrr     ! Fetch remote relay 
                                       ! header and number of relays
      if (dev1.eq.remote.and.dev2.eq.relay) go to 203
      write (6,201) remote,relay,dev1,dev2
 203  if (key(2) .eq. 1)    write (2,106) remote,relay,datx,lrr
 106                        format (3a10,i6)
      if (lrr.eq.0) go to 700 
      read (1,end=99) (lrrod(ii),ii=1,lrr)  ! Fetch all of them at once
      if (key( 2).eq.1)  then
         write (2,10) (lrrod(ii)(1:last_char(lrrod(ii),80)),ii=1,lrr)
                    ! Remote relays stacked in card image
      endif
C     - 
C               *************  SERIES CAPACITOR DATA  ****************
C     -
 700  read (1,end=99) dev1,dev2,datx,lsc    ! Fetch series cap header
                                            ! and number of series caps
      if (dev1.eq.series.and.dev2.eq.capac) go to 204
      write (6,201) series,capac,dev1,dev2
 204  continue
      if (key(3) .eq. 1) write (2,108) series,capac,datx,lsc
 108                      format (3a10,i6)
      if (lsc.eq.0) go to 720
      read (1,end=99)  (lsrcp(ii),ii=1,lsc)    ! Fetch all of them
      if (key( 3).eq.1)  then 
         write (2,10) (lsrcp(ii)(1:last_char(lsrcp(ii),80)),ii=1,lsc)
                                 ! Series cap data stacked in card image
      endif
C     -
C               ********* LOAD REPRESENTATION SORT BY AREA ***********
C     -
 720  read (1,end=99) dev1,dev2,datx,ldar  ! Fetch area load rep header 
                                          ! and number of load rep cards
      if (dev1.eq.load.and.dev2.eq.repr) go to 205
      write (6,201) load,repr,dev1,dev2
 205  if (key(4) .eq. 1) write (2,110) load,repr,datx,ldar
 110                      format (3a10,i6)
      if (ldar.eq.0) go to 780
      read (1,end=99) (lrepdt(ii),ii=1,ldar)  ! Fetch all of them
      if (key( 4).eq.1)  then
         write (2,10) (lrepdt(ii)(1:last_char(lrepdt(ii),80)),ii=1,ldar)
                                   ! Load rep data stacked in card image
      endif
C     -
C               ********* LOAD REPRESENTATION SORT BY ZONE ***********
C     -
 780  read (1,end=99)  dev1,dev2,datx,ldz  ! Fetch zone load rep header
                                          ! and number of load rep cards
      if (dev1.eq.load.and.dev2.eq.repr) go to 206
      write (6,201) load,repr,dev1,dev2
 206  continue
      if (key(4) .eq. 1)  write (2,112) load,repr,datx,ldz
 112                       format (3a10,i6)
      if (ldz.eq.0) go to 840
      if (ldz.gt.100) go to 820
C     -
C     -  Loop to read load rep cards in batches of 100
  800 continue
      read (1,end=99) (lrepdt(ii),ii=1,ldz)   ! Read all zone load rep 
                                        ! data if less than 100 cards
      if (key( 4).eq.1)  then
         write (2,10) (lrepdt(ii)(1:last_char(lrepdt(ii),80)),ii=1,ldz)
      endif
      go to 840

  820 continue
      i100=100
      read (1,end=99) (lrepdt(ii),ii=1,i100)    ! Read no more than 100 
                                             ! load rep cards at a time
      if (key( 4).eq.1)  then
         write (2,10) (lrepdt(ii)(1:last_char(lrepdt(ii),80)),ii=1,i100)
                              ! Load rep cards are stacked in card image
      endif
      ldz=ldz-100             ! Now have 100 less load rep cards to read
      go to 800               ! Get next 100 or less
C     -   End of load rep data rading loop
C     -
C               ********* LOAD REPRESENTATION SORT BY BUS  ***********
C     -
 840  read (1,end=99)  dev1,dev2,datx,lrep       ! Fetch individual bus 
                          ! load rep header and number of load rep cards
      if (dev1.eq.load .and. dev2.eq.repr) go to 207
      write (6,201) load,repr,dev1,dev2
 207  continue
      if (key(4) .eq. 1) write (2,115) load,repr,datx,lrep
 115                      format (3a10,i6)
      if (lrep.eq.0) go to 940
      if (lrep.lt.100) go to 920
 880  iblock=lrep/100
          ! Fetch bus load rep data in blocks of 100 buses
      do 900  i=1,iblock
         read (1,end=99)  (lrepdt(j),j=1,100)
         if (key( 4).eq.1) then
            write (2,10) (lrepdt(j)(1:last_char(lrepdt(j),80)),j=1,100)
                    ! Bus load rep data is stacked in card image
         endif
  900 continue
  920 ipart = mod(lrep,100)                   ! Less than 100 buses left
      if (ipart.eq.0) go to 940
      read (1,end=99)   (lrepdt(ii),ii=1,ipart)     ! Fetch all the rest
      if (key( 4).eq.1)  then
         write (2,10) (lrepdt(ii)(1:last_char(lrepdt(ii),80)),
     +      ii=1,ipart)
      endif
C     - 
C               ***************  LOAD SHEDDING DATA   ****************
C     - 
 940  read (1,end=99)   dev1,dev2,datx,ls  ! Fetch load shed header and 
                                           ! number of load shed specs
      if (dev1.eq.load.and.dev2.eq.shed) go to 208
      write (6,201) load,shed,dev1,dev2
 208  continue
      if (key(7) .eq. 1)  write (2,118) load,shed,datx,ls
 118                       format (3a10,i6)
      if (ls.eq.0) go to 1040
      if (ls.lt.100) go to 1020
 980  iblock=ls/100
      do 1000 i=1,iblock          ! Fetch 100 load shed specs at a time
         read (1,end=99)      (lshed(j),j=1,100)
         if (key( 7).eq.1) then
            write (2,10) (lshed(j)(1:last_char(lshed(j),80)),j=1,100)
         endif
 1000 continue
 1020 ipart = mod(ls,100)                    !  Less than 100 specs left
      if (ipart.eq.0) go to 1040
      read (1,end=99) (lshed(ii),ii=1,ipart)    ! Fetch the rest of them
      if (key( 7).eq.1)  then
         write (2,10) (lshed(ii)(1:last_char(lshed(ii),80)),ii=1,ipart)
      endif
C     -
C               ***************  LOAD NETTING  DATA   ****************
C     -
 1040 read (1,end=99)  dev1,dev2,datx,ln,ibxyz   ! Fetch load net header
                                          ! and number of load net buses
      if (dev1.eq.load.and.dev2.eq.net) go to 209
      write (6,201) load,net,dev1,dev2
 209  continue
      if (key(8) .eq. 1)  write (2,121) load,net,datx,ln,ibxyz
 121                       format (3a10,2i6)
      if (ln.eq.0) go to 1060
      read (1,end=99) (lnetc(i),i=1,ln), (lnetn(i),i=1,ln),
     1   (basekv(ii),ii=1,ibxyz)   ! Fetch all load netted buses at once
C     -   Loop to display all load netted buses, one per output record
      if (key(8) .eq. 1) then 
         do 1045 ii=1,ln
            lndx=lnetn(ii)               ! Get pointer in base kV array
            bkvnet(ii) = basekv(lndx)        ! Get base kV for this bus
C           -  
C           -    The format will vary depending on value of BKVNET
C           
            if     (bkvnet(ii).lt. 10.0) then        ! e.g, "4.16"
               write (2,'(A3,A8,F4.2)') 'LN ', lnetc(ii),bkvnet(ii)
            elseif (bkvnet(ii).lt. 100.) then        ! e.g. "13.8"
               write (2,'(A3,A8,F4.1)') 'LN ', lnetc(ii),bkvnet(ii)
            else                                     ! e.g. "230."
               write (2,'(A3,A8,F4.0)') 'LN ', lnetc(ii),bkvnet(ii)
            endif
C           -  Old code replaced Sep/04/92 
C           IF (BKVNET(II).LT.100.) THEN
C              WRITE (2,122) LNETC(II),BKVNET(II)
C122              FORMAT ('LN ',A8,F4.1)
C           ELSE
C              WRITE (2,222) LNETC(II),BKVNET(II)
C222              FORMAT ('LN ',A8,F4.0)
C           ENDIF
 1045    continue
      endif
C
      if (never) then            ! If extraction not desired, display
                                 ! data in semi-raw form
         write (2,322) (lnetc(ii),ii=1,ln)
 322                    format (5a8)
         write (2,422) (lnetn(ii),ii=1,ln)
 422                    format (5i8)
         write (2,522) (basekv(ii),ii=1,ibxyz)
 522                    format (5f8.1)
      endif
C     -
C               *****************   PLANT DATA     *******************
C     -
 1060 read (1,end=99)  dev1,dev2,datx,mac     ! Fetch machine header and
                                              ! number of machine cards
      if (dev1.eq.machn .and. dev2.eq.blnk10) go to 210
      write (6,201) machn,blnk10,dev1,dev2
 210  continue
      if (key(9) .eq. 1)  write (2,123) machn,blnk10,datx,mac
 123  format (3a10,i6)
      do while (mac .gt. 0)
         if (mac .ge. 100) then    ! We will fetch up to 100 at a time
            bunch = 100
         else
            bunch = mac
         endif
         read (1,end=99) (machin(la),la=1,bunch)     ! Read a bunch of 
                                                     ! machine cards 
         if (key(9) .eq. 1) then           !  If user wants machine data
            do la = 1,bunch
C              -   Exciter types chnaged from extended 'E' set to WSCC 
C              -     'F' set. 
               if (machin(la)(1:1) .eq. 'E') 
     +            call exciter_type (machin(la))
               write (2,10) machin(la)(1:last_char (machin(la),80))
            enddo                            ! Done copying this bunch
         endif                         ! Done with extraction test
         mac = mac - bunch          ! Num of machine cards left to copy
      enddo                    ! Restart loop to copy next bunch
C     -
C               ***************  DIRECT CURRENT DATA  ****************
C     -
 1180 read (1,end=99)   dev1,dev2,datx,jdc   ! Fetch DC header data and 
                                         ! number of converter stations
      if (dev1.eq.direct .and. dev2.eq.currnt) go to 211
      write (6,201) direct,currnt,dev1,dev2
 211  continue
      if (keysum.eq.0)  write (2,126) direct,currnt,datx,jdc
 126                       format (3a10,i6)  ! If extraction not desired
                                          ! then only display statistics
      if (jdc.eq.0) go to 1220
      read (1,end=99) (dccard(ii),ii=1,jdc)   ! Get all DC cards at once
                                              ! up to 60
      if (key(10).eq.1)  then
         write (2,10) (dccard(ii)(1:last_char(dccard(ii),80)),ii=1,jdc)
      endif
C     -
C     -   That's all the data for this binary file 
C     -
 1220 continue
      if (keysum.eq.0) write (2,128)
 128                      format (20x,'<  DONE  >') 

  99   close(unit=2)

      end                         ! End of SWINGAUX program

C   -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=- 

      subroutine clear_screen
C     -
C     -  Erases an entire CRT screen
      character csi*2, clear*4
C     -  
      csi = char(27)//'['
      clear = csi // '2J'
      write (6,'(1X,A,$)') clear
      return
      end

C   -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=- 

      subroutine clear_lines(num_lines)
C     -
C     -  Erases NUM_LINES number of lines on a CRT, counting the 
C     -  current line.  The cursor is returned to its starting 
C     -  position. 
C     -
      character csi*2, clrline*4, curdown*4, curup*4
C     -  
      csi = char(27)//'['
      clrline = csi // '2K'
      curdown = csi // '1B'
      curup = csi // '1A'
      do la = 1,num_lines
        write (6,'(1X,A)') clrline 
      enddo
      do la = 1,num_lines
        write (6,'(2A)') '+',curup
      enddo
      return
      end

C   -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=- 

      subroutine jump_line(line_num)
C     -
C     -  Moves CRT cursor to the left end of LINE_NUM. 
C     -
      character csi*2, goline*3, curdown*3 
C     -  
      csi = char(27)//'['
      goline =  '00H'
      if (line_num .lt. 10) write (goline(2:2),'(I1)') line_num
      if (line_num .ge. 10) write (goline(1:2),'(I2)') line_num
      curdown = csi // 'B'
      write (6,'(1X,2A,$)')  csi,goline
      return
      end
      function last_char(string,max)
C     -                    in    in
C     -    Find and return position of last non-blank character in 
C     -      STRING, whose maximum length is MAX
      character*80 string
      do  i = max,2,-1
         if (string(i:i).ne.' ') goto 200
      enddo
 200  last_char = i
      return
      end                            !  End of LAST_CHAR

C   -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=- 

      subroutine exciter_type (macdat) 
C     -                        in/out
C     -   This subroutine changes the type of an exciter card if it is 
C     -      in the range of EL to EX to corresponding FA to FL and FZ 
C     -      of WSCC types. 
      character macdat*(*)
      logical unmatched
      character oldtype(12)*2, newtype(12)*2
      data oldtype / 'EM','EN','EO','EP','EQ','ER','ES','ET','EU',
     +   'EV','EW','EX' /
      data newtype / 'FA','FB','FC','FD','FE','FF','FG','FH','FJ',
     +   'FK','FL','FZ' / 
C     -   
C     -   Begin 
      la = 1
      unmatched = .true.
      do while (la .le. 12  .and. unmatched)
         if (macdat(1:2) .eq. oldtype(la)) then
            macdat(1:2) = newtype(la)
            unmatched = .false.
         endif
      la = la + 1
      enddo
C     -   
      return
      end 

C   -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=- 

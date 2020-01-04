C    %W% %G%
      subroutine calplt 
C    
C     THIS SUBROUTINE FORMS DATA TABLES REQUIRED FOR PLOTTING.
C     IT IS CALLED BY SWINGM AND CALLS PLTCOM, MVPLT, BUSPLT
C     AND PLT
C    
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/lodbus.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/out512.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/dc.inc'
      include 'tspinc/ovly6.inc'
      include 'tspinc/nwgntn.inc'
      include 'tspinc/lindat.inc'
      include 'tspinc/newtab.inc'
      include 'tspinc/jbusdt.inc'
      include 'tspinc/idgen.inc'
      include 'tspinc/igend1.inc'
      include 'tspinc/dcname.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/room.inc'
      include 'tspinc/mvn.inc'
      include 'tspinc/spare1.inc'
      include 'tspinc/spare2.inc'

      dimension ylmt(2), sca(6), isub(3), xsymt(2), ysymt(2), nsymt(2),
     &          igrpdc(20), msub1(2), idx1(3), idx2(3)
      equivalence (msub1, msub)
      character*10 yzplt(6), ybus(4), ylin(6), ydc(18), ylabl(28)
      character*10 modlbl(14)
      character*8 ib1c, ib2c, ibdumc
      character*3 end

      real divisor, dividend, rem
      logical plotop
 
C     YLABL IS THE TABLE CONTAINING ALL Y-AXIS LABELS
C     APPARENT IMPEDANCE GRAPH

      data yzplt/'   APPAREN', 'T Z-IMAG  ', '   APPAREN', 'T Z-DOT',
     &     '   apparen', 'T   R-DOT   '/

C     BUS VOLTAGE AND FREQUENCY GRAPH

      data ybus/'   BUS VOL', 'TAGE PU   ', '  BUS FREQ', ' DEV HERTZ'/

C     LINE MW AND MVAR GRAPHS

      data ylin/'    LINE F', 'LOW MW    ', '   LINE FL', 'OW MVAR',
     &     '   LINE CU', 'RRENT AMP '/

C     DC FIRING ANGLE, CURRENT AND POWER

      data ydc/'FIRING ANG', 'LE DEGREES', 'DC CURRENT', ' KILOAMPS ',
     &     '      dc v', 'OLTS KV   ', 'MODSIG1   ', 'MODSIG2   ',
     &     'EXTINCTION', ' ANGLE DEG', ' V ALPHA P', 'ER UNIT   ',
     &     'V ALPHA PR', 'IME PU    ', 'OVERLAP AN', 'GLE DEG   ',
     &     'DC POWER  ', 'MW        '/

C     GENERATOR GRAPHS

      data ylabl/'REL ANGLE ', 'DEGREES   ', ' GEN FREQ ', 'DEV HERTZ '
     &     , '    FIELD ', 'VOLTS PU  ', '     FLUX ', 'LINKAGE   ',
     &     '  MAIN FIE', 'LD SAT PU ', '   TURBINE', ' POWERMW  ',
     &     ' GENERATOR', ' POWER MW ', ' EXCITATIO', 'N SAT PU  ',
     &     ' REG OUTPU', 'T VOLT PU ', ' ACCELERAT', 'ING PWR MW',
     &     '   GENERAT', 'OR MVAR   ', 'EXCITER SU', 'PP SIG PU ',
     &     ' TORQUEDAM', 'PING MW   ', ' FIELD CUR', 'RENT PU'/
 
      data xsymt(1), xsymt(2)/10.0, 1.5/
      data ysymt(1), ysymt(2)/10.0, 10.0/
      data nsymt(1), nsymt(2)/10, 5/
      data end/'END'/
      data modlbl/'LOW LEVEL ', 'P INPUT KA', 'LOW LEVEL ',
     &     'I INPUT KA', 'HIGH LEVEL', 'P INPUT MW', 'HIGH LEVEL',
     &     'I INPUT MW', 'GAMMA MOD ', '- DEGREES ', 'DUAL FREQU',
     &     'ENCY -  MW', 'EXTINCTION', ' ANG - DEG'/
 
      plotop(ind) = ind .eq. 2 .or. ind .eq. 3 .or. ind .eq. 6 .or. 
     &              ind .eq. 7 .or. ind .eq. 12 .or. ind .eq. 13
 
      call mpost('CALPLT')
      rewind l11
      call forbtm()
      call fortop()
      write (outbuf, 10000)
      call prtout(1)
      call skipln(1)
10000 format ('0SUBROUTINE CALPLT')
 
C     INITIALIZE PLOT/PRINT OPTION
      ipltsw = 0

C     NUMBER OF TIME STEPS PLUS TWO FOR THE SCALING
      icoun2 = icount + 2

C     DEFAULT TIME MAXIMUM AND MINIMUM
      if (tmax .le. 0.0) tmax = t(icount)
      tmin = t(icount+2)

C     USER SUPPLIED TMAX.  NO ERROR CHECKING.
      if (t(icount) .gt. tmax) then
 
C       FIND THE HIGHEST TIME STEP UNDER TMAX.
C       IF THERE ARE NO TIME STEPS UNDER TMAX, THIS IS AN ERROR.
        i = icount
        do while (.true.)
          i = i - 1
          if (t(i) .le. tmax) goto 100
        enddo
  100   if (t(i) .le. 0.0) then
          write (errbuf(1), 10010)
10010     format ('Error in TMAX, program stopped')
          call prterr('E', 1)
          call erexit()
        endif
 
        icount = i
      endif

C     USER SUPPLIED CYCLES PER AXIS INCH
      dt = edt

C     ADJUST TMAX UP SO THAT WE GET EVEN INCREMENTS OF DT.
c     fix a bug on unix by breaking down call to function amod
c      tmax = tmax + (dt-amod(tmax, dt))
      dividend = tmax
      divisor = dt
      rem = amod(dividend, divisor)
      tmax = tmax + rem

C     THE AXIS IS AT LEAST 6 INCHES LONG.
      if (tmax .lt. dt*6.) tmax = dt*6.

C     LENGTH OF THE AXIS IN INCHES
      tlenth = tmax/dt

C     SET IN THE SCALING FOR LINE PLOTTING.
      t(icount+1) = tmin
      t(icount+2) = dt
    
C     CALL PLTCOM TO PLOT COMMENTS, LS CARDS, AND WORST CASE
C     VOLTAGES AND FREQUENCIES
    
      call pltcom()
    
C     CALL MVPLT IF ANY TWO OPTION PLOTS HAVE BEEN REQUESTED
    
      if (mvkt .gt. 0) call mvplt()
    
C     IF SPARE DATA POINTS HAVE BEEN REQUESTED, CALL SPTPLT TO
C     PLOT THEM
    
      if (ispknt .gt. 0) call sptplt()

C     IF NO GENERATORS TO OUTPUT GO ON TO PLOTTING BUS VARIABLES
    
      if (jtot .gt. 0) then
 
 
C       ******* PLOT GENERATOR VARIABLES ********
 
C       <> THE PACKED ARRAY IGEND1 WAS WRITTEN AT IGECS1.  <>
C       <> THE NEW ARRAY IS IGEND1(31,MAXGEN).             <>
C       IGEND1(1,I) . . . . . . . . . . . USER-NAMED GROUP CODE
C       IGEND1(17,I)-IGEND1(30,I) . . . . OUTPUT INDICATORS
C       IGEND1(31,I)  . . . . . . . . . . NOT USED.
C       IGEND1(2,I)-IGEND1(16,I)  . . . . NOT USED.
 
C       LOOP FOR PLOTTING EACH TYPE OF
C       GENERATOR VARIABLE.

        do ind = 1, 14

C         SWITCH FOR USER-SUPPLIED MAX AND MIN
          isw = 0
          ihdsw = 0
 
C         LOOP TO FIND THE USER-REQUESTED PLOT
C         FOR SETTING THE MAXIMUM AND MINIMUM.

          do jj = 1, 14

C           LOOK FOR THE PLOT INDICATOR
            if (igencl(jj) .eq. ind) goto 110
          enddo
          goto 120

C         NO USER-SUPPLIED MAX AND MIN, SO ISW=0, UNLESS
C         PLOT TYPE NO. 1.
  110     if (genmax(jj) .ne. 0.0 .or. genmin(jj) .ne. 0.0) then

C           IF THERE IS A GENMAX, THEN ASSUME BOTH MAXIMUM AND MINIMUM 
C           ARE USER-SUPPLIED. THEREFORE ISW=1.

            if (genmax(jj) .eq. 0.0) then

C             IF ANYTHING IS PUNCHED IN THE MAX COLUMN
              if (sign(1., genmax(jj)) .le. 0.0) then

C               IF ANYTHING IS PUNCHED IN THE MIN COLUMN
                if (sign(1., genmin(jj)) .le. 0.0) goto 120

C               THEN NOT USER-SUPPLIED, ISW=0
              endif
            endif

C           GET THE USER-SUPPLIED VALUES
            ymin = genmin(jj)
            ymax = genmax(jj)

C           UNITS PER INCH, ON 8-INCH AXIS
            scdiv = (ymax-ymin)/8.0
            valmin = ymin
            kxp = 1
            isw = 1

C           NUMBER OF INCHES ABOVE ZERO
            py =  - valmin/scdiv
            if (ymax .eq. 0.0) py = 8.0
          endif
 
C         FOR GENERATOR-ANGLE PLOT.  DEFAULTS ARE 50 DEGREES PER INCH, 
C         MIN -200 DEG., MAX +200 DEG. ZERO LINE IS IN CENTER AND ISW 
C         IS ALWAYS ONE.
  120     if ((ind .eq. 1) .and. (isw .ne. 1)) then
            scdiv = 50.
            valmin =  - 200.0
            ymin =  - 200.0
            ymax = 200.
            py = 4.0
            kxp =  - 1
            isw = 1
          endif
        
C         FOR FREQUENCY DEVIATION, YMAX = .8 YMIN = -.8
C         UNLESS LIMITS HAVE BEEN ENTERED ON THE GH CARD
        
          if ((ind .eq. 2) .and. (isw .ne. 1)) then
            scdiv = .2
            valmin =  - .8
            ymin =  - .8
            ymax = .8
            py = 4.0
            kxp =  - 1
            isw = 1
          endif
        
C         FOR ACCELERATING POWER, LIMITS ARE YMAX = 400 YMIN = -40
C         UNLESS LIMITS HAVE BEEN  ENTERED ON THE GH CARD
        
          if ((ind .eq. 10) .and. (isw .ne. 1)) then
            scdiv = 100.
            valmin =  - 400.0
            ymin =  - 400.0
            ymax = 400.
            py = 4.0
            kxp =  - 1
            isw = 1
          endif
 
C         ISG IS THE NUMBER OF GENERATORS TO OUTPUT.
C         I DON'T THINK JADR IS NECESSARY; IT CAN BE
C         SET TO ZERO.
          jadr = isg + 1

C         NOGRP IS THE NUMBER OF DISTINCT GROUPS OF
C         GENERATORS THAT ARE TO BE PLOTTED SEPARATELY.
          do kgrp = 1, nogrp
            kount = 0

C           KTPLOT IS THE COUNTER OF PLOTS ON THIS AXIS
            ktplot = 0

C           IGRP IS THE USER-SUPPLIED GROUP NUMBER.
            igrp = igrup(kgrp)
            i = 0

C           I IS THE COUNTER OF THE GENERATORS PLOTTED.
C           ISG IS THE NUMBER OF GENS TO PLOT.
            do while (.true.)
              i = i + 1

C             DONE WITH GENERATORS IF I>ISG
              if (i .le. isg) then
C               <>  WE DON'T NEED TO CHECK ALL 31 ENTRIES OF  <>
C               <>  IGEND1.  THE TEST FOR ISW1 WILL GIVE THE  <>
C               <>  THE SAME RESULT.                          <>
                isw1 = igend1(ind+16, i)
C               ISW1 IS THE OUTPUT INDICATOR
C               ISW1 = 0 - NO OUTPUT
C               ISW1 = 1 - LIST
C               ISW1 = 2 - PLOT
C               ISW1 = 3 - LIST AND PLOT
C               ISW1 = 4 - AUXOUT
C               ISW1 = 5 - AUXOUT AND LIST
C               ISW1 = 6 - AUXOUT AND PLOT
C               ISW1 = 7 - AUXOUT PLOT AND LIST
                if (plotop(isw1)) then

C                 GET THE FILE WHERE TO PUT THE
C                 INFORMATION FOR THIS KIND OF PLOT.
                  mind = msindx(ind)

C                 KOUNT IS NOT NEEDED
                  kount = kount + 1

C                 SKIP IF NOT THE CURRENT GROUP NUMBER
                  if (igend1(1, i) .eq. igrp) then
                    ktplot = ktplot + 1
 
C                   <>  THE NEW ARRAYS ARE NWGNTN AND NWGNT.         <>
C                   GET THE GENERATOR NAME
 
                    idgenn(1, ktplot) = nwgntn(i)
                    idgenc(1, ktplot) = nwgntc(1, i)
                    idgenc(2, ktplot) = nwgntc(2, i)

C                   MSUB AND THIS STATEMENT ARE NOT NEEDED.
                    call redecp(msub(2*mind), ksub(ind)+kount-1, 1)

C                   PULL OUT THE NUMBERS THAT WE WANT TO PLOT.
C                   LOCATED ON FILE MIND, ICOUN2 VALUES, KEY=1.
                    call readmp(mind, work(jadr+1), icoun2, 1)

C                   KWORK IS THE STARTING ECS ADDRESS OF THE
C                   DATA WE ARE PUTTING INTO STORAGE.  THERE IS
C                   NO CONCERN WITH UPPER LIMITS ON USAGE.  IF
C                   KECST EXCEEDS THE ACCOUNTING MAX, KECMAX, BUMP
C                   UP KECMAX.
C                   KECS IS BIASED UP BY THE COUNT OF DATA TIMES
C                   THE NUMBER OF PLOTS.
                    kecs = kwork + (ktplot-1)*icoun2

C                   PUT THE DATA WE GOT FROM MASS STORAGE INTO ECS.
                    call ritecp(work(jadr+1), kecs, icoun2)
                    kecst = kecs + icoun2
                    if (kecst .gt. kecmax) kecmax = kecst
                    if (isw .eq. 1) then
 
C                     PROCESSING FOR USER-SUPPLIED MAX AND MIN . . .
C                     IN THIS LOOP, SIFT THRU THE DATA TO SEE IF IT
C                     STEPS OUT OF BOUNDS, THEN TRUNCATE IT.
                      do jj = 1, icount
                        jindx = jadr + jj
                        if (work(jindx) .gt. ymax) work(jindx) = ymax
                        if (work(jindx) .lt. ymin) work(jindx) = ymin
                      enddo
 
C                     PUT THE DATA INTO ECS.
C                     BY THE WAY, THIS CODE IS REDUNDANT.
C                     THE LOGIC AFTER STMT 280 SHOULD BE RECODED.
                      call ritecp(work(jadr+1), kecs, icoun2)
                      kecst = kecs + icoun2
                      if (kecst .gt. kecmax) kecmax = kecst
                    endif
                  endif
                endif

C               GO BACK AND AND PRE-PROCESS MORE PLOT INFORMATION
C               IF WE HAVENT YET REACHED THE PLOT MAXIMUM.
C               IF WE HAVE PROCESSED IPLTKT PLOTS, THEN GO ON TO
C               DO PLOTTING.
                if (ktplot .ne. ipltkt) goto 130
              endif

C             IF PLOTTING NOT ASKED FOR ON THIS GROUP, GO
C             ON TO NEXT GROUP.
              if (ktplot .eq. 0) goto 140

C             GET A COPY OF I IN CASE IT CHANGES.
              isave = i
              if (isw .ne. 1) then

C               THIS SECTION DOES AUTMATIC SCALING IF
C               THE MAX AND MIN ARE NOT SUPPLIED IN DATA.
                ymax = 0.0
                ymin = 0.0
                do i = 1, ktplot

C                 LOOP THROUGH THE PLOT DATA TO GET THE MAXIMA
C                 AND MINIMA FOR ALL PLOTS, TO FIND THE MAXIMUM
C                 MAXIMUM AND THE MINIMUM MINIMUM.  THIS IS SO
C                 THAT THEY ALL CAN FIT ON THE SAME GRAPH.
                  kecs = kwork + i*icoun2 - 2

C                 GET THE MAX AND MIN VALUES FOR EACH PLOT
C                 BACK OUT OF ECS. WAS STORED ABOVE.
                  call redecp(ylmt, kecs, 2)

C                 ADJUST OUR MAXIMUM AND MINIMUM SO THAT THEY
C                 COVER THE RANGE OF MAXIMA AND MINIMA FOR ALL PLOTS.
                  if (ylmt(1) .gt. ymax) ymax = ylmt(1)
                  if (ylmt(2) .lt. ymin) ymin = ylmt(2)
                enddo
                smax = ymax
                smin = ymin
                trnge = smax - smin
C              
C               GO GET THE MAXIMUM, MINIMUM, AND UNITS PER INCH.
C              
                call scal()
              endif
C            
C             NOW FILL IN THE AXIS LABELS AND DO THE GRAPH.
C            
              dy = scdiv
              id1 = 2*ind - 1
              yaxe(1) (1:10) = ylabl(id1)
              yaxe(1)(11:20) = ylabl(id1+1)
              call plt()

C             FINISHED WITH THE LAST GENERATOR
              if (isave .gt. isg) goto 140
              jadr = isg + 1
              i = isave
              ktplot = 0
  130         continue
            enddo
  140       continue
          enddo
        enddo
      endif
C    
C     CALL BUSPLT TO PLOT REAL AND REACTIVE BUS LOADS
C    
      if (itotmw .ne. 0 .and. itotvr .ne. 0) call busplt()
      if (.not. ((ktbus .eq. 0) .and. (ktbus1 .eq. 0))) then
 
C       PLOT BUS VARIABLES  
 
C       ISUB MAY NOT BE NEEDED
        isub(1) = ksub13
        isub(2) = ksb13c

C       JADR MAY NOT BE NEEDED
        jadr = nmx + 1
        ibussw = 0
        ipltsw = 1
 
C       GO THROUGH MAJOR LOOP TWICE, FIRST FOR BUS VOLTAGE, NEXT 
C       FOR BUS FREQUENCY
        do while (.true.)
          ibussw = ibussw + 1
          isw = 0
          ihdsw = 0
 
C         LOOP TO FIND THE USER-REQUESTED PLOT
C         FOR SETTING THE MAXIMUM AND MINIMUM.
          do jj = 1, 2

C           LOOK FOR THE PLOT INDICATOR
            if (ibuscl(jj) .eq. ibussw) then

C             NO USER-SUPPLIED MAX AND MIN, SO ISW=0, UNLESS
C             PLOT TYPE NO. 1.
              if ((busmax(jj) .eq. 0.0) .and. (busmin(jj) .eq. 0.0))
     &         goto 150

C             IF THERE IS A BUSMAX, THEN ASSUME BOTH
C             MAXIMUM AND MINIMUM ARE USER-SUPPLIED.
C             THEREFORE ISW=1.
              if (busmax(jj) .eq. 0.0) then

C               IF ANYTHING IS PUNCHED IN THE MAX COLUMN
                if (sign(1., busmax(jj)) .le. 0.0) then

C                 IF ANYTHING IS PUNCHED IN THE MIN COLUMN
                  if (sign(1., busmin(jj)) .le. 0.0) goto 150

C                 THEN NOT USER-SUPPLIED, ISW=0
                endif
              endif

C             GET THE USER-SUPPLIED VALUES
              ymin = busmin(jj)
              ymax = busmax(jj)

C             UNITS PER INCH, ON 8-INCH AXIS
              isw = 1
              scdiv = (ymax-ymin)/8.
              valmin = ymin
              kxp = 3

C             NUMBER OF INCHES ABOVE ZERO
              py =  - valmin/scdiv
            endif
          enddo
 
C         NOGRP IS THE NUMBER OF DISTINCT
C         GROUPS OF BUSSES TO BE PLOTTED SEPARATELY.
  150     continue
          do kgrp = 1, nogrp
            kount = 0

C           KTPLOT IS THE COUNTER OF PLOTS ON THIS AXIS
            ktplot = 0

C           IGRP IS THE USER-SUPPLIED GROUP NUMBER.
            igrp = igrup(kgrp)
            i = 0

C           I IS THE COUNTER OF THE BUSSES PLOTTED.
C           NMX IS THE NUMBER OF BUSSES TO PLOT.
            do while (.true.)
              i = i + 1

C             DONE WITH BUSSES IF I>NMX
              if (i .le. nmx) then

C               <>  WANT TO TEST THE OPTIONS STORED IN  <>
C               <>  JBUSDT(3,I) AND JBUSDT (5,I).       <>
                isw1 = jbusdt(ibussw+1, i)
C               ISW1 IS THE OUTPUT INDICATOR
C               ISW1 = 0 - NO OUTPUT
C               ISW1 = 1 - LIST
C               ISW1 = 2 - PLOT
C               ISW1 = 3 - LIST AND PLOT
                if (plotop(isw1)) then

C                 ISW1 = 2 OR 3 HERE, MEANING TO DO A PLOT.
C                 GET THE FILE WHERE TO PUT THE
C                 INFORMATION FOR THIS KIND OF PLOT.
C                 KOUNT MAY NOT BE NEEDED
                  kount = kount + 1

C                 SKIP IF NOT THE CURRENT GROUP NUMBER.
                  if (jbusdt(1, i) .eq. igrp) then
                    ktplot = ktplot + 1

C                   <>  THE PACKED ARRAY WRITTEN AT KNEWT WAS NEWTAB.
C                   <>  THE NEW ARRAYS ARE NEWTBC AND INWTB.          <>
C                   GET THE BUS NAME.
                    idgenc(1, ktplot) = newtbc(i)
                    idgenn(1, ktplot) = inwtb(i)

C                   ISUB,MSUB, AND THIS STATEMENT ARE NOT NEEDED.
                    call redecp(msub(26), isub(ibussw)+kount-1, 1)

C                   PULL OUT THE NUMBERS THAT WE WANT TO PLOT.
C                   LOCATED ON FILE MIND, ICOUN2 VALUES, KEY=1.
                    call readmp (13, work(jadr+1), icoun2, 1)

C                   KWORK IS THE STARTING ECS ADDRESS OF THE
C                   DATA WE ARE PUTTING INTO STORAGE.  THERE IS
C                   NO CONCERN WITH UPPER LIMITS ON USAGE.  IF
C                   KECST EXCEEDS THE ACCOUNTING MAX, KECMAX, BUMP
C                   UP KECMAX.
C                   KECS IS BIASED UP BY THE COUNT OF DATA TIMES
C                   THE NUMBER OF PLOTS.
                    kecs = kwork + (ktplot-1)*icoun2

C                   PUT THE DATA WE GOT FROM MASS STORAGE INTO ECS.
                    call ritecp(work(jadr+1), kecs, icoun2)
                    kecst = kecs + icoun2
                    if (kecst .gt. kecmax) kecmax = kecst
                    if (isw .eq. 1) then
 
C                     PROCESSING FOR USER-SUPPLIED MAX AND MIN . . .
C                     IN THIS LOOP, SIFT THRU THE DATA TO SEE IF IT
C                     STEPS OUT OF BOUNDS, THEN TRUNCATE IT.
                      do jj = 1, icount
                        jindx = jadr + jj
                        if (work(jindx) .gt. ymax) work(jindx) = ymax
                        if (work(jindx) .lt. ymin) work(jindx) = ymin
                      enddo

C                     PUT THE DATA INTO ECS.
C                     BY THE WAY, THIS CODE IS REDUNDANT.
C                     THE LOGIC AFTER STMT 280 SHOULD BE RECODED.
                      call ritecp(work(jadr+1), kecs, icoun2)
                      kecst = kecs + icoun2
                      if (kecst .gt. kecmax) kecmax = kecst
                    endif
                  endif
                endif

C               GO BACK AND AND PRE-PROCESS MORE PLOT INFORMATION
C               IF WE HAVENT YET REACHED THE PLOT MAXIMUM.
C               IF WE HAVE PROCESSED IPLTKT PLOTS, THEN GO ON TO
C               DO PLOTTING.
                if (ktplot .ne. ipltkt) goto 160
              endif

C             IF PLOTTING NOT ASKED FOR ON THIS GROUP, GO
C             ON TO NEXT GROUP.
              if (ktplot .eq. 0) goto 170

C             GET A COPY OF I IN CASE IT CHANGES
              isave = i
              if (isw .ne. 1) then

C               THIS SECTION DOES AUTMATIC SCALING IF
C               THE MAX AND MIN ARE NOT SUPPLIED IN DATA.
                ymax = 0.0
                ymin = 0.0
                do i = 1, ktplot

C                 LOOP THROUGH THE PLOT DATA TO GET THE MAXIMA
C                 AND MINIMA FOR ALL PLOTS, TO FIND THE MAXIMUM
C                 MAXIMUM AND THE MINIMUM MINIMUM.  THIS IS SO
C                 THAT THEY ALL CAN FIT ON THE SAME GRAPH.
                  kecs = kwork + i*icoun2 - 2

C                 GET THE MAX AND MIN VALUES FOR EACH PLOT
C                 BACK OUT OF ECS. WAS STORED ABOVE.
                  call redecp(ylmt, kecs, 2)

C                 ADJUST OUR MAXIMUM AND MINIMUM SO THAT THEY
C                 COVER THE RANGE OF MAXIMA AND MINIMA FOR ALL PLOTS.
                  if (ylmt(1) .gt. ymax) ymax = ylmt(1)
                  if (ylmt(2) .lt. ymin) ymin = ylmt(2)
                enddo
                smax = ymax
                smin = ymin
                trnge = smax - smin

C               DONT PLOT THIS GROUP, IF THE RANGE IS CLOSE TO ZERO
                if (abs(trnge) .lt. .001) goto 170

C               GO GET THE MAXIMUM, MINIMUM, AND UNITS PER INCH.
                call scal()
              endif

C             NOW FILL IN THE AXIS LABELS AND DO THE GRAPH
              dy = scdiv
              yaxe(1) (1:10) = ybus(2*ibussw-1)
              yaxe(1)(11:20) = ybus(2*ibussw)
              call plt()

C             NO MORE BRANCHES TO PLOT.
              if (isave .gt. nmx) goto 170
              ktplot = 0
              jadr = nmx + 1
              i = isave

C             GO GET THE NEXT LINE TO PLOT
  160         continue
            enddo
  170       continue
          enddo

C         GO ON TO PLOT FREQUENCY VARIABLE
          if (ibussw .ne. 1) goto 180
        enddo
      endif
  180 ktl = ktnol1 + ktnol2 + ktnol4
      if (ktl .gt. 0) then
 
 
C       PLOT LINE VARIABLES  
 
        linekt = ildatt

C       ISUB MAY NOT BE NECESSARY
        isub(1) = ksb13a
        isub(2) = ksb13b
        isub(3) = ksb13e

C       JADR MAY NOT BE NECESSARY
        jadr = 701
        ilinsw = 0
        ipltsw = 2
        idx1(1) = 2
        idx1(2) = 2
        idx1(3) = 3
        idx2(1) = 3
        idx2(2) = 5
        idx2(3) = 3
 
C       LOOP THRU MAIN LOOP THREE TIMES. ILINSW=1 FOR MW'S
C       ILINSW=2 FOR MVAR'S. ILINSW=3 FOR LINE CURRENT
 
        do while (.true.)
          ilinsw = ilinsw + 1
          isw = 0
          ihdsw = 0
 
C         LOOP TO FIND THE USER-REQUESTED PLOT
C         FOR SETTING THE MAXIMUM AND MINIMUM.
          do jj = 1, 3

C           LOOK FOR THE PLOT INDICATOR.
            if (ibrncl(jj) .eq. ilinsw) then

C             NO USER-SUPPLIED MAX AND MIN, SO
C             ISW=0, UNLESS PLOT TYPE NO. 1.
              if ((brnmax(jj) .eq. 0.0) .and. (brnmin(jj) .eq. 0.0))
     &         goto 190

C             IF THERE IS A BRNMAX, THEN ASSUME BOTH
C             MAXIMUM AND MINIMUM ARE USER-SUPPLIED.
C             THEREFORE ISW=1.
              if (brnmax(jj) .eq. 0.0) then

C               IF THERE IS ANYTHING PUNCHED IN THE MAX COLUMN
                if (sign(1., brnmax(jj)) .le. 0.0) then

C                 IF ANYTHING IS PUNCHED IN THE MIN COLUMN.
                  if (sign(1., brnmin(jj)) .le. 0.0) goto 190

C                 THEN NOT USER-SUPPLIED, ISW=0
                endif
              endif

C             GET THE USER-SUPPLIED VALUES.
              ymin = brnmin(jj)
              ymax = brnmax(jj)

C             UNITS PER INCH, ON 8-INCH AXIS
              isw = 1
              scdiv = (ymax-ymin)/8.
              valmin = ymin
              kxp = 1

C             NUMBER OF INCHES ABOVE ZERO
              py =  - valmin/scdiv
            endif
          enddo
 
C         NOGRP IS THE NUMBER OF DISTINCT
C         GROUPS OF BRANCHES TO BE PLOTTED SEPARATELY.
  190     continue
          do kgrp = 1, nogrp
            ktplot = 0
            lkt = linekt

C           KOUNT IS THE COUNTER OF PLOTS ON THIS AXIS
C           IGRP IS THE USER-SUPPLIED GROUP NUMBER
            kount = 0
            igrp = igrup(kgrp)
            kt = 0
            iecs = ladecs
C          
C           I IS THE COUNTER OF THE BRANCHES PLOTTED.
C           LINEKT IS THE NUMBER OF BRANCHES TO PLOT
C          
            i = 0
            do while (.true.)
              kt = kt + 50
              if (lkt .le. 50) kt = linekt

C             <>  NOTE IECS IS EQUATED TO LADECS.  THE PACKED <>
C             <>  ARRAY WRITTEN AT LADECS WAS LINDAT.  THE NEW <>
C             <>  ARRAYS ARE LNDATN AND LNDATC.              <>
              iecs = iecs + 9*kt
              lkt = lkt - kt
              do while (.true.)
                i = i + 1

C               DONE WITH BRANCHES IF I > KT.
                if (i .le. kt) then
                  if (lndatn(1, 1, i) .ne. 0) then

C                   <> WANT TO TEST OPTIONS IN LNDATN(2,3,I) AND  <>
C                   <> IN LNDATN(2,5,I).                          <>
                    isw1 = lndatn(idx1(ilinsw), idx2(ilinsw), i)
C                   ISW1 IS THE OUTPUT INDICATOR
C                   ISW1 = 0 - NO OUTPUT
C                   ISW1 = 1 - LIST
C                   ISW1 = 2 - PLOT
C                   ISW1 = 3 - LIST AND PLOT
                    if (plotop(isw1)) then

C                     ISW1 = 2 OR 3 HERE, MEANING TO DO A PLOT.
C                     GET THE FILE WHERE TO PUT THE
C                     INFORMATION FOR THIS KIND OF PLOT.
                      kount = kount + 1

C                     SKIP IF NOT THE CURRENT GROUP NUMBER.
                      if (lndatn(2, 8, i) .eq. igrp) then
                        ktplot = ktplot + 1

C                       GET THE BRANCH NAME.
                        idgenn(1, ktplot) = lndatn(1, 1, i)
                        idgenn(2, ktplot) = lndatn(1, 2, i)
                        idgenc(1, ktplot) = lndatc(i)

C                       ISUB,MSUB, AND THIS STATEMENT ARE NOT NEEDED.
                        call redecp(msub(26), isub(ilinsw)+kount-1, 1)

C                       PULL OUT THE NUMBERS THAT WE WANT TO PLOT.
C                       LOCATED ON FILE MIND, ICOUN2 VALUES, KEY=1.
                        call readmp (13, work(jadr+1), icoun2, 1)

C                       KWORK IS THE STARTING ECS ADDRESS OF THE
C                       DATA WE ARE PUTTING INTO STORAGE.  THERE IS
C                       NO CONCERN WITH UPPER LIMITS ON USAGE.  IF
C                       KECST EXCEEDS THE ACCOUNTING MAX, KECMAX, BUMP
C                       UP KECMAX.
C                       KECS IS BIASED UP BY THE COUNT OF DATA TIMES
C                       THE NUMBER OF PLOTS.
                        kecs = kwork + (ktplot-1)*icoun2

C                       PUT THE DATA WE GOT FROM MASS STORAGE INTO ECS.
                        call ritecp(work(jadr+1), kecs, icoun2)
                        kecst = kecs + icoun2
                        if (kecst .gt. kecmax) kecmax = kecst
                        if (isw .eq. 1) then
 
C                         PROCESSING FOR USER-SUPPLIED MAX AND MIN . . .
C                         IN THIS LOOP, SIFT THRU THE DATA TO SEE IF IT
C                         STEPS OUT OF BOUNDS, THEN TRUNCATE IT.
                          do jj = 1, icount
                            jindx = jadr + jj
                            if (work(jindx) .gt. ymax) work(jindx) =
     &                       ymax
                            if (work(jindx) .lt. ymin) work(jindx) =
     &                       ymin
                          enddo

C                         PUT THE DATA INTO ECS.
C                         BY THE WAY, THIS CODE IS REDUNDANT.
C                         THE LOGIC AFTER STMT 280 SHOULD BE RECODED.
                          call ritecp(work(jadr+1), kecs, icoun2)
                          kecst = kecs + icoun2
                          if (kecst .gt. kecmax) kecmax = kecst
                        endif
                      endif
                    endif
                  endif

C                 GO BACK AND AND PRE-PROCESS MORE PLOT INFORMATION
C                 IF WE HAVENT YET REACHED THE PLOT MAXIMUM.
C                 IF WE HAVE PROCESSED IPLTKT PLOTS, THEN GO ON TO
C                 DO PLOTTING.
                  if (ktplot .ne. ipltkt) goto 200

C                 IF PLOTTING NOT ASKED FOR ON THIS GROUP, GO
C                 ON TO NEXT GROUP.
                elseif (lkt .gt. 0) then
                  goto 210
                endif
                if (ktplot .eq. 0) goto 220

C               GET A COPY OF I IN CASE IT CHANGES
                isave = i
                if (isw .ne. 1) then

C                 THIS SECTION DOES AUTMATIC SCALING IF
C                 THE MAX AND MIN ARE NOT SUPPLIED IN DATA.
                  ymax = 0.0
                  ymin = 0.0
                  do i = 1, ktplot

C                   LOOP THROUGH THE PLOT DATA TO GET THE MAXIMA
C                   AND MINIMA FOR ALL PLOTS, TO FIND THE MAXIMUM
C                   MAXIMUM AND THE MINIMUM MINIMUM.  THIS IS SO
C                   THAT THEY ALL CAN FIT ON THE SAME GRAPH.
                    kecs = kwork + i*icoun2 - 2

C                   GET THE MAX AND MIN VALUES FOR EACH PLOT
C                   BACK OUT OF ECS. WAS STORED ABOVE.
                    call redecp(ylmt, kecs, 2)

C                   ADJUST OUR MAXIMUM AND MINIMUM SO THAT THEY
C                   COVER THE RANGE OF MAXIMA AND MINIMA FOR ALL PLOTS.
                    if (ylmt(1) .gt. ymax) ymax = ylmt(1)
                    if (ylmt(2) .lt. ymin) ymin = ylmt(2)
                  enddo
                  smax = ymax
                  smin = ymin
                  trnge = smax - smin
                  if (abs(trnge) .lt. .001) goto 220
                  call scal()
                endif
                dy = scdiv
                id1 = 2*ind - 1
                yaxe(1)( 1:10) = ylin(2*ilinsw-1)
                yaxe(1)(11:20) = ylin(2*ilinsw)
                call plt()
                if (isave .gt. kt) goto 220
                ktplot = 0
                jadr = 701
                i = isave
  200           continue
              enddo
  210         continue
            enddo
  220       continue
          enddo
          if (ilinsw .ge. 3) goto 230
        enddo
      endif
C     
C     PLOT DC BUS VARIABLES
C     
C     IF NO DC OUTPUT REQUESTS SKIP THE ENTIRE LOGIC
  230 if (iddat .eq. 0) goto 400

C     IF NO TERMNL OUTPUT REQUEST THEN CHECK FOR BRANCH OUTPUT
      if (idcbs .eq. 0) goto 390

C     BEGIN PROCESSING TERMINAL OUTPUT PLOTS
C     IBSBR IS A TERMINAL/BRANCH FLAG.  WHEN PROCESSING TERMINAL
C     PLOTS IBSBR =0.  WHEN PROCESSING A BRANCH IBSBR = 1.
      ibsbr = 0

C     THE LOGIC FROM STMT NO. 982 TO 1040 IS USED FOR BOTH TERMNL
C     AND BRANCH OUTPUTS
  240 jadr = 18*icoun2 + 1
      ipltsw = 1
C    
C     IOPTON: 1 = ANGLE, 
C             2 = CURRENT, 
C             3 = VOLTS, 
C             4 = MODULATION
C             5 = EXTINCTION ANGLE, 
C             6 = V ALPHA, 
C             7 = V ALPHA PRIME
C             8 = OVERLAP ANGLE, 
C             9 = POWER
C    
      iopton = 0
      do while (.true.)
        iopton = iopton + 1
        imov1 = 2*iopton + 1
        imov2 = imov1 + 1
        ihdsw = 0
        isw = 0
C      
C       CHECK TO SEE IF EXTERNAL MAX AND MIN ENTERED FROM DH CARD
C      
        do jj = 1, 4
          if (idcl(jj) .eq. iopton) goto 250
        enddo
        goto 260
  250   if (dcmax(jj) .ne. 0.0 .or. dcmin(jj) .ne. 0.0) then
          if (dcmax(jj) .eq. 0.0) then
            if (sign(1., dcmax(jj)) .le. 0.0) then
              if (sign(1., dcmin(jj)) .le. 0.0) goto 260
            endif
          endif
          ymin = dcmin(jj)
          ymax = dcmax(jj)
          isw = 1
          scdiv = (ymax-ymin)/8.
          valmin = ymin
          py =  - valmin/scdiv
          kxp = 1
        endif
  260   if (ibsbr .ne. 1) then

C         GROUPING OF PLOTS FOR DC TERMINAL OUTPUTS
          do k = 1, iddat
            igrpdc(k) = idcgrp(k)
          enddo
          goto 320
        elseif (idcbr .eq. 0) then
          goto 400
        else

C         PROCESS BRANCH OUTPUT PLOTS
C         GROUPING OF PLOTS FOR DC BRANCH OUTPUTS
          do k = 1, iddat
            igrpdc(k) = idcgrp(k)
          enddo
        endif
        do kgrp = 1, iddat
          if (idclin(kgrp) .eq. 0) goto 310
          if (idcxn(1, kgrp) .eq. 0 .and. idcxn(2, kgrp) .eq. 0 .and.
     &     idcxn(3, kgrp) .eq. 0) goto 310
          ktplot = 0
          igrp = igrpdc(kgrp)
          i = 0
          do while (.true.)
            i = i + 1
            if (i .gt. iddat) goto 280
            if (idclin(i) .eq. 0) goto 300
            if (idcxn(1, i) .eq. 0 .and. idcxn(2, i) .eq. 0 .and. idcxn
     &       (3, i) .eq. 0) goto 300
            if (igrpdc(i) .lt. 0) goto 300
            lgrp = igrpdc(i)
            if (lgrp .ne. igrp) goto 300
            igrpdc(i) =  - 1
            isw1 = idindn(imov1+2, i)
            isw2 = idindn(imov2+2, i)
            k1 = idcxn(1, i)
            k2 = idcxn(2, i)
            ibrn = idcxn(3, i)
            if (ibrn .eq. 0) goto 300

C           <> KDCBRN = KECMAX-NDC3.  THE PACKED ARRAY  <>
C           <> THAT WAS WRITTEN AT THIS LOCATION IS NOW <>
C           <> CALLED DCNME2 AND IDCBK2.  SEE OUTPUT2. <>
            ib1 = idcbk2(2*ibrn-1)
            ib1c = dcnme2(2*ibrn-1)

C           FOR DC BRANCHES, CORROSPONDING DC BUS NUMBERS ARE RETRIE
C           FROM STORED BRANCH NUMBER
            ib2 = idcbk2(2*ibrn)
            ib2c = dcnme2(2*ibrn)

C           IDIR DENOTES BRANCH SENSE IN THE OUTPUT REQUEST.  IDIR =
C           IT IS SAME AS IN THE BRANCH TABLE.  IDIR=2 IF IT IS OPPO
            idir = 1
            if (k1 .ge. k2) then
              ibdum = ib1
              ibdumc = ib1c
              ib1 = ib2
              ib1c = ib2c
              ib2 = ibdum
              ib2c = ibdumc
              idir = 2
            endif
            call redecp(msub1(2), kdcplt+i, 1)
            call readmp (1, work, 5*icoun2, 1)
            if (.not. plotop(isw1)) goto 290
            ktplot = ktplot + 1
            if (iopton .eq. 2) then

C             CURRENT OPTION
              ipltsw = 4

C             IDGEN(KTPLOT) IS A PACKED WORD - (BRANCH NO.,IDIR,3RD
C             THE 3RD ARGUMENT IS 0 FOR CURRENT OPTION
              idgenn(1, ktplot) = ibrn
              idgenn(2, ktplot) = idir
              idgenn(3, ktplot) = 0
              iadr = 1
              jindx = (icoun2-2) + iadr
            else
              if (iopton .eq. 3) then

C               VOLTAGE OPTION
                ipltsw = 1

C               IDGEN(KTPLOT) IS A PACKED WORD (DC BUS NAME - BASE C
                idgenn(1, ktplot) = ib1
                idgenc(1, ktplot) = ib1c
              else
C               POWER OPTION
                ipltsw = 5

C               IDGEN(KTPLOT) IS A PACKED WORD SIMILER TO CURRENT OP
C               3RD ARGUMENT IS 1 FOR POWER INTO THE BRANCH AT LH BU
C               FOR POWER OUT OF THE BRANCH AT RH BUS
                idgenn(1, ktplot) = ibrn
                idgenn(2, ktplot) = idir
                idgenn(3, ktplot) = 1
              endif
              iadr = (2*iopton-5)*icoun2 + 1
              jindx = iadr + (icoun2-2)
            endif
            if (isw .eq. 1) then
              if (work(jindx) .gt. ymax) work(jindx) = ymax
              if (work(jindx+1) .lt. ymin) work(jindx) = ymin
            endif
            kecs = kwork + (ktplot-1)*icoun2
            call ritecp(work(iadr), kecs, icoun2)
            kecst = kecs + icoun2
            if (kecst .gt. kecmax) kecmax = kecst
            if (ktplot .ne. ipltkt) goto 290

C           THE FOLLOWING LOGIC (UPTO STMT NO. 3280) PROCESSES GROUP
C           PLOTS BY CALLING SCAL AND PLT
            irturn = 1
  270       if (ktplot .eq. 0) goto 310
            isave = i
            if (isw .ne. 1) then
              ymax = 0.0
              ymin = 0.0
              do i = 1, ktplot
                kecs = kwork + (i*icoun2) - 2
                if (ylmt(1) .gt. ymax) ymax = ylmt(1)
                if (ylmt(2) .lt. ymin) ymin = ylmt(2)
              enddo
              smax = ymax
              smin = ymin
              trnge = smax - smin
              if (abs(trnge) .lt. .001) goto 310
              call scal()
            endif
            dy = scdiv
            yaxe(1)( 1:10) = ydc(2*iopton-1)
            yaxe(1)(11:20) = ydc(2*iopton)
***************** debug stuff *************************
      print 9901, yaxe(1)
 9901 format('yaxe = ',a)
            call plt()
            ktplot = 0
            i = isave
            if (irturn .eq. 1) goto 290
            if (irturn .eq. 2) goto 300
            if (irturn .eq. 3) goto 310
  280       irturn = 3
            goto 270

C           ONLY ONE CURRENT OPTION IS PROCESSED ON THE BRANCH OUTPU
  290       if (iopton .ne. 2) then
              if (plotop(isw2)) then
                ktplot = ktplot + 1
                if (iopton .eq. 3) then

C                 VOLTAGE OPTION
                  ipltsw = 1
                  idgenn(1, ktplot) = ib2
                  idgenc(1, ktplot) = ib2c
                else

C                 POWER OPTION
                  ipltsw = 5
                  idgenn(1, ktplot) = ibrn
                  idgenn(2, ktplot) = idir
                  idgenn(3, ktplot) = 2
                endif
                iadr = (2*iopton-4)*icoun2 + 1
                if (isw .eq. 1) then
                  jindx = iadr + (icoun2-2)
                  if (work(jindx) .gt. ymax) work(jindx) = ymax
                  if (work(jindx+1) .lt. ymin) work(jindx) = ymin
                endif
                kecs = kwork + (ktplot-1)*icoun2
                call ritecp(work(iadr), kecs, icoun2)
                kecst = kecs + icoun2
                if (kecst .gt. kecmax) kecmax = kecst
                if (ktplot .eq. ipltkt) then
                  irturn = 2
                  goto 270
                endif
              endif
            endif
  300       continue
          enddo
  310     continue
        enddo
        if (iopton .ne. 4) goto 380
        goto 400
  320   do kgrp = 1, iddat
          if (idclin(kgrp) .ne. 0) goto 370
          if (idcxn(1, kgrp) .eq. 0 .and. idcxn(2, kgrp) .eq. 0 .and.
     &     idcxn(3, kgrp) .eq. 0) goto 370
          ktplot = 0
          igrp = igrpdc(kgrp)
          i = 0
          do while (.true.)
            i = i + 1
            if (i .gt. iddat) goto 340

C           SKIP IF A BRANCH OUTPUT REQUEST ENCOUNTERED
            if (idclin(i) .ne. 0) goto 360
            if (idcxn(1, kgrp) .eq. 0 .and. idcxn(2, kgrp) .eq. 0 .and.
     &       idcxn(3, kgrp) .eq. 0) goto 360
            if (igrpdc(i) .lt. 0) goto 360
            lgrp = igrpdc(i)
            if (lgrp .ne. igrp) goto 360
            igrpdc(i) =  - 1
C           
C           IB1 OR IB2 COULD BE ZERO FOR A NON-VALID BUSNAME OR -1 FOR A
C           BLANK BUS NAME ON THE OUTPUT REQUEST CARD
C           TABLE SRTRUCTURE IS DIFFERENT FOR DIFFERENT .SOL FILES
C           IF PLOTT = VRSND001, THEN THIS IS THE NEW VERSION WITH DC MOD
C           
            if (plott .eq. 'VRSND001') then
              ib1 = idindn(1, i)
              ib2 = idindn(2, i)
              isw1 = idindn(imov1, i)
              isw2 = idindn(imov2, i)
            else
              ib1 = idindn(1, i)
              ib2 = idindn(2, i)
              isw1 = idindn(imov1+2, i)
              isw2 = idindn(imov2+2, i)
            endif
            call redecp(msub1(2), kdcplt+i, 1)
            call readmp (1, work, 18*icoun2, 1)
            icou28 = 18*icoun2
            if (.not. plotop(isw1)) goto 350

C           SKIP IF LH BUS IS NOT A VALID DC BUS
            if (ib1 .le. 0) goto 350
            ktplot = ktplot + 1

C           IDGEN(KTPLOT) CONTAINS PACKED BUS NAME-BASE CODE.
            idgenc(1, ktplot) = newtbc(ib1)
            idgenn(1, ktplot) = inwtb(ib1)
            iadr = 2*(iopton-1)*icoun2 + 1
            kecs = kwork + (ktplot-1)*icoun2
C          
C           IF ISW = 1 EXTERNAL LIMITS WERE ENTERED
C          
            if (isw .eq. 1) then
              jindx = iadr + icoun2 - 2
              if (work(jindx) .gt. ymax) work(jindx) = ymax
              if (work(jindx+1) .lt. ymin) work(jindx+1) = ymin
            endif
            call ritecp(work(iadr), kecs, icoun2)
            kecst = kecs + icoun2
            if (kecst .gt. kecmax) kecmax = kecst
            modcde = idcmd1(i)

C           IF PLOT COUNT EQUALS LIMIT SPECIFIED BY MH CARD, PLOT THE
C           ACCUMULATED DATA AND RETURN BACK HERE
            if (ktplot .ne. ipltkt) goto 350
            irturn = 1

C           THE FOLLOWING LOGIC (UPTO STMT NO. 1280) PROCESSES THE G
C           PLOTS USING SCALING (CALL SCAL) AND CREATES THE PLOT FIL
C           PLT). IF MORE GROUPS OF PLOTS HAVE TO BE PROCESSED THEN
C           BACK AND PROCESS FURTHER
  330       if (ktplot .eq. 0) goto 370
            isave = i
            if (isw .ne. 1) then
              ymax = 0.0
              ymin = 0.0
              do i = 1, ktplot
                kecs = kwork + (i*icoun2) - 2
                call redecp(ylmt, kecs, 2)
                if (ylmt(1) .gt. ymax) ymax = ylmt(1)
                if (ylmt(2) .lt. ymin) ymin = ylmt(2)
              enddo
              smax = ymax
              smin = ymin
              trnge = smax - smin
              if (abs(trnge) .lt. .001) goto 370
              call scal()
            endif
            dy = scdiv
          
C           GET PROPER X AXIS LABELS
          
            if (iopton .eq. 4) then
              if (modcde .le. 0) modcde = 7
              yaxe(1)( 1:10) = modlbl(2*modcde-1)
              yaxe(1)(11:20) = modlbl(2*modcde)
            else
              yaxe(1)( 1:10) = ydc(2*iopton-1)
              yaxe(1)(11:20) = ydc(2*iopton)
            endif
            call plt()
            ktplot = 0
            i = isave
            if (irturn .eq. 1) goto 350
            if (irturn .eq. 2) goto 360
            if (irturn .eq. 3) goto 370
  340       irturn = 3
            goto 330
  350       if (plotop(isw2)) then

C             SKIP IF RH BUS IS NOT A VALID DC BUS
              if (ib2 .gt. 0) then
                ktplot = ktplot + 1
                idgenc(1, ktplot) = newtbc(ib2)
                idgenn(1, ktplot) = inwtb(ib2)
                iadr = (2*iopton-1)*icoun2 + 1
                kecs = kwork + (ktplot-1)*icoun2
                modcde = idcmd2(i)
                if (isw .eq. 1) then
                  jindx = iadr + icoun2 - 2
                  if (work(jindx) .gt. ymax) work(jindx) = ymax
                  if (work(jindx+1) .lt. ymin) work(jindx+1) = ymin
                endif
                call ritecp(work(iadr), kecs, icoun2)
                kecst = kecs + icoun2
                if (kecst .gt. kecmax) kecmax = kecst
                if (ktplot .eq. ipltkt) then
                  irturn = 2
                  goto 330
                endif
              endif
            endif
  360       continue
          enddo
  370     continue
        enddo
        if (iopton .eq. 9) goto 390
  380   continue
      enddo
  390 if (idcbr .ne. 0) then
        ibsbr = 1
        goto 240
      endif
  400 ktl = ktzpp + ktnol5 + ktnol6
      if (ktl .gt. 0) then
 
C       PLOT APPARANT IMPEDANCE
 
        ilinsw = 0
        isub(1) = ksb13d
        idx1(1) = 2
        idx2(1) = 7
        isub(2) = ksb13f
        idx1(2) = 3
        idx2(2) = 5
        isub(3) = ksb13g
        idx1(3) = 3
        idx2(3) = 7
        nsca = 6
        sca(1) = 2.
        sca(2) = 2.5
        sca(3) = 5.0
        sca(4) = 10.
        sca(5) = 20.
        sca(6) = 40.
        if (ntim .eq. 0) ntim = 4
        icnt2 = 2*icount
        jadr = 701
        do while (.true.)
          ilinsw = ilinsw + 1
 
C         LOOP THROUGH LOGIC THREE TIMES. ILINSW=1 FOR APPARENT Z
C         ILINSW=2 FOR Z-Z DOT. ILINSW=3 FOR R-R DOT
 
          jstart = jadr + icnt2 + 1
          kt = 50
          iecs = ladecs
          kount = 0
          linekt = ildatt
          do while (.true.)
            if (linekt .le. 50) kt = linekt
            iecs = iecs + 9
            linekt = linekt - kt
            do i = 1, kt
              if (.not. (lndatn(1, 1, i) .eq. 0 .and. lndatn(1, 2, i)
     &         .eq. 0 .and. lndatc(i) .eq. ' ')) then
                izdotz = lndatn(2, 1, i)
                isw1 = lndatn(idx1(ilinsw), idx2(ilinsw), i)
                izdotz = izdotz/10
                iclas = mod(izdotz, 10)
                izdotz = izdotz - iclas
                zscale = (float(izdotz))/1000.
                izdotz = iclas
                tlenth = 8.
                if (izdotz .eq. 1) tlenth = zscale
                if (plotop(isw1)) then
                  actr = 0.0
                  actx = 0.0
                  if (bndatn(7, 1, i) .ne. 0.0) then
C                  
C                   COMPUTE ACTUAL LINE IMPEDANCE IN OHMS
C                  
                    ibus = lndatn(1, 1, i)
                    ibkv = inwtb(ibus)
                    bkv = basekv(ibkv)
                    zbase = bkv*bkv/bmva
                    actr = bndatn(3, 1, i)*zbase/bndatn(7, 1, i)
                    actx =  - bndatn(4, 1, i)*zbase/bndatn(7, 1, i)
                  endif
                  kount = kount + 1
                  call redecp(msub(26), isub(ilinsw)+kount-1, 1)
                  call readmp (13, work(jadr+1), icnt2, 1)
                  iswx = 0
                  iswr = 0
                  rmax = bndatn(8, 1, i)
                  rmin = bndatn(8, 2, i)
                  xmax = bndatn(9, 1, i)
                  xmin = bndatn(9, 2, i)
                  if (ilinsw .gt. 1) then
                    rmax = bndatn(8, 3, i)
                    rmin = bndatn(8, 4, i)
                    xmax = bndatn(9, 3, i)
                    xmin = bndatn(9, 4, i)
                  endif
                  if (xmax .ne. xmin) iswx = 1
                  if (rmax .ne. rmin) iswr = 1
                  if (izdotz .eq. 1) then
                    if (iswx .eq. 1) then
                      ymax = xmax
                      ymin = xmin
                    else
                      ymin = 1e32
                      ymax =  - ymin
                    endif
                    if (iswr .eq. 1) then
                      xmax = rmax
                      xmin = rmin
                    else
                      xmin = 1e32
                      xmax =  - xmin
                    endif
                    do jj = 1, icount
                      j2 = 2*jj + jadr
                      j1 = j2 - 1
                      if (iswr .eq. 0) then
                        if (work(j1) .gt. xmax) xmax = work(j1)
                        if (work(j1) .lt. xmin) xmin = work(j1)
                      endif
                      if (iswx .eq. 0) then
                        if (work(j2) .gt. ymax) ymax = work(j2)
                        if (work(j2) .lt. ymin) ymin = work(j2)
                      endif
                      work(jstart+jj) = work(j1)
                      work(jadr+jj) = work(j2)
                    enddo
                  else
                    if (iswx .eq. 1) then
                      ymax = xmax
                      ymin = xmin
                    endif
                    if (iswr .eq. 1) then
                      xmax = rmax
                      xmin = rmin
                    endif
                    if (iswx .eq. 0) then
                      ymax = 0.0
                      ymin = 0.0
                    endif
                    if (iswr .eq. 0) then
                      xmax = 0.0
                      xmin = 0.0
                    endif
                    do jj = 1, icount
                      j2 = 2*jj + jadr
                      j1 = j2 - 1
                      if (.not. ((iswx .ne. 0) .and. (iswr .ne. 0)))
     &                 then
                        if (work(j1) .gt. xmax) xmax = work(j1)
                        if (work(j1) .lt. xmin) xmin = work(j1)
                        if (work(j2) .gt. ymax) ymax = work(j2)
                        if (work(j2) .lt. ymin) ymin = work(j2)
                      endif
                      work(jstart+jj) = work(j1)
                      work(jadr+jj) = work(j2)
                    enddo
                  endif
                  work(jstart+icount+1) = xmax
                  work(jstart+icount+2) = xmin
                  work(jadr+icount+1) = ymax
                  work(jadr+icount+2) = ymin
                  rupi = (xmax-xmin)/tlenth
                  xupi = (ymax-ymin)/8.0
                  if (izdotz .eq. 1) then
                    anexr = alog10(rupi)
                    anexx = alog10(xupi)
                    if (anexr .lt. 0.0) anexr = anexr - 1.
                    if (anexx .lt. 0.0) anexx = anexx - 1.
                    nexr = anexr
                    nexx = anexx
                    sigr = rupi/10.**nexr
                    sigx = xupi/10.**nexx
                    do is = 1, nsca
                      nscalr = is
                      if (sigr .lt. sca(is)) goto 410
                    enddo
  410               do is = 1, nsca
                      nscalx = is
                      if (sigx .lt. sca(is)) goto 420
                    enddo
  420               do while (.true.)
                      scallr = sca(nscalr)*10.**nexr
                      scallx = sca(nscalx)*10.**nexx
                      mrmn = xmin/scallr
                      if (mrmn .lt. 0) mrmn = mrmn - 1
                      mxmn = ymin/scallx
                      if (mxmn .lt. 0) mxmn = mxmn - 1
                      amrmn = mrmn
                      amxmn = mxmn
                      armn = amrmn*scallr
                      axmn = amxmn*scallx
                      rmxx = armn + tlenth*scallr
                      xmxx = axmn + 8.*scallx
                      if (rmxx .lt. xmax) then
                        nscalr = nscalr + 1
                      elseif (xmxx .ge. ymax) then
                        goto 430
                      else
                        nscalx = nscalx + 1
                      endif
                    enddo
  430               dx = scallr
                    scdiv = scallx
                  else
                    zupi = amax1(rupi, xupi)
                    if (zupi .eq. 0.0) zupi = 0.1
                    anex = alog10(zupi)
                    if (anex .lt. 0.0) anex = anex - 1.
                    nex = anex
                    sig = zupi/10.**nex
                    do is = 1, nsca
                      nscal = is
                      if (sig .lt. sca(is)) goto 440
                    enddo
  440               do while (.true.)
                      scall = sca(nscal)*10.**nex
                      mrmn = xmin/scall
                      if (mrmn .le. 0) mrmn = mrmn - 1
                      mxmn = ymin/scall
                      if (mxmn .le. 0) mxmn = mxmn - 1
                      amrmn = mrmn
                      amxmn = mxmn
                      armn = amrmn*scall
                      axmn = amxmn*scall
                      rmxx = armn + tlenth*scall
                      xmxx = axmn + 8.*scall
                      if ((rmxx .ge. xmax) .and. (xmxx .ge. ymax)) goto
     &                 450
                      nscal = nscal + 1
                    enddo
  450               dx = scall
                    scdiv = scall
                  endif
                  xmin = armn
                  valmin = axmn
                  if (iswx .eq. 1 .and. iswr .eq. 1) then
                    dx = rupi
                    scdiv = xupi
                    xmin = rmin
                    valmin = ymin
                    rmxx = rmax
                    armn = rmin
                    xmxx = ymax
                    axmn = ymin
                  endif
                  kxpx = 2
                  kxp = 2
                  px =  - xmin/scdiv
                  py =  - axmn/scdiv
                  if (xmin .gt. 0.0) px = 0.0
                  if (axmn .gt. 0.0) py = 0.0
                  ipltsw = 3
                  yaxe(1)( 1:10) = yzplt(2*ilinsw-1)
                  yaxe(1)(11:20) = yzplt(2*ilinsw)
                  ktplot = 1
                  idgenn(1, 1) = lndatn(1, 1, i)
                  idgenn(2, 1) = lndatn(1, 2, i)
                  idgenc(1, 1) = lndatc(i)
 
                  do is = 1, icount
                    j1 = jstart + is
                    j2 = jadr + is
                    if (work(j1) .gt. rmxx) work(j1) = rmxx
                    if (work(j1) .lt. armn) work(j1) = armn
                    if (work(j2) .gt. xmxx) work(j2) = xmxx
                    if (work(j2) .lt. axmn) work(j2) = axmn
                  enddo
 
                  ihdsw = 0
                  call plt()
                endif
              endif
            enddo
            if (ilinsw .lt. 3) goto 460
            if (linekt .le. 0) goto 470
          enddo
  460     continue
        enddo
      endif
* 470 call symbol(0.5, 0.5, .125, end, 0.0, 3)
  470 call plot(0.0, 0.0, 998)
      return
      end

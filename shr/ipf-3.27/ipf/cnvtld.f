C    @(#)cnvtld.f	20.7 7/18/96
      subroutine cnvtld (error)
 
C     process /%LOAD_DISTRIBUTION  commands.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		ntypu
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
c	Global variables used:
c		jarzn
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8), busdta, kbsdta, inp2opt, opt2inp,
c		zone, bus, owner, base
      include 'ipfinc/cbsorc.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		None
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qsdup.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/tbx.inc'
c	Global variables used:
c		tbx(r*8), 
      include 'ipfinc/tran.inc'
c	Global variables used:
c		None
      include 'ipfinc/datainit.inc'
c
      common /scratch/ ntemp, ktemp(12,100), temp_index(100)
      real temp(12,100)
      integer temp_index
      equivalence (temp, ktemp)

      common /amtrx/ buspct(2,3,MAXBUS), ownlis(100), buscod(2*MAXBUS),
     1               nxtbus(2*MAXBUS), businx(2*MAXBUS),
     2               lstbus(2*MAXBUS)   
      character ownlis * 3, buscod * 9
      integer businx

      common /is_batch / is_batch

      character chgtyp * 1, chg(3) * 1, buschg * 1  
      integer compld
 
c     if CHGTYP = "P":
C
c        buspct (1,1,*) = % pload --> pload
c        buspct (2,1,*) = % qload --> qload
c        buspct (1,2,*) = % pload --> i
c        buspct (2,2,*) = % qload --> i
c        buspct (1,3,*) = % pload --> g(shunt)
c        buspct (2,3,*) = % qload --> b(shunt)
C
C     else if CHGTYP = "A":
C
c        buspct (1,1,*) = % aload --> pload
c        buspct (2,1,*) = % bload --> qload
c        buspct (1,2,*) = % aload --> i
c        buspct (2,2,*) = % bload --> i
c        buspct (1,3,*) = % aload --> g(shunt)
c        buspct (2,3,*) = % bload --> b(shunt)
C
C     else if CHGTYP = "R":
C
c        buspct (1,1,*) = % rload --> pload
c        buspct (2,1,*) = % xload --> qload
c        buspct (1,2,*) = % rload --> i
c        buspct (2,2,*) = % xload --> i
c        buspct (1,3,*) = % rload --> g(shunt)
c        buspct (2,3,*) = % xload --> b(shunt)
C
C     BUSCOD     BUS     +BUS
C     ------    -----   -----
C      (1:1)     "B"     "+"
C      (2:2)     " "    type
C      (3:5)    owner   owner   
C      (6:7)     " "    code yr 
C      (8:8)     "#"     "#"     specified with >CHANGE_SYSTEM  
C                " "     " "     specified with >CHANGE_BUSES   
C      (9:9)     "P"     "P"     P_load + jQ_load distributed.  
C                "A"     "A"     A_load + jB_load distributed.  
C                "R"     "R"     R_load + jX_load distributed.  
C       
      character bus1 * 8, word(100) * 30, lodtyp(3) * 1,
     1          bigbuf * 512, type * 4, cbown * 3, cbtyp * 1,   
     2          cbkyr * 2, ljstfy * 512, strng1 * 6, strng2 * 6,
     3          owntmp * 3, cbtypx * 1, cbkyrx * 2, cbownx * 3, 
     4          cbtypt * 1, cbkyrt * 2, cbownt * 3, tag * 14
      logical found, psw(3), qsw(3), prntbs, busflg, cbsflg, endrcd,
     1        match 
      integer find_bus, error, freesp, addcbs, first, firpc1, oldcvt, 
     1        findex, busown
      real pcnvt(3,3), qcnvt(3,3), bustot(2,6), systot(2,6)
     
      external komcbs, swpcbs, komcbx, swpcbx, find_bus
 
 
C     / %LOAD_DISTRIBUTION, DISTRIBUTED_VOLTAGES = BASE                *
C                                                  NOMINAL             *
C     > CHANGE_BUSES                                                   *
C     .                                                                *
C     .                       Constant P   Constant I   Constant Z     *
C     .                       (MW) (MVAR)  (MW) (MVAR)  (MW)(MVAR)     *
C     .                          %     %      %     %      %     %  
C     B      bus_name base yr  ###   ###    ###   ###    ###   ###     *
C     B      bus_name base yr  ###   ###    ###   ###    ###   ###     *
C     +T own bus_name base yr  ###   ###    ###   ###    ###   ###     *
C     +T own bus_name base yr  ###   ###    ###   ###    ###   ###     *
C                                                                      *
C     > CHANGE_SYSTEM, PLOAD = ##% P + ##% I + ##% Z, -                *
C                      QLOAD = ##% Q + ##% I + ##% Z, -                *
C                                                                      *
C                      AREAS = area_1, ... -                           *
C                      ZONES = zone_1, ...                             *
C                                                                      *
C     > EXCLUDE_BUSSES <                                               *
C     B      bus_name base                                             *
C     B      bus_name base                                             *
C                                                                      *
C     / CHANGE_SYSTEM, PLOAD = ##% P + ##% I + ##% Z, -                *
C                      QLOAD = ##% Q + ##% I + ##% Z, -                *
C                      AREAS = area_1, ... -                           *
C                      ZONES = zone_1, ...                             *
C                                                                      *
C     > EXCLUDE_BUSSES <                                               *
C     B      bus_name base                                             *
C     B      bus_name base                                             *
C                                                                      *
C       
C     Initialize variables. 
C       
      error = 0
      do 90 i = 1, 2 * MAXBUS   
         nxtbus(i) = 0  
         lstbus(i) = 0  
         businx(i) = 0  
         buscod(i) = ' '
   90 continue  
      do 94 i = 1, MAXBUS   
         do 92 j = 1, 3 
            buspct(1,j,i) = 0.0 
            buspct(2,j,i) = 0.0 
   92    continue   
   94 continue  
        
      call space (1)
      write (outbuf,100 ) buf(1:80) 
  100 format (' %LOAD_DISTRIBUTION text: (', a, ')')
      call prtout (1)   
C                                                                      *
C     Initialize IKK array:                                            *
C                                                                      *
C     (1,*) = 0 :  normal.                                             *
C             N :  % entity in BUSPCT.                                 *
C     (2,*) = 0 :  bus is not eligible for systematic % changes.       *
C             1 :  bus is eligible for systematic % changes.           *
C     (3,*) = I    (cross index to TBX array)                          *
C     (4,*) = 0    (not used)                                          *
C     (5,*) = 0    (not used)                                          *
C                                                                      *
      do 110 nb = 1, ntot   
         ikk(1,nb) = 0  
         ikk(2,nb) = 0  
         ikk(3,nb) = 0  
         ikk(4,nb) = 0  
         ikk(5,nb) = 0  
  110 continue  
        
      do 112 i = 1, ntotb   
         ltyp = tbx(1,i)   
         if (ltyp .lt. 10) then 
            nb = tbx(2,i)  
            if (ordtbx .eq. 2) nb = opt2inp(nb)   
            ikk(3,nb) = i   
         endif  
  112 continue  
        
      num1 = 0  
      num2 = 0  
      numpc1 = 0
      numpc2 = 0
 
C     Check for obsolete format of %LOAD_DISTRIBUTION command.
C
      bigbuf = buf
      endrcd = .false.
      last = lastch (bigbuf)
      do while (bigbuf(last:last) .eq. '-') 
         read (inp, 150, end=114) buf   
  150    format (a) 
         call space (1) 
         write (outbuf,100) buf(1:80)   
         call prtout (1)
         if (buf(1:1) .eq. ' ') bigbuf(last:) = ljstfy (buf)
         last = lastch (bigbuf)
         buf = bigbuf   
      enddo
      go to 116
  114 endrcd = .true.
  116 continue
        
      call uscan(bigbuf,word,nwrd,'=+', ' ,/\\<>()') 
        
      if (nwrd .gt. 3) then 
         if (findex (word(2), 'DIST') .ne. 0) then  
            if (findex (word(4), 'BASE') .ne. 0) then   
               defvlt = 'BASE'  
            else if (findex (word(4), 'NOM') .ne. 0) then   
            else
               last = lastch (word(4))
               write (errbuf(1), 117) word(4)(1:last)
  117          format ('Unrecognized %LOAD_DISTRIBUTION keyword',
     &                 ' ( ',a, ')')
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
               error = 1
               return   
            endif   
         else   
            last = lastch (word(2)) 
            write (errbuf(1), 117) word(2)(1:last)  
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
            error = 1   
            return  
         endif  
      endif 
C                                                                      *
C     Process %LOAD_DISTRIBUTION records here.                         *
C                                                                      *
  122 if (endrcd) then  
         buf = '( END ) CNVTLD' 
         card = buf(1:1)
      else  
         read (inp, 150, end=124) buf   
         card = buf(1:1)
         call space (1) 
         write (outbuf,100) buf(1:80)   
         call prtout (1)
         if (card .eq. '.') go to 122   
         go to 125  
  124    endrcd = .true.
         buf = '( END ) CNVTLD' 
         card = buf(1:1)
  125    continue   
      endif 
C                                                                      *
C     Check for and concatenate continuation records.                  *
C                                                                      *
  126 bigbuf = buf  
      last = lastch (bigbuf)
      do while (bigbuf(last:last) .eq. '-') 
         read (inp, 150, end=127) buf   
         call space (1) 
         write (outbuf,100) buf(1:80)   
         call prtout (1)
         if (buf(1:1) .eq. ' ') bigbuf(last:) = ljstfy (buf)
         last = lastch (bigbuf)
         buf = bigbuf   
      enddo
      go to 128
  127 endrcd = .true.
  128 continue
C       
C     Process > CHANGE_SYSTEM, ... commands here.   
C       
      if (card .eq. '>' .and. index (bigbuf,'CHANGE_SYS') .ne. 0)   
     1   then   
      else  
         go to 420  
      endif 
        
      call uscan(bigbuf,word,nwrd,'=+',' ,/\\<>()') 
        
      psw(1) = .false.  
      qsw(1) = .false.  

      do 152 i = 1, ntot
         ikk(2,i) = 0
  152 continue
        
      do 160 i = 1, 3   
      do 160 j = 1, 3   
         pcnvt(j,i) = 0.0   
         qcnvt(j,i) = 0.0   
  160 continue  
C                                                                      *
C     Search for OWNERS = <owner_1,...>                                *
C                                                                      *
      kntown = 0
        
      do 164 i = 2, nwrd
         if (word(i)(1:4) .eq. 'OWNE') then 
C                                                                      *
C           Check for "=" separator.                                   *
C                                                                      *
            if (word(i+1) .ne. '=') then
               last = lastch (word(i))  
               write (errbuf(1),170) word(i)(1:last)
               call prterx ('W', 1) 
               next = i + 1 
            else
               next = i + 2 
            endif   
            first = i   
            last = i
            do 162 j = next, nwrd   
               if (word(j)(1:4) .eq. 'AREA') then   
                  go to 166 
               else if (word(j)(1:4) .eq. 'ZONE') then  
                  go to 166 
               else 
                  kntown = kntown + 1   
                  ownlis(kntown) = word(j)  
                  last = j  
               endif
  162       continue
            go to 164   
         endif  
  164 continue  
C       
C     Eliminate "OWNERS = <owner_1>,..." from WORD. 
C       
  166 if (kntown .gt. 0) then   
         do 168 i = last+1, nwrd
            word(i-last+first-1) = word(i)  
  168    continue   
         nwrd = nwrd - last + first - 1 
      endif 
C                                                                      *
C     Search for AREAS = <area_1,...>                                  *
C                                                                      *
      jwrd = nwrd   
      do 270 i = 2, jwrd
         if (word(i)(1:4) .eq. 'AREA') then 
            nwrd = i - 1
C                                                                      *
C           Check for "=" separator.                                   *
C                                                                      *
            if (word(i+1) .ne. '=') then
               last = lastch (word(i))  
               write (errbuf(1),170) word(i)(1:last)
  170          format ('Keyword (', a,  
     1') in > change_system text is not followed with an "=" sign.')
               call prterx ('W', 1) 
               next = i + 1 
            else
               next = i + 2 
            endif   
            do 180 nb = 1, ntot 
  180       ikk(2,nb) = 0   
            do 220 j = next, jwrd   
               do 200 k = 1, ntotc  
                  if (arcnam(k) .eq. word(j)) then  
                     do 190 nb = 1, ntot
                        if (jarzn(nb) .eq. k) then  
                           if (busown(nb,kntown,ownlis) .eq. 1) then
                              ikk(2,nb) = 1 
                           endif
                        endif   
  190                continue   
                     go to 220  
                  endif 
  200          continue 
               last = lastch (word(j))  
               write (errbuf(1),210 ) word(j)(1:last)   
  210          format ('Interchange area (', a, ') is not in system')   
               call prterx ('W', 1) 
  220       continue
            go to 280   
         else if (word(i)(1:4) .eq. 'ZONE') then
            nwrd = i - 1
C                                                                      *
C           Check for "=" separator.                                   *
C                                                                      *
            if (word(i+1) .ne. '=') then
               last = lastch (word(i))  
               write (errbuf(1),170) word(i)(1:last)
               call prterx ('W', 1) 
               next = i + 1 
            else
               next = i + 2 
            endif   
            do 230 nb = 1, ntot 
  230       ikk(2,nb) = 0   
            do 260 j = next, jwrd   
               found = .false.  
               do 240 nb = 1, ntot  
                  if (zone(nb) .eq. word(j)) then   
                     found = .true. 
                     if (busown(nb,kntown,ownlis) .eq. 1) then  
                        ikk(2,nb) = 1   
                     endif  
                  endif 
  240          continue 
               if (.not.found) then 
                  last = lastch (word(j))   
                  write (errbuf(1),250 ) word(j)(1:last)
  250             format ('Zone (', a, ') is not in system')
                  call prterx ('W', 1)  
               endif
  260       continue
            go to 280   
         endif  
  270 continue  
C       
C     Neither ZONES nor AREAS have been specified.  The entire system   
C     is eligible.  
C       
      do 272 nb = 1, ntot   
         if (busown(nb,kntown,ownlis) .eq. 1) then  
            ikk(2,nb) = 1   
         endif  
  272 continue  
        
  280 continue  
C                                                                      *
C     Decode second set of parameters: "PLOAD", "QLOAD"                *
C                                                                      *
C     ..., PLOAD = ##% P + ##% I + ##% Z, -                            *
C     ..., QLOAD = ##% Q + ##% I + ##% Z                               *
C                                                                      *
      first = 2 
      chgtyp = 'P'  
      numtyp = 0
      firpc1 = numpc1 + 1
      laspc1 = numpc1 + 1
 
  290 if (first .lt. nwrd) then
 
         if (word(first) .eq. 'PLOAD' .or.
     1       word(first) .eq. 'ALOAD' .or.
     2       word(first) .eq. 'RLOAD') then
C
C           , ..., PLOAD = ##% P + ##% I + ##% Z
C           , ..., ALOAD = ##% P + ##% I + ##% Z
C           , ..., RLOAD = ##% P + ##% I + ##% Z
C
            if (word(first) .eq. 'PLOAD') then
               chgtyp = 'P'
            else if (word(first) .eq. 'ALOAD') then
               chgtyp = 'A'
            else if (word(first) .eq. 'RLOAD') then
               chgtyp = 'R'
            endif
            do 291 i = 1, numtyp
               if (chgtyp .eq. lodtyp(i)) then  
                  ixt = i   
                  go to 292 
               endif
  291       continue
            numtyp = numtyp + 1 
            ixt = numtyp
  292       psw(ixt) = .true.   
            lodtyp(ixt) = chgtyp
c                                                                      *
c           cHECK FOR "=" SEPARATOR.                                   *
c                                                                      *
            if (word(first+1) .ne. '=') then
               last = lastch (word(first))  
               write (errbuf(1),170) word(first)(1:last)
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
            endif   
        
            do 300 j = first + 2, nwrd, 3   
               if (word(j+1) .eq. 'P') then 
                  l = index (word(j), '%')  
                  if (l .ne. 0) word(j)(l:) = ' '   
                  pcnvt(1,ixt) = rval (word(j)) 
               else if (word(j+1) .eq. 'I') then
                  l = index (word(j), '%')  
                  if (l .ne. 0) word(j)(l:) = ' '   
                  pcnvt(2,ixt) = rval (word(j)) 
               else if (word(j+1) .eq. 'Z') then
                  l = index (word(j), '%')  
                  if (l .ne. 0) word(j)(l:) = ' '   
                  pcnvt(3,ixt) = rval (word(j)) 
               else 
                  first = j 
                  go to 290 
               endif
               if (word(j+2) .ne. '+') then 
                  first = j + 2 
                  go to 290 
               endif
  300       continue
        
         else if (word(first) .eq. 'QLOAD' .or. 
     1            word(first) .eq. 'BLOAD' .or. 
     2            word(first) .eq. 'XLOAD') then
C                                                                      *
C           , ..., QLOAD = ##% Q + ##% I + ##% Z                       *
C           , ..., BLOAD = ##% Q + ##% I + ##% Z                       *
C           , ..., XLOAD = ##% Q + ##% I + ##% Z                       *
C                                                                      *
            if (word(first) .eq. 'QLOAD') then  
               chgtyp = 'P' 
            else if (word(first) .eq. 'BLOAD') then 
               chgtyp = 'A' 
            else if (word(first) .eq. 'XLOAD') then 
               chgtyp = 'R' 
            endif   
            do 293 i = 1, numtyp
               if (chgtyp .eq. lodtyp(i)) then  
                  ixt = i   
                  go to 294 
               endif
  293       continue
            numtyp = numtyp + 1 
            ixt = numtyp
  294       qsw(ixt) = .true.   
            lodtyp(ixt) = chgtyp
C                                                                      *
C           Check for "=" separator.                                   *
C                                                                      *
            if (word(first+1) .ne. '=') then
               last = lastch (word(first))  
               write (errbuf(1),170) word(first)(1:last)
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
            endif   
        
            do 310 j = first + 2, nwrd, 3   
               if (word(j+1) .eq. 'Q') then 
                  l = index (word(j), '%')  
                  if (l .ne. 0) word(j)(l:) = ' '   
                  qcnvt(1,ixt) = rval (word(j)) 
               else if (word(j+1) .eq. 'I') then
                  l = index (word(j), '%')  
                  if (l .ne. 0) word(j)(l:) = ' '   
                  qcnvt(2,ixt) = rval (word(j)) 
               else if (word(j+1) .eq. 'Z') then
                  l = index (word(j), '%')  
                  if (l .ne. 0) word(j)(l:) = ' '   
                  qcnvt(3,ixt) = rval (word(j)) 
               else 
                  first = j 
                  go to 290 
               endif
               if (word(j+2) .ne. '+') then 
                  first = j + 2 
                  go to 290 
               endif
  310       continue
        
         else   
        
            last = lastch (word(first)) 
            write (errbuf(1),320) word(first)(1:last)   
  320       format ('Unrecognized > CHANGE_SYSTEM kewword (', a, ')')   
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            error = 1   
        
         endif  
        
      endif 
C                                                                      *
C     Check percentage sums                                            *
C                                                                      *
      lastpc = 0
      do 10360 ixt = 1, numtyp  
      if (psw(ixt)) then
         pcntp = pcnvt(1,ixt) + pcnvt(2,ixt) + pcnvt(3,ixt) 
         if (abs (pcntp - 100.0) .gt. 0.1) then 
            write (errbuf(1),330 ) pcnvt(1,ixt), 'CONSTANT_P'   
  330       format ('%p DOES NOT TOTAL 100%: ', f6.1, 1x, a)
            write (errbuf(2),340 ) pcnvt(2,ixt), 'CONSTANT_I'   
  340       format (t25, f6.1, 1x, a)   
            write (errbuf(3),340 ) pcnvt(3,ixt), 'CONSTANT_Z'   
            write (errbuf(4),340 ) pcntp, 'TOTAL'   
            if (is_batch .eq. 0) then
               call prterx ('E',4)
            else
               call prterx ('F',4)
            endif
            error = 1   
            return  
         endif  
      endif 
        
      if (qsw(ixt)) then
         pcntq = qcnvt(1,ixt) + qcnvt(2,ixt) + qcnvt(3,ixt) 
         if (abs (pcntq - 100.0) .gt. 0.1) then 
            write (errbuf(1),350 ) qcnvt(1,ixt), 'CONSTANT_Q'   
  350       format ('%Q does not total 100%: ', f6.1, 1x, a)
            write (errbuf(2),340 ) qcnvt(2,ixt), 'CONSTANT_I'   
            write (errbuf(3),340 ) qcnvt(3,ixt), 'CONSTANT_Z'   
            write (errbuf(4),340 ) pcntq, 'TOTAL'   
            if (is_batch .eq. 0) then
               call prterx ('E',4)
            else
               call prterx ('F',4)
            endif
            error = 1   
            return  
         endif  
        
      endif 
C       
C     Generate corresponding BUSCOD entity. 
C       
      if (kntown .eq. 0) then   
         kntown = 1 
         ownlis(kntown) = '###' 
      else  
C       
C        Sort OWNLIS
C       
         do 354 i = 1, kntown-1 
            do 352 j = i+1, kntown  
               komp = kompr(ownlis(i), ownlis(i), komp) 
               if (komp .gt. 0) then
                   owntmp = ownlis(i)   
                   ownlis(j) = ownlis(i)
                   ownlis(i) = owntmp   
               endif
  352       continue
  354    continue   
        
      endif 
        
      do 358 i = 1, kntown  
        
         if (numpc1 .lt. 2*MAXBUS .and. numpc2 .lt. MAXBUS) then
            numpc1 = numpc1 + 1 
            numpc2 = numpc2 + 1 
            buscod(numpc1)(1:2) = '##'  
            buscod(numpc1)(3:5) = ownlis(i) 
            buscod(numpc1)(6:8) = '###' 
            buscod(numpc1)(9:9) = lodtyp(ixt)   
            businx(numpc1) = numpc2 
            do 356 j = 1, 3 
               buspct(1,j,numpc2) = pcnvt(j,ixt)
               buspct(2,j,numpc2) = qcnvt(j,ixt)
  356       continue
            lstbus(numpc1) = lastpc 
            nxtbus(numpc1) = 0  
            if (lastpc .gt. 0) nxtbus(lastpc) = numpc1  
            lastpc = numpc1 
        
         else   
        
            write (errbuf(1), 460) numpc1   
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            error = 1   
            return  
        
         endif  
        
  358 continue  
10360 continue  
c                                                                      *
c     cHECK COMPATIBILITY IF MULTIPLE pload, aload, rload CHANGS.   
c                                                                      *
      if (numtyp .gt. 1) then   
         do 10361 i = 1, 6  
            systot(1,i) = 0.0   
            systot(2,i) = 0.0   
10361    continue   
         do 10363 ixt = 1, numtyp   
            jxt = businx(ixt)   
            do 10362 j = 1, 3   
               systot(1,j) = systot(1,j) + buspct(1,j,jxt)  
               systot(2,j) = systot(2,j) + buspct(2,j,jxt)  
10362       continue
10363    continue
         nump = 0
         numq = 0
         do 10364 i = 1,3
            if (systot(1,i) .ne. 0.0) nump = nump + 1
            if (systot(2,i) .ne. 0.0) numq = numq + 1
10364    continue
         if (nump .gt. 1 .or. numq .gt. 1) then
            write (errbuf(1),10365) nump, numq
10365       format ('Inconsistent target of PLOAD, ALOAD, RLOAD ',
     &              'distributions: P (', i2, ') Q (', i2, ')')
            write (errbuf(2), 10366) systot(1,1), systot(1,2),
     1          systot(1,3)
10366       format ('NET PLOAD =', f8.1, ' ALOAD =', f8.1, ' RLOAD =',
     1               f8.1)
            write (errbuf(3), 10367) systot(2,1), systot(2,2),
     1          systot(2,3)
10367       format ('NET QLOAD =', f8.1, ' BLOAD =', f8.1, ' XLOAD =',
     1               f8.1)
            if (is_batch .eq. 0) then
               call prterx ('E',3)
            else
               call prterx ('F',3)
            endif
            error = 1   
            return  
         endif  
      endif 
C                                                                      *
C     Before assigning percentage changes to candidate buses, read     *
C     ahead to determine whether any > EXCLUDE buses apply.            *
C                                                                      *
  360 if (endrcd) then  
         buf = '( END ) CNVTLD' 
         card = buf(1:1)
      else  
         read (inp, 150, end=362) buf   
         card = buf(1:1)
         call space (1) 
         write (outbuf,100) buf(1:80)   
         call prtout (1)
         if (card .eq. '.') go to 360   
         go to 364  
  362    endrcd = .true.
         buf = '( END ) CNVTLD' 
         card = buf(1:1)
  364    continue   
      endif 
C                                                                      *
C     Check for and concatenate continuation records.                  *
C                                                                      *
      bigbuf = buf  
      last = lastch (bigbuf)
      do while (bigbuf(last:last) .eq. '-') 
         read (inp, 150, end=381) buf   
         call space (1) 
         write (outbuf,100) buf(1:80)   
         call prtout (1)
         if (buf(1:1) .ne. '.') bigbuf(last:) = ljstfy (buf)
         last = lastch (bigbuf)
         buf = bigbuf   
      enddo
      go to 382
  381 endrcd = .true.
  382 continue
        
      if (card .eq. '>' .and. index (bigbuf,'EXCLUDE_BUS') .ne. 0) then 
c                                                                      *
C        > EXCLUDE_BUSSES <                                            *
C                                                                      *
  390    if (endrcd) then   
            buf = '( END ) CNVTLD'  
            card = buf(1:1) 
         else   
            read (inp, 150, end=392) buf
            card = buf(1:1) 
            call space (1)  
            write (outbuf,100) buf(1:80)
            call prtout (1) 
            if (card .eq. '.') go to 390
            go to 394   
  392       endrcd = .true. 
            buf = '( END ) CNVTLD'  
            card = buf(1:1) 
  394       continue
         endif  
        
         if (card .eq. 'B') then
            read (buf, 400) bus1, base1 
  400       format (bz, t7, a8, f4.0)   
            nb = find_bus (bus1, base1)   
            if (nb .le. 0) then 
               write (errbuf(1),410) bus1, base1
  410          format ('EXCLUDE_BUS (', a8, f6.1,   
     1            ') is not in system.')
               call prterx ('W', 1) 
            else
               ikk(2,nb) = 0
            endif   
            go to 390   
         endif  
        
      endif 
C       
C     Link up candidates defined by current > CHANGE_SYSTEM 
C       
      do 416 nb = 1, ntot   
         if (ikk(2,nb) .ne. 0) then 
            if (ikk(1,nb) .eq. 0) then  
C       
C              If no > CHANGE_SYSTEM is already linked to this bus, 
C              use the existing threaded list.  
C       
               ikk(1,nb) = firpc1   
            else if (compld(numcp1, ikk(1,nb), firpc1, ix) .gt. 0) then 
C       
C              If a existing > CHANGE_SYSTEM or > CHANGE_BUS already
C              applies to this bus, determine whether any other 
C              threaded list suffices.  
C       
               ikk(1,nb) = ix   
            else
C       
C              Create a copy of the existing threaded list and append   
C              to it the new threaded list BUSCOD(I).   
C       
               icnvt = ikk(1,nb)
               last = 0 
10414          if (icnvt .gt. 0) then   
                  numpc1 = numpc1 + 1   
                  if (numpc1 .lt. 2*MAXBUS) then
                     numpc1 = numpc1 + 1
                     buscod(numpc1)(1:7) = buscod(icnvt)(1:7)   
                     buscod(numpc1)(8:8) = '#'  
                     buscod(numpc1)(9:9) = buscod(icnvt)(9:9)   
                     businx(numpc1) = businx(icnvt) 
                     if (last .eq. 0) then  
                        ikk(1,nb) = numpc1  
                     else   
                        nxtbus(last) = numpc1   
                     endif  
                     lstbus(numpc1) = last  
                     nxtbus(numpc1) = 0 
                     last = numpc1  
                  else  
                     write (errbuf(1),460 ) numpc1  
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     error = 1  
                     return 
                  endif 
                  icnvt = nxtbus(icnvt) 
                  go to 10414   
               endif
C       
C              Append current set of %LOAD distribution codes to
C              current bus. 
C       
               do 10422 i = firpc1, laspc1  
C       
C                 Insert BUSCOD(NUMPC1) into sorted list.   
C       
                  if (numpc1 .lt. 2*MAXBUS) then
                     numpc1 = numpc1 + 1
                     buscod(numpc1)(1:7) = buscod(i)(1:7)   
                     buscod(numpc1)(8:8) = '#'  
                     buscod(numpc1)(9:9) = buscod(i)(9:9)   
                     businx(numpc1) = businx(i) 
                     if (last .eq. 0) then  
                        ikk(1,nb) = numpc1  
                     else   
                        nxtbus(last) = numpc1   
                     endif  
                     lstbus(numpc1) = last  
                     nxtbus(numpc1) = 0 
                     last = numpc1  
                     go to 10422
                  else  
                     write (errbuf(1),460 ) numpc1  
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     error = 1  
                     return 
                  endif 
10422          continue 
        
            endif   
        
         endif  
  416 continue  
      go to 126 
C                                                                      *
C     Process > CHANGE_BUSSES records here.                            *
C                                                                      *
C             > CHANGE_BUSSES, CHANGE_TYPE = PLOAD                     *
C                                            ALOAD                     *
C                                            RLOAD                     *
C                                                                      *
  420 if (card .eq. '>' .and. index (bigbuf,'CHANGE_BUS') .ne. 0) then  
        
         call uscan(bigbuf,word,nwrd,'=+',' ,/\\<>()')  
         if (nwrd .gt. 1) then  
            if (findex (word(nwrd), 'PLOAD') .ne. 0) then   
               chgtyp = 'P' 
            else if (findex (word(nwrd), 'ALOAD') .ne. 0) then  
               chgtyp = 'A' 
            else if (findex (word(nwrd), 'RLOAD') .ne. 0) then  
               chgtyp = 'R' 
            else
               last = lastch (word(nwrd))   
               write (errbuf(1), 421) word(nwrd)(1:last)
  421          format ('Unrecognized CHANGE_TYPE (',
     1            a, '). "P" presumed. ')   
               call prterx ('W',1)  
               chgtyp = 'P' 
            endif   
         else   
            chgtyp = 'P'
         endif  
        
      else  
         go to 500  
      endif 
        
C     > BUS 
C     .                      Constant P   Constant I   Constant Z Type *
C     .                      (MW) (MVAR)  (MW) (MVAR)  (MW)(MVAR)      *
C     . 
C     B  own  bus_name base   ###   ###    ###   ###    ###   ### PLOAD*
C     B  own  bus_name base   ###   ###    ###   ###    ###   ### ALOAD*
C     +X own  bus_name base   ###   ###    ###   ###    ###   ###      *
C     +X own  bus_name base   ###   ###    ###   ###    ###   ###      *
C                                                                      *
  422 if (endrcd) then  
         buf = '( END ) CNVTLD' 
         card = buf(1:1)
      else  
         read (inp, 150, end=424) buf   
         card = buf(1:1)
         call space (1) 
         write (outbuf,100) buf(1:80)   
         call prtout (1)
         if (card .eq. '.') go to 422   
         go to 426  
  424    endrcd = .true.
         buf = '( END ) CNVTLD' 
         card = buf(1:1)
  426    continue   
      endif 
        
      if (card .eq. 'B' .or. card .eq. '+') then
        
         if (numpc1 .lt. 2*MAXBUS .and. numpc2 .lt. MAXBUS) then
            numpc1 = numpc1 + 1 
            numpc2 = numpc2 + 2 
            if (card .eq. 'B') then 
               read (buf,430) buscod(numpc1)(1:2),  
     1            buscod(numpc1)(3:5),  
     2            bus1, base1,  
     3           (buspct(1,i,numpc2),buspct(2,i,numpc2), i = 1, 3)  
  430          format (bz, a2, 1x, a3, a8, f4.0, 1x, f5.0, 1x, f5.0,
     1            2(2x, f5.0, 1x, f5.0))
            else
               read (buf,432) buscod(numpc1)(1:2),  
     1            buscod(numpc1)(3:5),  
     2            bus1, base1, buscod(numpc1)(6:7), 
     3           (buspct(1,i,numpc2),buspct(2,i,numpc2), i = 1, 3)  
  432          format (bz, a2, 1x, a3, a8, f4.0, a2, f4.0, 1x, f5.0,
     1            2(2x, f5.0, 1x, f5.0))
            endif   
            buscod(numpc1)(8:8) = ' '   
            businx(numpc1) = numpc2 
            if (findex(buf(58:),'P') .ne. 0) then   
               buschg = 'P' 
            else if (findex(buf(58:),'A') .ne. 0) then  
               buschg = 'A' 
            else if (findex(buf(58:),'R') .ne. 0) then  
               buschg = 'R' 
            else
               buschg = chgtyp  
            endif   
            buscod(numpc1)(9:9) = buschg
            if (buf(1:1) .eq. 'B' .and. buschg .ne. 'P') then   
               write (errbuf(1),438) buf(1:1), bus1, base1, buschg  
  438          format ('Illegal %LOAD change on > BUS record ',a, 1x,   
     1             a8, f6.1, ' Change type "', a, '")') 
               call prterx ('W', 1) 
            endif   
            nb = find_bus (bus1, base1)   
            if (nb .le. 0) then 
               write (errbuf(1),440) bus1, base1
  440          format ('> BUS (', a8, f6.1, ') is not in system.')  
               call prterx ('W', 1) 
            else if (buf(1:1) .eq. '+') then
C       
C              Search for valid continuation bus.   
C       
               found = .false.  
               icb = kbsdta(15,nb) 
               do while (icb .gt. 0 .and. .not. found)
                  call getchr(1,cbtyp,kbctbl(8,icb)) 
                  call getchr(2,cbkyr,kbctbl(9,icb)) 
                  call getchr(3,cbown,kbctbl(10,icb))
                  if (cbtyp .eq. buscod(numpc1)(2:2) .and.   
     1                cbown .eq. buscod(numpc1)(3:5) .and.   
     2                cbkyr .eq. buscod(numpc1)(6:7)) then   
                     found = .true.  
                  endif  
                  icb = bctbl_nxt(icb)
               enddo

               if (.not.found) then 
                  write (errbuf(1),10446) buf(1:2), buf(3:5), bus1, 
     1               base1, buf(19:20)  
10446             format ('Continuation record type ', a2,  
     1               ' owner ', a3, ' bus ', a8, f6.1, ' code year ',   
     2               a2, ' is not in system.')  
                  call prterx ('W', 1)  
                  nb = 0
               endif
            endif   
            if (nb .gt. 0) then 
               numopc = numpc1  
C       
C              Create a copy of any existing threaded list and append   
C              to it the new threaded list BUSCOD(I).   
C       
               icnvt = ikk(1,nb)
               last = 0 
10447          if (icnvt .gt. 0) then   
                  numpc1 = numpc1 + 1   
                  if (numpc1 .lt. 2*MAXBUS) then
                     numpc1 = numpc1 + 1
                     buscod(numpc1) = buscod(icnvt) 
                     businx(numpc1) = businx(icnvt) 
                     if (last .eq. 0) then  
                        ikk(1,nb) = numpc1  
                     else   
                        nxtbus(last) = numpc1   
                     endif  
                     lstbus(numpc1) = last  
                     nxtbus(numpc1) = 0 
                     last = numpc1  
                  else  
                     write (errbuf(1),460 ) numpc1  
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     error = 1  
                     return 
                  endif 
                  icnvt = nxtbus(icnvt) 
                  go to 10447   
               endif
C       
C              Append current %LOAD distribution code to bus.   
C              Individually selected buses have precedence  
C              over > CHANGE_SYSTEM selection. Insert after any 
C              other >CHANGE_BUS but before any other >CHANGE_SYSTEM.   
C       
               next = ikk(1,nb) 
               last = 0 
  442          if (next .gt. 0) then
                  if (buscod(next)(8:8) .eq. '#') then  
                  else  
                     last = next
                     next = nxtbus(next)
                     go to 442  
                  endif 
               endif
C       
C              Insert BUSCOD(NUMOPC) into the threaded list.
C       
               if (last .eq. 0) then
                  ikk(1,nb) = numopc
               else 
                  nxtbus(last) = numopc 
               endif
               nxtbus(numopc) = next
               lstbus(numopc) = last
               if (next .eq. 0) then
               else 
                  lstbus(next) = numopc 
               endif
               ikk(2,nb) = 1
C                                                                      *
C              Check percentage sums                                   *
C                                                                      *
               pcntp = buspct(1,1,numpc2) + 
     1                 buspct(1,2,numpc2) + buspct(1,3,numpc2)  
               if (abs (pcntp - 100.0) .gt. 0.1) then   
                  write (errbuf(1),330 ) buspct(1,1,numpc2),
     1               'CONSTANT_P'   
                  write (errbuf(2),340 ) buspct(1,2,numpc2),
     1               'CONSTANT_I'   
                  write (errbuf(3),340 ) buspct(1,3,numpc2),
     1               'CONSTANT_Z'   
                  write (errbuf(4),340 ) pcntp, 'TOTAL' 
                  if (is_batch .eq. 0) then
                     call prterx ('E',4)
                  else
                     call prterx ('F',4)
                  endif
                  error = 1 
               endif
        
               pcntp = buspct(2,1,numpc2) + 
     1                 buspct(2,2,numpc2) + buspct(2,3,numpc2)  
               if (abs (pcntp - 100.0) .gt. 0.1) then   
                  write (errbuf(1),350 ) buspct(2,1,numpc2),
     1               'CONSTANT_Q'   
                  write (errbuf(2),340 ) buspct(2,2,numpc2),
     1               'CONSTANT_I'   
                  write (errbuf(3),340 ) buspct(2,3,numpc2),
     1               'CONSTANT_Z'   
                  write (errbuf(4),340 ) pcntp, 'TOTAL' 
                  if (is_batch .eq. 0) then
                     call prterx ('E',4)
                  else
                     call prterx ('F',4)
                  endif
                  error = 1 
               endif
        
            endif   
            go to 422   
        
         else   
        
            write (errbuf(1),460 ) numpc1   
  460       format ('More than ', i5, ' >%LOAD BUS records')
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            error = 1   
            return  
        
         endif  
        
      endif 
      go to 126 
C       
C     Check for unprocessed %LOAD_DISTRIBUTION records. 
C       
  500 if (card .eq. '>') then   
        
         call uscan(buf,word,nwrd,'=+',' ,/\\<>()') 
         last = lastch (word(2))
         write (errbuf(1), 512) word(2)(1:last) 
  512    format ('Unrecognized %LOAD_DISTRIBUTION > command (', a, ')') 
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         error = 1  
        
      endif 
C                                                                      *
C     Perform percentage changes                                       *
C                                                                      *
      call forbtm   
      write (outbuf,510 )   
  510 format (t53, ' Summary of %LOAD_DISTRIBUTION Conversion ')
      call shdlod(1)
      last = lastch (defvlt)
      write (outbuf,520) defvlt(1:last), defvlt(1:last) 
  520 format (  
     1   ' Type Owner Zone Bus              --- Original loads at ', a, 
     2   ' voltage --', t80, '--- Converted loads at ', a,  
     3   ' voltage --', t124, 'Voltage')
      call shdlod(2)
      write (outbuf,530)
  530 format (
     1   '                                  Constant MVA    ',
     &   'Constant I Contant Z       Constant MVA    Constant I',
     &   '     Constant Z    (p.u)')
      call shdlod(3)
      write (outbuf,540)
  540 format (  
     1   '                                   (MW) (MVAR)    (MW) (MVAR) 
     2 (mw) (mvar)       (mw) (mvar)   (mw) (mvar)   (mw) (mvar)')  
      call shdlod(4)
        
      write (outbuf,550)
  550 format (t80, 3x, '%Px ', 3x, '%Qx ', 3x, '%Px ', 3x, '%Qx ', 3x,  
     1   '%Px ', 3x, '%Qx ')
      call shdlod(5)
      call fortop   
C                                                                      *
C     Initialize system totals                                         *
C                                                                      *
      do 560 i = 1, 6   
         systot(1,i) = 0.0  
         systot(2,i) = 0.0  
  560 continue  
C                                                                      *
C     FREESP is a count of free entities available in BCTBL            *
C                                                                      *
      freesp = 0
      addcbs = 0
      oldcvt = 0
        
      do 770 nb = 1, ntot   
C                                                                      *
C        Initialize bus totals                                         *
C                                                                      *
         icnvt = ikk(1,nb)  
         if (icnvt .eq. 0) go to 770
        
         kt = inp2opt(nb) 
         ntyp = ntypu(kt)                           

C        Skip DC Busses

         if (ntyp .eq. 5 .or. ntyp .eq. 12) go to 770   
c
C        Use either BASE voltages or NOMINAL voltages, depending
C        upon the option selected.  
c
         if (defvlt(1:3) .eq. 'NOM') then   
            vk = 1.0
         else   
            vk = dsqrt (e(kt) ** 2 + f(kt) ** 2) 
         endif  
         vk2 = vk ** 2  
         ntemp = 0  
         prntbs = .false.   
         busflg = .false.   
C       
C        Flag duplicates.   
C       
         last = ikk(1,nb)   
         next = last
10560    next = nxtbus(next)
         if (next .gt. 0) then  
            if (buscod(last) .eq. buscod(next)) then
               write (errbuf(1),10562) bus(nb), base(nb), buscod(last)
10562          format ('Duplicate >CHANGE_BUS or >CHANGE_SYSTEM ',
     &         'records (', a8, f6.1, 1x, a, '). Second one ignored')
               call prterx ('W',1)
            endif
            go to 10560
         else
            last = nxtbus(last)
            next = last
            if (last .gt. 0) go to 10560
         endif  
         go to 563  
        
  562    if (nxtbus(icnvt) .gt. 0) then 
            icnvt = nxtbus(icnvt)   
            go to 563   
         else   
            icnvt = ikk(1,nb)   
            go to 564   
         endif  
C       
C        Find relevant %LOAD_DISTRIBUTION record. Note that there   
C        is only a "PLOAD" or "QLOAD" test here. "ALOAD", "BLOAD",  
C        "RLOAD", and "XLOAD" do not apply to "B" records.  
C       
  563    chgtyp = buscod(icnvt)(9:9)
         if (buscod(icnvt)(1:1) .eq. '#') then  
            if (buscod(icnvt)(3:5) .eq. '###' .or.  
     1          buscod(icnvt)(3:5) .eq. owner(nb)) then 
               if (busdta(3,nb) .ne. 0.0 .or. busdta(4,nb) .ne. 0.0)
     1            then  
                  if (chgtyp .eq. 'P') then 
                     busflg = .true.
                  else  
                     go to 562  
                  endif 
               else 
                  go to 562 
               endif
            else
               go to 562
            endif   
         else if (buscod(icnvt)(1:1) .eq. 'B') then 
            if (busdta(3,nb) .ne. 0.0 .or. busdta(4,nb) .ne. 0.0)
     1         then  
               if (chgtyp .eq. 'P') then 
                  busflg = .true.
               else  
                  go to 562  
               endif 
            else 
               go to 562 
            endif
         else   
            go to 562   
         endif  
  564    continue   
        
         if ( busflg ) then   

            ibcnvt = businx(icnvt)  
            pctp1 = 0.01 * buspct(1,1,ibcnvt)   
            pctq1 = 0.01 * buspct(2,1,ibcnvt)   
            pctp2 = 0.01 * buspct(1,2,ibcnvt)   
            pctq2 = 0.01 * buspct(2,2,ibcnvt)   
            pctp3 = 0.01 * buspct(1,3,ibcnvt)   
            pctq3 = 0.01 * buspct(2,3,ibcnvt)   
        
            ip1 = buspct(1,1,ibcnvt)
            iq1 = buspct(2,1,ibcnvt)
            ip2 = buspct(1,2,ibcnvt)
            iq2 = buspct(2,2,ibcnvt)
            ip3 = buspct(1,3,ibcnvt)
            iq3 = buspct(2,3,ibcnvt)
        
            if (chgtyp .eq. 'P') then   
               write (outbuf,570 ) ip1, iq1, ip2, iq2, ip3, iq3 
  570          format ('0', t36, '  PL     QL', t79, i5, '%PL', i5, 
     1            '%QL', i5, '%PL', i5, '%QL', i5, '%PL', i5,   
     2            '%QL ')   
               call prtout (1)  
            else if (chgtyp .eq. 'A') then  
               write (outbuf,571 ) ip1, iq1, ip2, iq2, ip3, iq3 
  571          format ('0', t50, '  PI     QI', t79, i5, '%PI', i5, 
     1            '%QI', i5, '%PI', i5, '%QI', i5, '%PI', i5,   
     2            '%QI ')   
               call prtout (1)  
            else if (chgtyp .eq. 'R') then  
               write (outbuf,572 ) ip1, iq1, ip2, iq2, ip3, iq3 
  572          format ('0', t64, '  PZ     QZ', t79, i5, '%PZ', i5, 
     1            '%QZ', i5, '%PZ', i5, '%QZ', i5, '%PZ', i5,   
     2            '%QZ ')   
               call prtout (1)  
            endif   
            oldcvt = icnvt  
        
         endif  
        
         do 573 i = 1, 6
            bustot(1,i) = 0.0   
            bustot(2,i) = 0.0   
  573    continue   
C       
         if (busflg) then   
C                                                                      *
C           Distribute PLO + jQLO                                      *
C                                                                      *
            plo = busdta(3,nb)  
            qlo = busdta(4,nb)  
            pio = 0.0   
            qio = 0.0   
            pzo = busdta(5,nb)  
            qzo = busdta(6,nb)  
        
            bustot(1,1) = plo   
            bustot(2,1) = qlo   
            bustot(1,2) = pio * vk  
            bustot(2,2) = qio * vk  
            bustot(1,3) = pzo * vk2 
            bustot(2,3) = qzo * vk2 
C                                                                      *
C           Distribute PLO + jQLO to PLN + jQLN on bus record          *
C           and to PIN + jQIN, PZN + jQZN on new +A01 record.          *
C                                                                      *
C           Note: +A01 pertains to the original form of constant       *
C           current.  It is superseded by the newer form +X *I, where  *
C           X is the original subtype from which the original quantity *
C           was derived.                                               *
C                                                                      *
C           A similar relationship exists between +A02 and +X *P.      *
C           For expositional convenience, the terms +A01 and +A02      *
C           are used, but they pertain to their newer forms.           *
C                                                                      *
            pln = pctp1 * plo   
            qln = pctq1 * qlo   
            pin = pio   
            qin = qio   
            pzn = pzo   
            qzn = qzo   
        
            busdta(3,nb) = pln  
            busdta(4,nb) = qln  
        
            bustot(1,4) = pln   
            bustot(2,4) = qln   
            bustot(1,5) = pin * vk  
            bustot(2,5) = qin * vk  
            bustot(1,6) = pzn * vk2 
            bustot(2,6) = qzn * vk2 
        
            type = 'B' // bustyp(ntyp)  
            write (outbuf,580 ) type(1:2), owner(nb), zone(nb), 
     1         bus(nb), base(nb), plo, qlo, pio * vk, qio * vk, 
     2         pzo * vk2, qzo * vk2, pln, qln, pin * vk,
     3         qin * vk, pzo * vk2, qzo * vk2, vk   
  580       format ('0', a2, t8, a3, t14, a2, t18, a8, f6.1, t33,   
     1         6f7.1, t79, 6f7.1, t124, f6.3)   
            call prtout (1) 
            prntbs = .true. 
C                                                                      *
C           PLO + jQLO, PLN + jQLN are implicitly zero for the +A01    *
c           record.                                                    *
C                                                                      *
            pio = 0.0   
            qio = 0.0   
            pzo = 0.0   
            qzo = 0.0   
        
            pin = pio + pctp2 * plo / vk
            qin = qio + pctq2 * qlo / vk
            pzn = pzo + pctp3 * plo / vk2   
            qzn = qzo - pctq3 * qlo / vk2   
        
            if (plo .ne. 0.0 .or. qlo .ne. 0.0) then
C       
C              Create new +A01 entity 
C       
               num1 = num1 + 1  
               if (ntemp .ge. 100) then 
                  write (errbuf(1), 581) ntemp, bus(nb), base(nb)   
  581             format ('More than ', i5, ' + records added from %LOAD
     &_distribution, overflow at bus ', a8, f6.1) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
                  error = 1 
                  go to 810 
               endif
               ntemp = ntemp + 1

               ktemp(1,ntemp) = nb  
               temp(2,ntemp) = pin  
               temp(3,ntemp) = qin  
               temp(4,ntemp) = pzn  
               temp(5,ntemp) = qzn  
               temp(6,ntemp) = 0.0  
               temp(7,ntemp) = 0.0  
               cbownx = owner(nb)   
               cbtypx = 'A' 
               cbkyrx = '*I'
               call putchr (1, cbtypx, ktemp(8,ntemp))  
               call putchr (2, cbkyrx, ktemp(9,ntemp))  
               call putchr (3, cbownx, ktemp(10,ntemp)) 
               temp(11,ntemp) = 0.0 
               temp(12,ntemp) = 0.0 
        
               bustot(1,5) = bustot(1,5) + pin * vk 
               bustot(2,5) = bustot(2,5) + qin * vk 
               bustot(1,6) = bustot(1,6) + pzn * vk2
               bustot(2,6) = bustot(2,6) + qzn * vk2
        
               type = '+' // cbtypx // cbkyrx   
               write (outbuf, 590) type, cbownx, 0.0, 0.0, pio * vk,
     1            qio * vk, pzo * vk2, qzo * vk2, 0.0, 0.0, 
     2            pin * vk, qin * vk, pzn * vk2, qzn * vk2  
  590          format (t2, a4, t8, a3, t18, '(New record)', t33,
     1            6f7.1, t79, 6f7.1)
               call prtout (1)  
        
            endif   
        
         endif  
C                                                                      *
C        First pass through customer buses:                            *
C                                                                      *
         icb = kbsdta(15,nb)
         do while (icb .gt. 0)
        
            call getchr(1,cbtyp,kbctbl(8,icb))  
            call getchr(2,cbkyr,kbctbl(9,icb))  
            call getchr(3,cbown,kbctbl(10,icb)) 
        
            ncount = 0

C           Transfer + entity array TEMP and delete original
C           entity.  This is necessary to keep the + records
C           linked to the bus record.
C
            if (ntemp+2 .ge. 100) then
               write (errbuf(1), 581) ntemp, bus(nb), base(nb)
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
               error = 1
               go to 810
            endif

            ntemp = ntemp + 1
            do i = 1, 12
               ktemp(i,ntemp) = kbctbl(i,icb)
            enddo
            ita00 = ntemp
C       
C           Create new +A01 entity 
C       
            if (ntemp .ge. 100) then 
               write (errbuf(1), 581) ntemp, bus(nb), base(nb)   
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
               error = 1 
               go to 810 
            endif
            ntemp = ntemp + 1   
            ita01 = ntemp   

            ktemp(1,ntemp) = nb 
            temp(2,ntemp) = 0.0 
            temp(3,ntemp) = 0.0 
            temp(4,ntemp) = 0.0 
            temp(5,ntemp) = 0.0 
            temp(6,ntemp) = 0.0 
            temp(7,ntemp) = 0.0 
C       
C           Use original owner (CBOWN) and type (CBTYP), but create 
C           new code year.  
C       
            cbtypx = cbtyp  
            cbkyrx = '*I'   
            cbownx = cbown  
        
            call putchr (1, cbtypx, ktemp(8,ntemp)) 
            call putchr (2, cbkyrx, ktemp(9,ntemp)) 
            call putchr (3, cbownx, ktemp(10,ntemp))
            temp(11,ntemp) = 0.0
            temp(12,ntemp) = 0.0
c       
C           Create a new or possibly pseudo +A02 entity in array
C           TEMP(*,ITA02) for receiving distributed constant P loads.   
C           This +A02 entity can assume one of the following three  
C           forms:  
C       
C           1. BUSDTA(*,NB) if conditions (a) and (b) are met.  
C       
C              a. CBTYPX = +A and (CBKYRX = 01 or CBKYRX = *I) .
C              b. CBOWNX = OWNER(NB).   
C       
C              If true, define new CBTYPX = '$' and CBKYRX = '*P'.  
C       
C           2. BCTBL(*,ITXXX), where ITXXX is the index of the original 
C              continuation bus from which ICB was derived, if  
C              conditions (a), (b), and (c) are met.
C       
C              a. CBTYPT = CBTYPX.  
C              b. CBKYRT = *I.  
C              c. CBOWNT = CBOWNX.  
C       
C              If true, define new CBTYPX = CBTYPT and CBKYRX = CBKYRT. 
C       
C           3. TEMP(*,ITA02) otherwise. This is the default value.  
C       
            ntemp = ntemp + 1   
            ita02 = ntemp   

            do i = 1, 12
               ktemp(i,ntemp) = ktemp(i,ntemp-1)   
            enddo
            cbkyrx = '*P'   
            call putchr (2, cbkyrx, ktemp(9,ntemp)) 
        
            if (cbtyp .eq. 'A' .and.
     1         (cbkyr .eq. '01' .or. cbkyr .eq. '*I') .and. 
     1          cbown .eq. owner(nb)) then  
C       
C              Flag bus for target. 
C       
               cbtypx = '$' 
               cbkyrx = '*P'
               call putchr (1, cbtypx, ktemp(8,ntemp))  
               call putchr (2, cbkyrx, ktemp(9,ntemp))  
        
            else if (cbkyr .eq. '*I') then  
        
               do 10596 i = 1, ita00-1  
                  call getchr(1,cbtypt,ktemp(8,i))  
                  call getchr(2,cbkyrt,ktemp(9,i))  
                  call getchr(3,cbownt,ktemp(10,i)) 
                  if (cbtypx .eq. cbtypt .and.  
     1                cbownx .eq. cbownt .and.  
     2                cbkyrt .ne. '*I') then
c       
c                    Flag original + bus for target.
c       
                     cbkyrx = cbkyrt
                     call putchr (2, cbkyrx, ktemp(9,ntemp))
                     go to 10597
                  endif 
10596          continue 
10597          continue 
        
            endif   
C       
C           Test if current +bus entity is affected by any %LOAD
C           record. 
C       
            cbsflg = .false.
            match = .false. 
            icnvt = ikk(1,nb)   
            go to 596   
        
  595       if (nxtbus(icnvt) .gt. 0) then  
               icnvt = nxtbus(icnvt)
               go to 596
            else
               icnvt = ikk(1,nb)
               go to 605
            endif   
        
  596       chgtyp = buscod(icnvt)(9:9) 
            if (buscod(icnvt)(1:1) .eq. '#') then   
               if (buscod(icnvt)(3:5) .eq. '###' .or.   
     1             buscod(icnvt)(3:5) .eq. cbown) then  
                  if (chgtyp .eq. 'P') then 
C       
C                    PLOAD and QLOAD distribution on all except +A01.   
C       
                     if ((cbkyr .ne. '*I') .and.
     1                   (cbtyp .ne. 'A' .or. cbkyr .ne. '01')) then
                        if (bctbl(2,icb) .ne. 0.0 .or.  
     1                      bctbl(3,icb) .ne. 0.0) then 
                           do 597 i = 1, ncount 
                              if (chg(i) .eq. chgtyp) go to 595 
  597                      continue 
                           ncount = ncount + 1  
                           chg(ncount) = chgtyp 
                           cbsflg = .true.  
                           match = .true.   
                        endif   
                     else   
                        go to 595   
                     endif  
                  else if (chgtyp .eq. 'A') then
C       
C                    ALOAD and BLOAD distribution only on +A01. 
C       
                     if ((cbkyr .eq. '*I') .or. 
     1                   (cbtyp .eq. 'A' .and. cbkyr .eq. '01')) then   
                        if (bctbl(2,icb) .ne. 0.0 .or.  
     1                      bctbl(3,icb) .ne. 0.0) then 
                           do 598 i = 1, ncount 
                              if (chg(i) .eq. chgtyp) go to 595 
  598                      continue 
                           ncount = ncount + 1  
                           chg(ncount) = chgtyp 
                           cbsflg = .true.  
                           match = .true.   
                        endif   
                     else   
                        go to 595   
                     endif  
                  else if (chgtyp .eq. 'R') then
C       
C                    ZLOAD distribution only on +A01 or +A02.   
C       
                     if ((cbkyr .eq. '*I' .or. cbkyr .eq. '*P') .or.
     1                   (cbtyp .eq. 'A' .and.  
     1                   (cbkyr .eq. '01' .or. cbkyr .eq. '02'))) then  
                        if (bctbl(4,icb) .ne. 0.0 .or.  
     1                      bctbl(5,icb) .ne. 0.0) then 
                           do 599 i = 1, ncount 
                              if (chg(i) .eq. chgtyp) go to 595 
  599                      continue 
                           ncount = ncount + 1  
                           chg(ncount) = chgtyp 
                           cbsflg = .true.  
                           match = .true.   
                        else
                           go to 595
                        endif   
                     else   
                        go to 595   
                     endif  
                  else  
                     write (errbuf(1), 10599) bus(nb), base(nb),
     1                  buscod(icnvt)   
10599                format ('Unrecognized change code for bus ', a8,   
     1                  f6.1, ' code (', a, ')')
                     call prterx ('W',1)
                     go to 595  
                  endif 
               else 
                  go to 595 
               endif
            else
               strng1 = buscod(icnvt)(2:7)  
               strng2 = cbtyp // cbown // cbkyr 
               if (strng1 .ne. strng2) then 
                  go to 595 
               else 
                  if (chgtyp .eq. 'P') then 
C       
C                    PLOAD and QLOAD distribution on all except +A01.   
C       
                     if ((cbkyr .ne. '*I') .and.
     1                   (cbtyp .ne. 'A' .and. cbkyr .ne. '01')) then   
                        if (bctbl(2,icb) .ne. 0.0 .or.  
     1                      bctbl(3,icb) .ne. 0.0) then 
                           do 600 i = 1, ncount 
                              if (chg(i) .eq. chgtyp) go to 595 
  600                      continue 
                           ncount = ncount + 1  
                           chg(ncount) = chgtyp 
                           cbsflg = .true.  
                           match = .true.   
                        endif   
                     else   
                        go to 595   
                     endif  
                  else if (chgtyp .eq. 'A') then
C       
C                    ALOAD and BLOAD distribution only on +A01. 
C       
                     if ((cbkyr .eq. '*I') .or. 
     1                   (cbtyp .eq. 'A' .and. cbkyr .eq. '01')) then   
                        if (bctbl(2,icb) .ne. 0.0 .or.  
     1                      bctbl(3,icb) .ne. 0.0) then 
                           do 601 i = 1, ncount 
                              if (chg(i) .eq. chgtyp) go to 595 
  601                      continue 
                           ncount = ncount + 1  
                           chg(ncount) = chgtyp 
                           cbsflg = .true.  
                           match = .true.   
                        endif   
                     else   
                        go to 595   
                     endif  
                  else  
C       
C                    ZLOAD distribution only on +A01 or +A02
C       
                     if ((cbkyr .eq. '*I' .or. cbkyr .eq. '*P') .or.
     1                   (cbtyp .eq. 'A' .and.  
     1                   (cbkyr .eq. '01' .or. cbkyr .eq. '02'))) then  
                        if (bctbl(4,icb) .ne. 0.0 .or.  
     1                      bctbl(5,icb) .ne. 0.0) then 
                           do 602 i = 1, ncount 
                              if (chg(i) .eq. chgtyp) go to 595 
  602                      continue 
                           ncount = ncount + 1  
                           chg(ncount) = chgtyp 
                           cbsflg = .true.  
                           match = .true.   
                        else
                           go to 595
                        endif   
                     endif  
                  endif 
               endif
        
            endif   
        
            if ( match  .and.  icnvt .ne. oldcvt ) then
        
               ibcnvt = businx(icnvt)   
               pctp1 = 0.01 * buspct(1,1,ibcnvt)
               pctq1 = 0.01 * buspct(2,1,ibcnvt)
               pctp2 = 0.01 * buspct(1,2,ibcnvt)
               pctq2 = 0.01 * buspct(2,2,ibcnvt)
               pctp3 = 0.01 * buspct(1,3,ibcnvt)
               pctq3 = 0.01 * buspct(2,3,ibcnvt)
        
               ip1 = buspct(1,1,ibcnvt) 
               iq1 = buspct(2,1,ibcnvt) 
               ip2 = buspct(1,2,ibcnvt) 
               iq2 = buspct(2,2,ibcnvt) 
               ip3 = buspct(1,3,ibcnvt) 
               iq3 = buspct(2,3,ibcnvt) 
        
               if (chgtyp .eq. 'P') then
                  write (outbuf,570 ) ip1, iq1, ip2, iq2, ip3, iq3  
                  call prtout (1)   
               else if (chgtyp .eq. 'A') then   
                  write (outbuf,571 ) ip1, iq1, ip2, iq2, ip3, iq3  
                  call prtout (1)   
               else if (chgtyp .eq. 'R') then   
                  write (outbuf,572 ) ip1, iq1, ip2, iq2, ip3, iq3  
                  call prtout (1)   
               endif
               oldcvt = icnvt   
        
            endif   
        
            if (match) then 
C                                                                      *
C              Store in TEMP the updated values of this record.        *
C                                                                      *
               if (chgtyp .eq. 'P') then
C       
C                 Process PLOAD + jQLOAD =  
C       
                  if ((cbkyr .eq. '*I') .or.
     1                (cbtyp .eq. 'A' .and. cbkyr .eq. '01')) then  
                     write (errbuf(1), 604) chgtyp, cbtyp, cbown,   
     1                  cbkyr   
  604                format ('Illegal %LOAD change type "', a,  
     1                  '" on +', a, a, 1x, a, ' record')   
                     call prterx ('W', 1)   
                  else if ((cbkyr .eq. '*P') .or.   
     1                     (cbtyp .eq. 'A' .and. cbkyr .eq. '02')) then 
                     plo = bctbl(2,icb) 
                     qlo = bctbl(3,icb) 
                     temp(2,ita00) = temp(2,ita00) - plo + pctp1 * plo  
                     temp(3,ita00) = temp(3,ita00) - qlo + pctq1 * qlo  
                     temp(4,ita00) = temp(4,ita00) + pctp3 * plo / vk2  
                     temp(5,ita00) = temp(5,ita00) - pctq3 * qlo / vk2  
C       
C                    Add distributed current to new +A01 entity in  
C                    array TEMP(*,ITA01).   
C       
                     temp(2,ita01) = temp(2,ita01) + pctp2 * plo / vk   
                     temp(3,ita01) = temp(3,ita01) + pctq2 * qlo / vk   
        
                  else  
        
                     plo = bctbl(2,icb) 
                     qlo = bctbl(3,icb) 
                     temp(2,ita00) = temp(2,ita00) - plo + pctp1 * plo  
                     temp(3,ita00) = temp(3,ita00) - qlo + pctq1 * qlo  
C       
C                    Add distributed current and impedance to new   
C                    +A01 entity in array TEMP(*,ITA01).
C       
                     temp(2,ita01) = temp(2,ita01) + pctp2 * plo / vk   
                     temp(3,ita01) = temp(3,ita01) + pctq2 * qlo / vk   
                     temp(4,ita01) = temp(4,ita01) + pctp3 * plo / vk2  
                     temp(5,ita01) = temp(5,ita01) - pctq3 * qlo / vk2  
                  endif 
        
               else if (chgtyp .eq. 'A') then   
C       
C                 Process ALOAD + jBLOAD =  
C       
                  if ((cbkyr .eq. '*I') .or.
     1                (cbtyp .eq. 'A' .and. cbkyr .eq. '01')) then  
                     pio = bctbl(2,icb) * vk
                     qio = bctbl(3,icb) * vk
                     temp(2,ita00) = temp(2,ita00) - pio / vk   
     1                                             + pctp2 * pio / vk   
                     temp(3,ita00) = temp(3,ita00) - qio / vk   
     1                                             + pctq2 * qio / vk   
                     temp(4,ita00) = temp(4,ita00) + pctp3 * pio / vk2  
                     temp(5,ita00) = temp(5,ita00) - pctq3 * qio / vk2  
c       
C                    Add distributed load to new +A02 entity in 
C                    array TEMP(*,ITA02).   
C       
                     temp(2,ita02) = temp(2,ita02) + pctp1 * pio
                     temp(3,ita02) = temp(3,ita02) + pctq1 * qio
                  else  
                     write (errbuf(1), 604) chgtyp, cbtyp, cbown,   
     1                  cbkyr   
                     call prterx ('W', 1)   
                  endif 
        
               else if (chgtyp .eq. 'R') then   
c       
C                 Process RLOAD + jXLOAD =  
C       
                  if ((cbkyr .eq. '*I') .or.
     1                (cbtyp .eq. 'A' .and. cbkyr .eq. '01')) then  
                     pzo = bctbl(4,icb) * vk2   
                     qzo = -bctbl(5,icb) * vk2  
                     temp(2,ita00) = temp(2,ita00) + pctp2 * pzo / vk   
                     temp(3,ita00) = temp(3,ita00) + pctq2 * qzo / vk   
                     temp(4,ita00) = temp(4,ita00) - pzo / vk2  
     1                                             + pctp3 * pzo / vk2  
                     temp(5,ita00) = temp(5,ita00) + qzo / vk2  
     1                                             - pctq3 * qzo / vk2  
C       
C                    Add distributed power to new +A02 entity in
C                    array TEMP(*,ITA02).   
C       
                     temp(2,ita02) = temp(2,ita02) + pctp1 * pzo
                     temp(3,ita02) = temp(3,ita02) + pctq1 * qzo
        
                  else if ((cbkyr .eq. '*P') .or.   
     1                     (cbtyp .eq. 'A' .and. cbkyr .eq. '02')) then 
        
                     pzo = bctbl(4,icb) * vk2   
                     qzo = -bctbl(5,icb) * vk2  
                     temp(2,ita00) = temp(2,ita00) + pctp1 * pzo
                     temp(3,ita00) = temp(3,ita00) + pctq1 * qzo
                     temp(4,ita00) = temp(4,ita00) - pzo / vk2  
     1                                             + pctp3 * pzo / vk2  
                     temp(5,ita00) = temp(5,ita00) + qzo / vk2  
     1                                             - pctq3 * qzo / vk2  
C       
C                    Add distributed current to new +A01 entity in  
C                    array TEMP(*,ITA01).   
C       
                     temp(2,ita01) = temp(2,ita01) + pctp2 * pzo / vk   
                     temp(3,ita01) = temp(3,ita01) + pctq2 * qzo / vk   
        
                  else  
        
                     write (errbuf(1), 604) chgtyp, cbtyp, cbown,   
     1                  cbkyr   
                     call prterx ('W', 1)   
                  endif 
               endif
C       
C              Get next BUSCOD record.  
C       
               match = .false.  
               go to 595
            endif   
  605       continue
        
            if (cbsflg) then
C                                                                      *
C              This record is affected with %LOAD_DISTRIBUTION         *
C              records. Recall that the updated + entity is in array   *
C              TEMP(*,ITA00).   
C       
               if ((cbkyr .eq. '*I') .or.   
     1             (cbtyp .eq. 'A' .and. cbkyr .eq. '01')) then 
                  pio = bctbl(2,icb)
                  qio = bctbl(3,icb)
                  plo = 0.0 
                  qlo = 0.0 
                  pzo = bctbl(4,icb)
                  qzo = bctbl(5,icb)
               else 
                  pio = 0.0 
                  qio = 0.0 
                  plo = bctbl(2,icb)
                  qlo = bctbl(3,icb)
                  pzo = bctbl(4,icb)
                  qzo = bctbl(5,icb)
               endif
               pln = 0.0
               qln = 0.0
               pin = 0.0
               qin = 0.0
               pzn = 0.0
               qzn = 0.0
        
               do 606 i = 1, ita02  
                  call getchr(1,cbtypx,ktemp(8,i))  
                  call getchr(2,cbkyrx,ktemp(9,i))  
                  call getchr(3,cbownx,ktemp(10,i)) 
                  if (cbtyp .eq. cbtypx .and.   
     1                cbkyr .eq. cbkyrx .and.   
     2                cbown .eq. cbownx) then   
                     if ((cbkyr .eq. '*I') .or. 
     1                  (cbtyp .eq. 'A' .and. cbkyr .eq. '01')) then
                        pin = pin + temp(2,i)   
                        qin = qin + temp(3,i)   
                     else   
                        pln = pln + temp(2,i)   
                        qln = qln + temp(3,i)   
                     endif  
                     pzn = pzn + temp(4,i)  
                     qzn = qzn + temp(5,i)  
                  endif 
  606          continue 
        
               bustot(1,1) = bustot(1,1) + plo  
               bustot(2,1) = bustot(2,1) + qlo  
               bustot(1,2) = bustot(1,2) + pio * vk 
               bustot(2,2) = bustot(2,2) + qio * vk 
               bustot(1,3) = bustot(1,3) + pzo * vk2
               bustot(2,3) = bustot(2,3) + qzo * vk2
        
               bustot(1,4) = bustot(1,4) + pln  
               bustot(2,4) = bustot(2,4) + qln  
               bustot(1,5) = bustot(1,5) + pin * vk 
               bustot(2,5) = bustot(2,5) + qin * vk 
               bustot(1,6) = bustot(1,6) + pzn * vk2
               bustot(2,6) = bustot(2,6) + qzn * vk2
        
               if (prntbs) then 
        
                  type = '+' // cbtyp // cbkyr  
                  write (outbuf,610) type, cbown, plo, qlo, pio * vk,   
     1               qio * vk, pzo * vk2, qzo * vk2, pln,   
     2               qln, pin * vk, qin * vk, pzn * vk2,
     3               qzn * vk2  
  610                format (t2, a4, t8, a3, t33, 6f7.1, t79, 6f7.1)
                  call prtout (1)   
        
               else 
        
                  type = '+' // cbtyp // cbkyr  
                  write (outbuf,612) type, cbown, zone(nb), bus(nb),
     1               base(nb), plo, qlo, pio * vk,  
     2               qio * vk, pzo * vk2, qzo * vk2, pln,   
     3               qln, pin * vk, qin * vk, pzn * vk2,
     4               qzn * vk2  
  612                format ('0', a4, t8, a3, t14, a2, t18, a8, f6.1,   
     1                  t33, 6f7.1, t79, 6f7.1) 
                  call prtout (1)   
                  prntbs = .true.   
        
               endif
C       
C              Print out +A01 record if non-zero entities exits and 
C              the original record is not an +A01 record.   
C       
               call getchr(1,cbtypx,ktemp(8,ita01)) 
               call getchr(2,cbkyrx,ktemp(9,ita01)) 
               call getchr(3,cbownx,ktemp(10,ita01))
        
               total = abs(temp(2,ita01)) + abs(temp(3,ita01))  
     1               + abs(temp(4,ita01)) + abs(temp(5,ita01))  
               if (total .gt. 0.0 .and. cbkyrx .ne. cbkyr) then 
        
                  num2 = num2 + 1   
                  plo = 0.0 
                  qlo = 0.0 
                  pio = 0.0 
                  qio = 0.0 
                  pzo = 0.0 
                  qzo = 0.0 
                  pln = 0.0 
                  qln = 0.0 
                  pin = temp(2,ita01)   
                  qin = temp(3,ita01)   
                  pzn = temp(4,ita01)   
                  qzn = temp(5,ita01)   
        
                  bustot(1,4) = bustot(1,4) + pln   
                  bustot(2,4) = bustot(2,4) + qln   
                  bustot(1,5) = bustot(1,5) + pin * vk  
                  bustot(2,5) = bustot(2,5) + qin * vk  
                  bustot(1,6) = bustot(1,6) + pzn * vk2 
                  bustot(2,6) = bustot(2,6) + qzn * vk2 
        
                  type = '+' // cbtypx // cbkyrx
                  write (outbuf,590 ) type, cbownx, plo, qlo,   
     1               pio * vk, qio * vk, pzo * vk2, 
     2               qzo * vk2, pln, qln, pin * vk, qin * vk,   
     3               pzn * vk2, qzn * vk2   
                  call prtout (1)   
        
               endif
C       
C              Print out +A02 record if non-zero entities exits 
C              and the original record is not a +A02 record.
C       
               call getchr(1,cbtypx,ktemp(8,ita02)) 
               call getchr(2,cbkyrx,ktemp(9,ita02)) 
               call getchr(3,cbownx,ktemp(10,ita02))
        
               total = abs(temp(2,ita02)) + abs(temp(3,ita02))  
     1               + abs(temp(4,ita02)) + abs(temp(5,ita02))  
               if (total .gt. 0.0 .and. cbkyr .ne. cbkyrx) then 
        
                  num2 = num2 + 1   
                  plo = 0.0 
                  qlo = 0.0 
                  pio = 0.0 
                  qio = 0.0 
                  pzo = 0.0 
                  qzo = 0.0 
                  pln = temp(2,ita02)   
                  qln = temp(3,ita02)   
                  pin = 0.0 
                  qin = 0.0 
                  pzn = temp(4,ita02)   
                  qzn = temp(5,ita02)   
        
                  bustot(1,4) = bustot(1,4) + pln   
                  bustot(2,4) = bustot(2,4) + qln   
                  bustot(1,5) = bustot(1,5) + pin * vk  
                  bustot(2,5) = bustot(2,5) + qin * vk  
                  bustot(1,6) = bustot(1,6) + pzn * vk2 
                  bustot(2,6) = bustot(2,6) + qzn * vk2 
        
C                 Print out according to interpretation of +A02 record. 
C       
                  if (cbtypx .eq. '$') then 
                     type = 'B' // ' ' // cbkyrx
                     tag = '(Modified)' 
C       
C                    Output total values on modified bus record.
C       
                     plo = plo + busdta(3,nb)   
                     qlo = qlo + busdta(4,nb)   
                     pln = pln + busdta(3,nb)   
                     qln = qln + busdta(4,nb)   
                     do 614 i = 1, ita02-1  
                        call getchr(1,cbtypt,ktemp(8,i))
                        call getchr(2,cbkyrt,ktemp(9,i))
                        call getchr(3,cbownt,ktemp(10,i))   
                        if (cbtypt .eq. '$') then   
                           plo = plo + temp(2,i)
                           qlo = qlo + temp(3,i)
                           pln = pln + temp(2,i)
                           qln = qln + temp(3,i)
                        endif   
  614                continue   
        
                  else if (cbtypx .ne. '*P') then   
c       
c                    Flag original + bus for target.
c       
                     type = '+' // cbtypx // cbkyrx 
                     tag = '(Modified)' 
c       
c                    Output total values on modified + bus record.  
c       
                     do 616 i = 1, ita02-1  
                        call getchr(1,cbtypt,ktemp(8,i))
                        call getchr(2,cbkyrt,ktemp(9,i))
                        call getchr(3,cbownt,ktemp(10,i))   
                        if (cbtypt .eq. cbtyp .and. 
     1                      cbownt .eq. cbown .and. 
     2                     (cbkyrt .ne. '*P' .and. cbkyrt .ne. '*I'))   
     3                        then  
                           plo = plo + temp(2,i)
                           qlo = qlo + temp(3,i)
                           pln = pln + temp(2,i)
                           qln = qln + temp(3,i)
                        endif   
  616                continue   
                  else  
                     type = '+' // cbtypx // cbkyrx 
                     tag = '(New record)'   
                  endif 
        
                  write (outbuf, 618) type, cbownx, tag, plo, qlo,  
     1               pio * vk, qio * vk, pzo * vk2, 
     2               qzo * vk2, pln, qln, pin * vk, qin * vk,   
     3               pzn * vk2, qzn * vk2   
  618             format (t2, a4, t8, a3, t18, a, t33, 6f7.1, t79,  
     1               6f7.1) 
                  call prtout (1)   
        
               endif
        
            endif   
            icb = bctbl_nxt(icb)
         enddo
  630    continue   
        
         if (ntemp .eq. 0) go to 742
C                                                                      *
C        Sort TEMP array                                               *
C                                                                      *
         dupsw = .false.
         call qiksrt (1, ntemp, komcbx, swpcbx) 
C                                                                      *
C        Consolidate any duplicates and create/update BCTBL entities.  *
C                                                                      *
         last = 0   

         icbtot = 0
         icb = kbsdta(15,nb)
         do while (icb .gt. 0)
            icbtot = icbtot + 1
            temp_index(icbtot) = icb
            icb = bctbl_nxt(icb)
         enddo
         icbnum = 0
         icblast = 0
*************** fixing pointer to + bus stuff *****************
         kbsdta(15,nb) = 0
*************** fixing pointer to + bus stuff *****************         
         do 740 i = 1, ntemp
        
            call getchr(1,cbtyp,ktemp(8,i)) 
            call getchr(2,cbkyr,ktemp(9,i)) 
            call getchr(3,cbown,ktemp(10,i))
c       
C           Append +A01 and +A02 records only if they contain   
C           non-zero entities.  
C       
            if ((cbkyr .eq. '*I' .or. cbkyr .eq. '*P') .or. 
     1          (cbtyp .eq. '$') .or.   
     2          (cbtyp .eq. 'A' .and.   
     3          (cbkyr .eq. '01' .or. cbkyr .eq. '02'))) then   
               total = abs(temp(2,i)) + abs(temp(3,i))  
     1               + abs(temp(4,i)) + abs(temp(5,i))  
               if (total .lt. 0.1) go to 740
            endif   
            strng2 = cbtyp // cbown // cbkyr
        
            if (cbtyp .eq. '$' .and. cbkyr .eq. '*P') then  
        
               busdta(3,nb) = busdta(3,nb) + temp(2,i)  
               busdta(4,nb) = busdta(4,nb) + temp(3,i)  
               if (temp(4,i) .ne. 0.0 .or. temp(5,i) .ne. 0.0) then 
                  write (errbuf(1), 708) bus(nb), base(nb), strng2, 
     1               temp(4,i), temp(5,i)   
  708             format ('%LOAD_DISTRIBUTION error: + Bus ', a8, f6.1, 
     1               ' Type ', a, ' has undistributed shunt ', 2f8.1)   
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
               endif
        
            else if (last .gt. 0) then  
        
               komp = kompr (strng1, strng2, komp)  
               if (komp .eq. 0) then
        
                  do 710 j = 2, 6   
                     bctbl(j,last) = bctbl(j,last) + temp(j,i)  
  710             continue  
                  bctbl(11,last) = bctbl(11,last) + temp(11,i)  
                  bctbl(12,last) = bctbl(12,last) + temp(12,i)  
        
               else if (komp .gt. 0) then   
        
                  write (errbuf(1), 712)
  712             format('Continuation bus records not sorted.')
                  call bcdcbs (last, bigbuf)
                  write (errbuf(2), 714) last, bigbuf(1:80) 
  714             format ('Record No. ', i5, ' (', a, ')')  
                  call bcdcbs (i,bigbuf)
                  write (errbuf(3), 714) i, bigbuf(1:80)
                  call prterx ('W', 3)  
        
               endif

               icbnum = icbnum + 1
               if (icbnum .le. icbtot) then
                  last = temp_index(icbnum)
               else
                  if (ntot2 .ge. MAXCBS) then
                     write (errbuf(1), 716) ntot2, bus(nb), base(nb)   
  716                format ('More than ', i5, ' + records in system aft
     &er %load_distribution at bus ', a8, f6.1) 
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     error = 1 
                     ntot2 = 0
                  endif
                  ntot2 = ntot2 + 1
                  last = ntot2  
               endif

               if (icblast .eq. 0) then
                  kbsdta(15,nb) = last   
               else 
                  bctbl_nxt(icblast) = last
               endif
               icblast = last
               bctbl_nxt(last) = 0        

               strng1 = strng2   
               do j = 1, 12  
                  kbctbl(j,last) = ktemp(j,i)   
               enddo

            else

               icbnum = icbnum + 1
               if (icbnum .le. icbtot) then
                  last = temp_index(icbnum)
               else
                  if (ntot2 .ge. MAXCBS) then
                     write (errbuf(1), 716) ntot2, bus(nb), base(nb)   
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     error = 1 
                     ntot2 = 0
                  endif
                  ntot2 = ntot2 + 1
                  last = ntot2  
               endif

               if (icblast .eq. 0) then
                  kbsdta(15,nb) = last   
               else 
                  bctbl_nxt(icblast) = last
               endif
               icblast = last
               bctbl_nxt(last) = 0        

               strng1 = strng2   
               do j = 1, 12  
                  kbctbl(j,last) = ktemp(j,i)   
               enddo

            endif   
        
  740    continue   
C       
C        Check subtotals
C       
  742    perr = bustot(1,1) + bustot(1,2) + bustot(1,3) 
     1        - bustot(1,4) - bustot(1,5) - bustot(1,6) 
         qerr = bustot(2,1) + bustot(2,2) - bustot(2,3) 
     1        - bustot(2,4) - bustot(2,5) + bustot(2,6) 
        
         if (abs (perr) .gt. 0.1 .or. abs (qerr) .gt. 0.1) then 
        
            write (errbuf(1), 750) bustot   
  750       format ('   Subtotal error ', t19, 6f7.1, t65, 6f7.1)   
            write (errbuf(2), 752) perr, qerr   
  752       format ('   Mismatch', t19, 2f7.1)  
            call prterx ('W', 2)
        
         endif  
        
         do 760 i = 1, 6
             systot(1,i) = systot(1,i) + bustot(1,i)
             systot(2,i) = systot(2,i) + bustot(2,i)
  760    continue   
        
  770 continue   ! End of big bus loop !!!
        
      write (outbuf,772 ) systot
  772 format ('0', t18, 'System Total', t33, 6f7.1, t79, 6f7.1) 
      call prtout (1)   
        
      write (outbuf, 774) num1, num2
  774 format ('0 ', i5, ' Buses, ',  i5,
     &        ' + Buses affected by / %LOAD_DISTRIBUTION')
      call prtout (1)

  810 continue
C                                                                      *
C     Check proper control card upon program exit                      *
C                                                                      *
      if (index ('[(/HSC', card) .eq. 0) then   
c                                                                      *
C        Meaningless control record                                    *
C                                                                      *
         write (errbuf(1),850) buf(1:80)
  850    format ('Unrecognized command: (', a80, ')')   
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         error = 1  
         return 
      endif 
        
  890 continue  
        
      outbuf = ' '  
      call rpnlod   
      call shdlod (1)   
      call shdlod (2)   
      call shdlod (3)   
      call shdlod (4)   
      call shdlod (5)   
        
      outbuf = '0End of %LOAD_DISTRIBUTION '
      call prtout(1)
      call forbtm   
        
      return
      end   

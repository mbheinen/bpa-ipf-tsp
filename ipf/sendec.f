C    @(#)sendec.f	20.3 2/13/96
      subroutine sendec
C
C        This subroutine decodes sensitivity records:
C
C        1. /BUS_SENSITIVITIES
C        2. /LINE_SENSITIVITIES
C        3. /TRANSFER_SENSITIVITIES
C        4. /LOSS_SENSITIVITIES
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/loscom.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
 
 
      character word(100) * 30, capital * 30, sentyp * 1,
     1          bigbuf * 512, comprs * 512
      logical found
 
      call space (1)
      write (outbuf,90 ) buf(1:80)
   90 format (' SOLUTION text: (',a,')')
      call prtout (1)
 
      call scan(buf,word,nwrd)
      do 100 i = 1, nwrd
         word(i) = capital (word(i))
  100 continue
      if (word(1)(1:6) .eq. 'BUSSEN') then
C
C        /BUS_SENSITIVITY,LTC=ON,AI_CONTROL=CON,Q_SHUNT=ADJ,Q_GEN=ADJ
C                             OFF,          OFF         FIXED     FIXED
C                                           MON
         kspare(19) = 2
         kspare(20) = 1
         kspare(39) = 0
         kspare(40) = 0
         do 110 i = 2, nwrd - 1, 2
            if (word(i)(1:3) .eq. 'LTC') then
               if (word(i+1) .eq. 'OFF') kspare(19) = 0
               if (word(i+1) .eq. 'ONNVL') kspare(19) = 1
            else if (word(i)(1:2) .eq. 'AI') then
               if (word(i+1) .eq. 'OFF') kspare(20) = 0
               if (word(i+1) .eq. 'MON') kspare(20) = 2
            else if (word(i)(1:6) .eq. 'QSHUNT') then
               if (word(i+1) .eq. 'FIXED') kspare(39) = 1
            else if (word(i)(1:4) .eq. 'QGEN') then
               if (word(i+1) .eq. 'FIXED') kspare(40) = 1
            else
               last = lastch (word(i))
               write (errbuf(1),105 ) word(i)(1:last)
  105          format('0 Undefined keyword (',a,')')
               write (errbuf(2), 140) buf(1:80)
               call prterx ('W',2)
            endif
  110    continue
         ksw = 1
  120    read (inp,130 ,end=440 ) buf
  130    format (a)
         card = buf(1:1)
         if (card .ne. 'B') then
            if (idat .eq. 0) then
               write (errbuf(1), 200 )
  200          format('0 No "B" records follow "BUS_SENSITIVITIES" ')
               call prterx ('W',1)
            endif
            return
         endif
         write (outbuf,140 ) buf(1:80)
  140    format (' BUS_SENSITIVITY text: (',a,')')
         call prtout(1)
         if (ksw .ne. 1) go to 120
         idat = idat + 1
C
         if (idat .gt. 100) then
            write (errbuf(1),150 )
  150       format('0 More than 100 "BUS_SENSITIVITY" buses. ',
     &             'Remaining records are ignored. ')
            call prterx ('W',1)
            idat = 100
            ksw = 2
         else
            read (buf,160 ,err=170 ) namdat(idat), bsedat(idat),
     1         (data(i,idat),i=1,6)
  160       format(bz, 6x, a8, f4.0, 2x, 2f5.0, 2f4.0, 4x, 2f5.0)
            zondat(idat) = buf(1:2)
         endif
         go to 120
C
  170    write (errbuf(1),180 )
  180    format('0 Illegal character data in field.')
         errbuf(2) = ' '
         write (errbuf(3),182) buf(1:80)
  182    format (' (',a80,')')
         call prterx ('W',3)
         if (idat .eq. 0) then
            write (errbuf(1), 200 )
            call prterx ('W',1)
         endif
         return
 
      else if (word(1)(1:6) .eq. 'LINESE') then
C
C        /LINE_SENSITIVITY,LTC=ON,AI_CONTROL=CON
C                              OFF,          OFF
C                                            MON
         kspare(19) = 2
         kspare(20) = 1
         kspare(39) = 0
         kspare(40) = 0
         do 210 i = 2, nwrd - 1, 2
            if (word(i)(1:3) .eq. 'LTC') then
               if (word(i+1) .eq. 'OFF') kspare(19) = 0
               if (word(i+1) .eq. 'ONNVL') kspare(19) = 1
            else if (word(i)(1:2) .eq. 'AI') then
               if (word(i+1) .eq. 'OFF') kspare(20) = 0
               if (word(i+1) .eq. 'MON') kspare(20) = 2
            endif
  210    continue
         ksw = 1
  220    read (inp,130 ,end=440 ) buf
         card = buf(1:1)
         if (card .ne. '>' .and.
     &       card .ne. 'L' .and.
     1       card .ne. 'E')      goto 280
         write (outbuf,230 ) buf(1:80)
  230    format (' LINE_SENSITIVITY text: (',a,')')
         call prtout(1)
         if (ksw .ne. 1) go to 220
         idat = idat + 2
C
         if (idat .gt. 100) then
            write (errbuf(1),240 )
  240       format('0 More than 100 "LINE_SENSITIVITY" records. ',
     &             'Remaining records are ignored.')
            call prterx ('W',1)
            idat = 100
            ksw = 2
         else
            read (buf,250 ,err=260 ) zondat(idat-1), namdat(idat-1),
     1         bsedat(idat-1),namdat(idat), bsedat(idat),
     2         zondat(idat),(data(i,idat-1),i=1,4)
  250       format(bz, a2, 4x, a8, f4.0, 1x, a8, f4.0, a2, t39, 4f6.5)
         endif
         go to 220
C
  260    write (errbuf(1),180 )
         errbuf(2) = ' '
         write (errbuf(3),182) buf(1:80)
         call prterx ('W',3)
         go to 280
 
  280    if (idat .eq. 0) then
            write (errbuf(1),290 )
  290       format('0 NO ">", "L", OR "E" RECORDS FOLLOW ',
     &             '"LINE_SENSITIVITIES" ')
            call prterx ('W',1)
         endif
         return
C
      else if (word(1)(1:6) .eq. 'TRANSF') then
C
C        /TRANSFER_SENSITIVITIES
C
         sentyp = ' '
         ksw = 1
  300    read (inp,130 ,end=440 ) buf
         card = buf(1:1)
         if (index ('>LETI',card) .eq. 0) go to 400
         write (outbuf,310 ) buf(1:80)
  310    format (' TRANSFER_SENSITIVITY text: (',a,')')
         call prtout(1)
         if (ksw .ne. 1) go to 300
 
         if (card .eq. '>') then
            if (index (buf,'OUTAGE') .ne. 0) then
               sentyp = 'L'
               go to 300
            else if (index (buf,'OVERLOAD') .ne. 0) then
               sentyp = 'F'
               go to 300
            else if (index (buf,'TRANSFER') .ne. 0) then
               sentyp = 'T'
               go to 300
            else
               write (errbuf(1),318 )
  318          format('0 Non-recognizable ">" type follows',
     &                ' /TRANSFER_SENSITIVIES.')
               errbuf(2) = ' '
               write (errbuf(3),330 ) buf(1:80)
               call prterx ('W',3)
            endif
         else if (index ('LET',card) .ne. 0) then
            if (sentyp .ne. 'F' .and. sentyp .ne. 'L') then
               write (errbuf(1),320 )
  320          format('0 "L", "T", or "E" record not preceded with ',
     &                '">OUTAGE<" or ">OVERLOAD<" record. ')
               errbuf(2) = ' '
               write (errbuf(3),330 ) buf(1:80)
  330          format ('   (',a,')')
               call prterx ('W',3)
            else if (idat .ge. 99) then
               write (errbuf(1),340 )
  340          format('0 More than 100 "TRANSFER_SENSITIVITY" records.',
     &                ' Remainding records are ignored. ')
               call prterx ('W',1)
               idat = 100
               ksw = 2
            else
               idat = idat + 2
               read (buf,350 ,err=380 ) namdat(idat-1),
     1            bsedat(idat-1),namdat(idat), bsedat(idat),
     2            zondat(idat)
  350          format(bz, t7, a8, f4.0, 1x, a8, f4.0, a2)
               zondat(idat-1) = buf(1:1) // sentyp
            endif
         else if (card .eq. 'I') then
            if (sentyp .ne. 'T') then
               write (errbuf(1),360 )
  360          format('0 "I" record is not preceded with ',
     &                '">TRANSFER<" record')
               errbuf(2) = ' '
               write (errbuf(3),330 ) buf(1:80)
               call prterx ('W',3)
            else if (idat .ge. 99) then
               write (errbuf(1),340 )
               call prterx ('W',1)
               idat = 100
               ksw = 2
            else
               idat = idat + 2
               read (buf,370 ,err=380 ) aredat(idat-1), aredat(idat)
  370          format(bz, t4, a10, t15, a10)
               zondat(idat-1) = buf(1:1) // sentyp
            endif
         else
         endif
         go to 300
C
  380    write (errbuf(1),180 )
         errbuf(2) = ' '
         write (errbuf(3),182) buf(1:80)
         call prterx ('W',3)
         go to 300
 
  400    if (idat .eq. 0) then
            write (errbuf(1),410 )
  410       format('0 No "I", "L", "T", or "E" records follow',
     &             ' ">TRANSFER_SENSITIVITIES<" ')
            call prterx ('W',1)
         endif
         return
 
      elseif (word(1)(1:4) .eq. 'LOSS') then
C
C        /LOSS_SENSITIVITY,LTC=ON,   AI_CONTROL=CON, Q_SHUNT=ADJ, -
C                              OFF,             OFF          FIXED
C
C                          QGEN=ADJ, -
C                               FIXED
C
C                          AREAS=<area1,area2,...>,
C
C                          ZONES=<zone1,...>
C
         bigbuf = comprs (buf)
C
C        Check for and concatenate continuation records.
C
  411    last = lastch (bigbuf)
         if (bigbuf(last:last) .eq. '-') then
            read (inp, 130, end=440) buf
            call space (1)
            write (outbuf,422) buf(1:80)
            call prtout (1)
            bigbuf(last:) = comprs(buf)
            go to 411
         endif
         call scan(bigbuf,word,nwrd)
 
C
C        Flag all nodes as sensitivity printout candidates
C
         do 412 i = 1, ntot
  412    ikk(5,i+ntota) = 1
 
         kspare(19) = 2
         kspare(20) = 1
         kspare(39) = 0
         kspare(40) = 0
         numlsz = 0
         numlsa = 0
         do 430 i = 2, nwrd - 1, 2
            if (word(i)(1:3) .eq. 'LTC') then
               if (word(i+1) .eq. 'OFF') kspare(19) = 0
               if (word(i+1) .eq. 'ONNVL') kspare(19) = 1
            else if (word(i)(1:2) .eq. 'AI') then
               if (word(i+1) .eq. 'OFF') kspare(20) = 0
               if (word(i+1) .eq. 'MON') kspare(20) = 2
            else if (word(i)(1:6) .eq. 'QSHUNT') then
               if (word(i+1) .eq. 'FIXED') kspare(39) = 1
            else if (word(i)(1:4) .eq. 'QGEN') then
               if (word(i+1) .eq. 'FIXED') kspare(40) = 1
            else if (word(i)(1:4) .eq. 'ZONE') then
               do 413 kt = 1, ntot
  413          ikk(5,kt+ntota) = 0
               do 416 j = i+1, nwrd
                  numlsz = numlsz + 1
                  lszone(numlsz) = word(j)
                  found = .false.
                  do 414 k = 1, ntot
                     if (zone(k) .eq. word(j)) then
                         kt = inp2opt(k)
                         ikk(5,kt+ntota) = 1
                         found = .true.
                     endif
  414             continue
                  if (.not.found) then
                     last = lastch (word(j))
                     write (errbuf(1), 415) word(j)(1:last)
  415                format('ZONE (',a,') is not in system')
                     call prterx ('W', 1)
                  endif
  416          continue
            else if (word(i)(1:4) .eq. 'AREA') then
               do 417 kt = 1, ntot
  417          ikk(5,kt+ntota) = 0
               do 421 j = i+1, nwrd
                  numlsa = numlsa + 1
                  lsarea(numlsa) = word(j)
                  do 419 k = 1, ntotc
                     if (arcnam(k) .eq. word(j)) then
                         do 418 l = 1, ntot
                            if (jarzn(l) .eq. k) then
                               kt = inp2opt(l)
                               ikk(5,kt+ntota) = 1
                            endif
  418                    continue
                         go to 421
                     endif
  419             continue
                  last = lastch (word(j))
                  write (errbuf(1), 420) word(j)(1:last)
  420             format('Interchange area (',a,') is not in system')
                  call prterx ('W', 1)
  421          continue
            else
               last = lastch (word(i))
               write (errbuf(1),105 ) word(i)(1:last)
               write (errbuf(2),422 ) buf(1:80)
  422          format(' LOSS_SENSITIVITIY text: (',a80,')')
               call prterx ('W',2)
            endif
  430    continue
         read (inp,130 ,end=440 ) buf
         card = buf(1:1)
         write (outbuf,422) buf(1:80)
         call prtout(1)
 
      else
 
         write (errbuf(1),180 )
         errbuf(2) = ' '
         write (errbuf(3),182) buf(1:80)
         call prterx ('W',3)
         if (idat .eq. 0) then
            write (errbuf(1), 200 )
            call prterx ('W',1)
         endif
         return
      endif
 
  440 buf = '( END ) SENDEC'
      card = buf(1:1)
      return
      end

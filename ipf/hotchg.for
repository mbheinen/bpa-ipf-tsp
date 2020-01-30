C    %W% %G%
      subroutine hotchg
 
C     process /CHANGE_BUS_TYPES commands.
 
      include 'ipfinc:parametr.inc'

      include 'ipfinc:alpha.inc'
      include 'ipfinc:arcntl.inc'
      include 'ipfinc:area.inc'
      include 'ipfinc:blank.inc'
      include 'ipfinc:branch.inc'
      include 'ipfinc:bus.inc'
      include 'ipfinc:cbsorc.inc'
      include 'ipfinc:cbus.inc'
      include 'ipfinc:coment.inc'
      include 'ipfinc:ecvar.inc'
      include 'ipfinc:ikk.inc'
      include 'ipfinc:intbus.inc'
      include 'ipfinc:lfiles.inc'
      include 'ipfinc:lndpcp.inc'
      include 'ipfinc:ordsta.inc'
      include 'ipfinc:prt.inc'
      include 'ipfinc:qsdup.inc'
      include 'ipfinc:slnopt.inc'
      include 'ipfinc:snput.inc'
      include 'ipfinc:tbx.inc'
      include 'ipfinc:tran.inc'
      include 'ipfinc:xdata.inc'
      include 'ipfinc:basval.inc'
      include 'ipfinc:miscfile.inc'
 
      common /is_batch / is_batch

      integer find_bus, error, findex, ptr, num_delltcs, offset,
     &        inpold
      character bs_code*1, bus1*8, word(100)*60, capital*132,
     &          bigbuf*512, comprs*512, tag*24, word2(10)*60,
     &          tempfilename*60

      logical found, chgbrn, plist, finished_1, finished_2
      real cv(1), ci(1), cz(1)
      external find_bus

      tbx_loaded = 0
      xdt_flag = .false.
      plist = .true.

c***  Fix for base cases prior to version 4  (lskp is now set in rddtai)

      if ( lskp .ne. 1 .and. basval(8)(1:2) .eq. 'PF') then
         write (errbuf(1), 11)
   11    format(' CHANGE_BUS_TYPES is illegal with vintage PF60xx  ',
     &          'base cases (history files) --')
         write (errbuf(2), 12)
   12    format(' unless the case is resolved with the new IPF/BPF',
     &          ' version.')
         write (errbuf(3), 13)
   13    format(' / CHANGE_BUS_TYPES  command ignored.')
         if (is_batch .eq. 0) then
            call prterx ('E',3)
         else
            call prterx ('F',3)
         endif
         return
      elseif ( lskp .ne. 1) then
         write (errbuf(1), 21)
   21    format(' CHANGE_BUS_TYPES is invalid with a failed solution in
     & the base case history file.')
         write (errbuf(2), 22)
   22    format(' Regenerate the base case to start with a solved base c
     &ase')
         write (errbuf(3), 23)
   23    format(' / CHANGE_BUS_TYPES  command ignored.')
         if (is_batch .eq. 0) then
            call prterx ('E',3)
         else
            call prterx ('F',3)
         endif
         return
      endif

      chgbrn = .false.
      num_delltcs = 0

C     / CHANGE_BUS_TYPES,  BQ=B, LTC=OFF, AREAS=<area_1,...>,
C                          BG=BQ,
C                          BG=B ,
C                          BQ=BF ,
C                          BT=B ,
C                          BX=B ,
C                          BX=BF,
C                                         ZONES=<zone_1,...>
C                                         LIST=ON
C     > EXCLUDE_BUSES
C     B     bus_name bkv
C     B     bus_name bkv
C
C     > LINE_DROP_COMPENSATORS
C     BG    bus_name bkv, ##%
C     BG    bus_name bkv, ##%
C
C     > REACTIVE_COMPENSATION
C     BG    bus_name bkv, ##%, ##
C     BG    bus_name bkv, ##%, ##

      call space (1)
      write (outbuf,90 ) buf(1:80)  
   90 format (' CHANGE_BUS_TYPES text: (',a,')')
      call prtout (1)   
        
      inpold = inp
      buf = capital(buf)
      if (index (buf,'CHANGE_BUS') .ne. 0 .or.  
     1    index (buf,'CHANGEBUS')  .ne. 0) then 
C       
C        Check for and concatenate continuation records.
C       
         bigbuf = comprs (buf)  
  298    last = lastch (bigbuf) 
         if (bigbuf(last:last) .eq. '-') then   
            read (inp, 260, end=261) buf
  260       format (a)  
            call space (1)  
            write (outbuf,90) buf(1:80) 
            call prtout (1) 
            buf = capital(buf)
            bigbuf(last:) = comprs(buf) 
            go to 298   
        
  261       buf = '( END ) HOTCHG'  
            card = buf(1:1) 
         endif  
         call uscan(bigbuf, word, nwrd, '=', ' ,/\\<>()')   
         jwrd = nwrd
C       
C     Initialize IKK array: 
C       
C       (1,*)        (not used) 
C       (2,*) = 0 :  bus is not eligible for type change.   
C               1 :  bus is eligible for type change.   
C               2 :  (generation is dropped, therefore ineligible   
C                     for allocation)   
C       (3,*) = I    (cross index to TBX array) 
C       (4,*) = NC   (forced BG -> BG retention because of line drop
C                     compensation) 
C       (5,*) = J    (LTC index of controlled bus)  
C       
         do nb = 1, ntot 
            ikk(1,nb) = 0   
            ikk(2,nb) = 1   
            ikk(3,nb) = 0   
            ikk(4,nb) = 0   
            ikk(5,nb) = 0   
         enddo
c
c        Load LINE_DROP_COMPENSATION from any prior /CHANGE_BUS_TYPE
c
         do i = 1, numldc  
            nb = lndpcp(1,i)
            ikk(4,nb) = i
         enddo

         do i = 1, ntotb 
            ltyp = tbx(1,i)
            if (ltyp .lt. 10) then  
               nb = tbx(2,i)   
               if (ordtbx .eq. 2) nb = opt2inp(nb)
               ikk(3,nb) = i
            endif   
         enddo
         do i = 1, ntota 
            ltyp = mod (ltran(10,i), 10)
            if (ltyp .eq. 1 .or. ltyp .eq. 2 .or. ltyp .eq. 4) then 
               kc = ltran(2,i)  
               if (kc .eq. -1) then 
                  nb = ltran(1,i)   
               else if (kc .eq. -2) then
                  nb = ltran(9,i)   
               else if (kc .gt. 0) then 
                  nb = kc   
               else 
                  nb = ltran(1,i)   
               endif
C       
C              If NB is already controlled by a different LTC,  
C              flag the opposite terminal as LTC controlled.
C       
               if (ikk(5,nb) .eq. 0) then   
               else if (nb .eq. ltran(1,i)) then
                  nb = ltran(9,i)   
               else 
                  nb = ltran(1,i)   
               endif
               if (ordltc .eq. 2) nb = opt2inp(nb)
               ikk(5,nb) = i
            else
               nb = ltran(1,i)  
C       
C              If NB is already controlled by a different LTC,  
C              flag the opposite terminal as LTC controlled.
C       
               if (ikk(5,nb) .ne. 0) then   
                  nb = ltran(9,i)   
               endif
               if (ordltc .eq. 2) nb = opt2inp(nb)
               ikk(5,nb) = i
            endif   
         enddo
C       
C        Search for FILE = <file_name>
C
         i = 1
         finished_1 = (i .ge. jwrd)
         do while (.not. finished_1)
            if (word(i)(1:4) .eq. 'FILE') then
C
C              Check for "=" separator.
C
               if (word(i+1) .ne. '=') then
                  last = lastch (word(i))
                  write (errbuf(1), 84) word(i)(1:last)
   84             format('Keyword (',a,') in / CHANGE_BUS_TYPES ',
     &                   'text is not followed with an "=" sign.')
                  call prterx ('W', 1)
                  next = i + 1
               else
                  next = i + 2
               endif
               finished_1 = .true.
               tempfilename = word(next)
               inpold = inp
               inp = lunscr1
               ierror = 0
               call opnfil(inp, tempfilename, ierror)
               if (ierror .ne. 0) inp = inpold

               offset = next - i + 1
               do j = next+1, jwrd
                  word(j-offset) = word(j)
               enddo
               jwrd = jwrd - offset
               nwrd = jwrd
            else
               i = i + 1
               finished_1 = (i .ge. jwrd)
            endif
         enddo
C       
C        Search for LIST = ON
C
         i = 1
         finished_1 = (i .ge. jwrd)
         do while (.not. finished_1)
            if (word(i)(1:4) .eq. 'LIST') then
C
C              Check for "=" separator.
C
               if (word(i+1) .ne. '=') then
                  last = lastch (word(i))
                  write (errbuf(1), 84) word(i)(1:last)
                  call prterx ('W', 1)
                  next = i + 1
               else
                  next = i + 2
               endif
               finished_1 = .true.
               if (word(next) .eq. 'OFF') plist = .false.
               offset = next - i + 1
               do j = next+1, jwrd
                  word(j-offset) = word(j)
               enddo
               jwrd = jwrd - offset
               nwrd = jwrd
            else
               i = i + 1
               finished_1 = (i .ge. jwrd)
            endif
         enddo
C       
C        Search for AREAS = <area_1,...>
C
         do i = 1, jwrd
            if (word(i)(1:4) .eq. 'AREA') then
               nwrd = i - 1
C
C              Check for "=" separator.
C
               if (word(i+1) .ne. '=') then
                  last = lastch (word(i))
                  write (errbuf(1), 84) word(i)(1:last)
                  call prterx ('W', 1)
                  next = i + 1
               else
                  next = i + 2
               endif
               do nb = 1, ntot  
                 ikk(2,nb) = 0
               enddo
               do j = next, jwrd
                  do k = 1, ntotc   
                     if (arcnam(k) .eq. word(j)) then   
                        do nb = 1, ntot
                           if (jarzn(nb) .eq. k) then  
                              ikk(2,nb) = 1
                           endif   
                        enddo
                        go to 350  
                     endif  
                  enddo
                  last = lastch (word(j))   
                  write (errbuf(1), 340) word(j)(1:last)
  340             format('Interchange area (',a,') is not in system')
                  call prterx ('W', 1)  

  350             continue 
               enddo
               go to 410

            else if (word(i)(1:4) .eq. 'ZONE') then 
               nwrd = i - 1 
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 84) word(i)(1:last) 
                  call prterx ('W', 1)  
                  next = i + 1  
               else 
                  next = i + 2  
               endif
               do nb = 1, ntot  
                 ikk(2,nb) = 0
               enddo
               do j = next, jwrd
                  found = .false.   
                  do nb = 1, ntot   
                     if (zone(nb) .eq. word(j)) then
                         ikk(2,nb) = 1  
                         found = .true. 
                     endif  
                  enddo
                  if (.not.found) then  
                     last = lastch (word(j))
                     write (errbuf(1), 380) word(j)(1:last) 
  380                format('Zone (',a,') is not in system')
                     call prterx ('W', 1)   
                  endif 
               enddo
               go to 410
            endif   
         enddo
  410    continue   
C       
C        Read next card, check for >EXCLUDE qualifier.  
C       
10250    read (inp, 260, end=10450) buf 
         call space (1) 
         write (outbuf, 90) buf(1:80)   
         call prtout (1)
        
         finished_1 = .false.
         do while (.not. finished_1)
           card = buf(1:1)
        
           if (card .eq. '.') then
              read (inp, 260, end=10450) buf   
              write (outbuf, 90) buf(1:80)   
              call prtout (1)
           else if (card .eq. '>') then   
        
             if (findex(buf(2:10),'EXCLUDE') .ne. 0) then
C       
C              > EXCLUDE_BUSSES <   
C       
               finished_2 = .false.
               do while (.not. finished_2)
                 read (inp, 260, end=10450) buf   
                 card = buf(1:1)  
        
                 call space (1)   
                 write (outbuf, 90) buf(1:80) 
                 call prtout (1)  
         
                 if (card .eq. '.') then  
                 else if (card .eq. 'B') then 
                   read (buf, 10280) bus1, base1 
10280              format (bz, t7, a8, f4.0)   
                   nb = find_bus (bus1, base1) 
                   if (nb .le. 0) then   
                     write (errbuf(1),10290) bus1, base1
10290                format ('EXCLUDE_BUS (', a8, f6.1,
     &                       ') is not in system.')
                     call prterx ('W', 1)
                   else
                     ikk(2,nb) = 0
                   endif
                 else
                   finished_2 = .true.
                 endif
               enddo
        
             else if (findex(buf(2:10),'LINE') .ne. 0 .or. 
     &                findex(buf(2:10),'REACTIVE') .ne. 0) then  
C       
C              > LINE_DROP_COMPENSATORS 
C              > REACTIVE_COMPENSATION 
C       
               call uscan(buf(2:), word2, nwrd2, '=', ' ,')   
               tag = word2(1)
               last = lastch (tag)

               finished_2 = .false.
               do while (.not. finished_2)

                 read (inp, 260, end=10450) buf   
                 card = buf(1:1)  
        
                 call space (1)   
                 write (outbuf, 90) buf(1:80) 
                 call prtout (1)  
        
                 if (card .eq. '.') then  
                 else if (card .eq. 'B') then 
                   read (buf, 10294) bus1, base1 
10294              format (bz, t7, a8, f4.0)   
                   nb = find_bus (bus1, base1) 
                   error = 0 
                   if (nb .le. 0) then   
                     write (errbuf(1), 10296) tag(1:last), bus1, base1
10296                format (a, ' bus (', a8, f6.1,  
     1                       ') is not in system.')   
                     call prterx ('W', 1)   
                     error = 1  
                   else if (kbsdta(1,nb) .eq. 8) then
                     ikk(4,nb) = numldc + 1 
                     mb = kbsdta(13,nb) 
                     if (mb .eq. 0 .or. mb .eq. nb) then
                       ptr = kbsdta(16,nb)
                       mb = ky(ptr) 
                     else   
                       ptr = kbsdta(16,nb) 
                       found = .false.
                       do while (ptr .gt. 0 .and. .not. found)
                         if (ky(ptr) .eq. mb) then
                            found = .true.
                         else
                            ptr = brnch_nxt(ptr)
                         endif
                       enddo
                       if (.not. found) then
                         write (errbuf(1), 10300) tag(1:last), bus1, 
     &                     base1, bus(mb), base(mb) 
10300                    format (a, ' bus (', a8, f6.1,   
     1                      ') is controlling a remote bus (', a8, 
     2                      f6.1, ')') 
                         call prterx ('W', 1)
                         error = 1   
                       endif  
                     endif
                   else  
                     call typno (bs_code, kbsdta(1,nb))
                     write (errbuf(1), 10304) tag(1:last), bus1, 
     &                 base1, 'B'//bs_code
10304                format (a, ' (', a8, f6.1,  
     1                 ') is illegal type "', a, '".')  
                     call prterx ('W', 1)   
                     error = 1  
                   endif 
                   if (error .eq. 0) then
                     call uscan(buf(20:), word2, nwrd2, '=', ' ,%')   
                     pct = ftn_atof (word2(1))
                     if (pct .le. 0.0 .or. pct .gt. 100.0) then 
                       write (errbuf(1), 10310) tag(1:last), bus1, 
     &                   base1, pct   
10310                  format (a, ' bus (', a8, f6.1,   
     &                   ') has an unconventional percentage (', f6.1, 

     &                   ')')  
                        call prterx ('W', 1)
                     endif
                     do i = 1, numldc  
                       if (lndpcp(1,i) .eq. nb) then
                         write (errbuf(1), 10312) tag(1:last), bus1, 
     &                      base1  
10312                    format ('Duplicate ', a, ' buses (', 
     &                     a8, f6.1, ') ignored.)')
                         call prterx ('W', 1)
                         go to 10318
                       endif
                     enddo
                     if (numldc .ge. 20) then
                       write (errbuf(1), 10316) 20, tag(1:last), bus1, 

     &                   base1
10316                  format ('More than ', i3, 1x, a, 
     &                   ' records. Bus (', a8, f6.1, ') ignored.)')
                       call prterx ('W', 1)
                     else
                       numldc = numldc + 1  
                       lndpcp(1,numldc) = nb
                       drppct(numldc) = pct / 100.0 
                       kt = inp2opt(nb)
                       vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                       if (tag(1:4) .eq. 'LINE') then
                         lndp_type(numldc) = 1
                         lndpcp(2,numldc) = mb
c
c                        Compute voltage
c
                         mt = inp2opt(mb)
                         vm = dsqrt (e(mt) ** 2 + f(mt) ** 2)
                         vmax_ldc(numldc) = drppct(numldc) * vk
     &                        + (1.0 - drppct(numldc)) * vm
                         vmin_ldc(numldc) = vmax_ldc(numldc)
                         xc_ldc(numldc) = 0.0
                       else
                         lndp_type(numldc) = 2
                         lndpcp(2,numldc) = 0
                         xbase = ftn_atof (word2(2))
                         if (xbase .eq. 0.0) xbase = bmva
                         xc_ldc(numldc) = 0.01 * pct * bmva / xbase
                         cz(1) = xc_ldc(numldc)
                         ci(1) = qnetu(kt) / vk
                         cv(1) = vk - ci(1) * cz(1)
                         vmax_ldc(numldc) = cv(1)
                         vmin_ldc(numldc) = cv(1)
                       endif
                     endif   
                   endif  
                 else
                   finished_2 = .true.
                 endif 
10318            continue
               enddo
        
             else
        
               write (errbuf(1), 10430) buf(1:20)   
10430          format('Unrecognized /CHANGE_BUS_TYPE command (',  
     1                a,').')   
               call prterx ('W', 1) 
        
             endif   
           else   
             finished_1 = .true.
           endif  
         enddo
        
         go to 10453

10450    buf = '( END ) HOTCHG' 
         card = buf(1:1)

10453    if (inp .ne. inpold) then
           inp = inpold
           read (inp, 260, end=10451) buf 
           go to 10452
10451      buf = '( END ) HOTCHG'
10452      card = buf(1:1)
         endif
C       
C        Write header   
C       
         call forbtm
         write (outbuf, 10454)  
10454    format (t53, ' Summary of /CHANGE_BUS_TYPES Conversion ')  
         call shdlod(1) 
         write (outbuf, 411)
  411    format ('0BUS', t18, 'LTC disabled?', t34, 'Zone', 
     1      t40, 'Bus Type', t50, '-- Shunt (MVAR) --', 
     2      t72, '-- Generation (MVAR) --', 
     3      t98, '-- Voltage Original    Final --') 
         call shdlod(2) 
         write (outbuf, 412)
  412    format (t40, 'old new', t48, '   Orig Final Removed',  
     1      t72, ' Orig  Min  Max  Removed',
     2      t98, ' Vact   Vmin  Vmax  Vmin  Vmax')  
         call shdlod(3) 
         outbuf = ' '   
         call shdlod(4) 
         call shdlod(5) 
         call fortop
C       
C        First pass.  Convert type BG LINE_DROP_COMPENSATOR and type
C        BG REACTIVE_COMPSENSATION generators to type BG, controlling 
C        themselves.   
C       
         do i = 1, numldc 
            nb = lndpcp(1,i)
            if (ikk(4,nb) .ne. 0) then  
               kt = inp2opt(nb)   
               jtbx = ikk(3,nb) 
               jltc = ikk(5,nb) 
               kbsdta(13,nb) = nb   
               if (ordtbx .eq. 1) then  
                  tbx(8,jtbx) = nb 
               else 
                  tbx(8,jtbx) = kt 
               endif
C       
C              Reset type conversion flag to prevent duplicate  
C              type change. 
C       
               ikk(2,nb) = 0
            endif   
         enddo
        
         do i = 2, nwrd, 3  
            if (word(i) .eq. 'BQ') then 
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 84) word(i)(1:last) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
               endif
C       
C              BQ --> B* unconditionally.   
C              BQ --> B if PGEN, QGEN, or QGEN_limits = 0.  
C              BQ --> BQ otherwise. 
C       
               if (word(i+2) .eq. 'B ' .or. word(i+2) .eq. 'B*') then
                  do nb = 1, ntot   
                     if (ikk(2,nb) .eq. 1 .and. kbsdta(1,nb) .eq. 7)
     1                   then   
                        kt = inp2opt(nb)  
                        jtbx = ikk(3,nb)
                        jltc = ikk(5,nb)
                        qmach = busdta(9,nb) - busdta(10,nb)
                        if (abs (qmach) .le. 0.5 .or.   
     1                      word(i+2) .eq. 'B*') then   
                           call chgbty (nb, 1, jtbx, jltc, plist)  
                        else
                           call chgbty (nb, 7, jtbx, jltc, plist)  
                        endif   
                     endif  
                  enddo
        
               else if (word(i+2) .eq. 'BF' .or. word(i+2) .eq. 'BF*')
     1            then  
C       
C                 BQ --> BF* unconditionally.   
C                 BQ --> BF if PGEN, QGEN, or QGEN_limits = 0.  
C                 BQ --> BQ otherwise.  
C       
                  do nb = 1, ntot   
                     if (ikk(2,nb) .eq. 1 .and. kbsdta(1,nb) .eq. 7)
     1                   then   
                        kt = inp2opt(nb)  
                        jtbx = ikk(3,nb)
                        jltc = ikk(5,nb)
                        vksq = e(kt) ** 2 + f(kt) ** 2  
                        qmach = busdta(9,nb) - busdta(10,nb)
                        if (abs (qmach) .le. 0.5 .or.   
     1                      word(i+2) .eq. 'BF*') then  
                           call chgbty (nb, 1, jtbx, jltc, plist)  
                        else
                           call chgbty (nb, 7, jtbx, jltc, plist)  
                        endif   
                     endif  
                  enddo
               else 
                  write (errbuf(1), 420) word(i), word(i+2) 
  420             format('Illegal bus type conversion (', a2,') > (',
     1              a2,') ignored') 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
                  go to 440 
               endif
        
            else if (word(i) .eq. 'BG') then
C       
C              BG --> BQ* unconditionally.  
C              BG --> BQ if PGEN, QGEN, or QGEN_limits = 0. 
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 84) word(i)(1:last) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
               endif
        
               if (word(i+2) .eq. 'BQ' .or. word(i+2) .eq. 'BQ*') then
                  do nb = 1, ntot   
                     if (ikk(2,nb) .eq. 1 .and. kbsdta(1,nb) .eq. 8)
     1                   then   
                        kt = inp2opt(nb)  
                        jtbx = ikk(3,nb)
                        jltc = ikk(5,nb)
                        qmach = busdta(9,nb) - busdta(10,nb)
                        if (abs (qmach) .le. 0.5 .or.   
     1                      word(i+2) .eq. 'BQ*') then  
                           call chgbty (nb, 1, jtbx, jltc, plist)  
                        else
                           call chgbty (nb, 7, jtbx, jltc, plist)  
                        endif   
                     endif  
                  enddo
        
               else if (word(i+2) .eq. 'B ' .or. word(i+2) .eq. 'B*')
     1            then  
C       
C                 BG --> B* unconditionally.
C                 BG --> B if PGEN, QGEN, or QGEN_limits = 0.   
C       
                  do nb = 1, ntot   
                     if (ikk(2,nb) .eq. 1 .and. kbsdta(1,nb) .eq. 1)
     1                   then   
                        kt = inp2opt(nb)  
                        jtbx = ikk(3,nb)
                        jltc = ikk(5,nb)
                        qmach = busdta(9,nb) - busdta(10,nb)
                        if (abs (qmach) .le. 0.5 .or.   
     1                      word(i+2) .eq. 'BQ*') then  
                           call chgbty (nb, 1, jtbx, jltc, plist)  
                        endif   
                     endif  
                  enddo
        
               else if (word(i+2) .eq. 'BF' .or. word(i+2) .eq. 'BF*')
     1            then  
C       
C                 BG --> BF* unconditionally.   
C                 BG --> BF if PGEN, QGEN, or QGEN_limits = 0.  
C       
                  do nb = 1, ntot   
                     if (ikk(2,nb) .eq. 1 .and. kbsdta(1,nb) .eq. 7)
     1                   then   
                        kt = inp2opt(nb)  
                        jtbx = ikk(3,nb)
                        jltc = ikk(5,nb)
                        qmach = busdta(9,nb) - busdta(10,nb)
                        if (abs (qmach) .le. 0.5 .or.   
     1                      word(i+2) .eq. 'BQ*') then  
                           call chgbty (nb, 13, jtbx, jltc, plist) 
                        endif   
                     endif  
                  enddo
        
               else 
                  write (errbuf(1), 420) word(i), word(i+2) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
                  go to 440 
               endif
        
            else if (word(i) .eq. 'BT') then
C       
C              BT --> B 
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 84) word(i)(1:last) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
               endif
        
               if (word(i+2) .eq. 'B ') then
C       
C                 Set flag to compress BRNCH array. 
C       
                  do nb = 1, ntot   
                     if (ikk(2,nb) .eq. 1 .and. kbsdta(1,nb) .eq. 10)
     1                   then   
                        jtbx = ikk(3,nb)
                        jltc = ikk(5,nb)
                        if (jltc .eq. 0) then   
                           write (errbuf(1), 10425) bus(nb), base(nb),
     1                        word(i), word(i+2)
10425                      format(' Bus ', a8, f6.1,
     &                           ' type is changed from (', a2,') to (',
     &                             a2, ') but has no LTC control.')
                           call prterx ('W', 1)
                        else
                           chgbrn = .true.  
                           call chgbty (nb, -1, jtbx, jltc, plist)  
                        endif   
                     endif  
                  enddo
               else 
                  write (errbuf(1), 420) word(i), word(i+2) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
                  go to 440 
               endif
        
            else if (word(i) .eq. 'BX') then
C       
C              BX --> B 
C              BX --> B*
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 84) word(i)(1:last) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
               endif
        
               if (word(i+2) .eq. 'B ' .or. word(i+2) .eq. 'B*') then
                  do nb = 1, ntot   
                     if (ikk(2,nb) .eq. 1 .and. kbsdta(1,nb) .eq. 11)
     1                   then   
                        jtbx = ikk(3,nb)
                        jltc = ikk(5,nb)
                        call chgbty (nb, 1, jtbx, jltc, plist) 
                     endif  
                  enddo
               else if (word(i+2) .eq. 'BF' .or. word(i+2) .eq. 'BF*')
     1            then  
C       
C              BX --> BF
C              BX --> BF*   
C       
                  do nb = 1, ntot   
                     if (ikk(2,nb) .eq. 1 .and. kbsdta(1,nb) .eq. 11)
     1                   then   
                        jtbx = ikk(3,nb)
                        jltc = ikk(5,nb)
                        call chgbty (nb, 13, jtbx, jltc, plist)
                     endif  
                  enddo
               else 
                  write (errbuf(1), 420) word(i), word(i+2) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
                  go to 440 
               endif
        
            else if (word(i) .eq. 'LTC') then   
C       
C              LTC --> OFF  
C       
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 84) word(i)(1:last) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
               endif
        
               if (word(i+2) .eq. 'OFF') then   
C       
C                 Set flag to compress BRNCH array. 
C       
                  do nb = 1, ntot 
                     if (ikk(2,nb) .eq. 1 .and. ikk(5,nb) .gt. 0) then
                        jtbx = ikk(3,nb)
                        ktyp = kbsdta(1,nb) 
                        jltc = ikk(5,nb)
C       
C                       Exclude d-c commutating LTC's.  
C                       Changing a bus type to itself is a magic
C                       code to delete a connected LTC. 
C       
                        if (ktyp .ne. 5 .and. ktyp .ne. 12) then
                           chgbrn = .true.  
                           num_delltcs = num_delltcs + 1
                           call chgbty (nb, -ktyp, jtbx, jltc, plist)  

                        endif   
                     endif  
                  enddo
               else 
                  write (errbuf(1), 10427) word(i), word(i+2)   
10427             format('Illegal LTC option (', a3,') > (',
     1              a6,') ignored') 
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
                  go to 440 
               endif
        
            else
        
               write (errbuf(1), 430) word(i), word(i+2)
  430          format('Unrecognized bus type conversion (', a2,') > (',
     1              a2,') ignored') 
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
               go to 440
            endif   

  440       continue   
         enddo
        
      else  
        
         write (errbuf(1),180 ) 
  180    format('Illegal / CHANGE_BUS_TYPES command.')  
         errbuf(2) = ' '
         write (errbuf(3),182) buf(1:80)
  182    format (' (',a80,')')  
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         return 
        
      endif 

      if (num_delltcs .gt. 0) then
         write (outbuf, 736) num_delltcs/2
  736    format ('0 / CHANGE_BUS_TYPES has deleted ', i3,
     1           ' LTC''s from system') 
      endif
      outbuf = ' '  
      call shdlod (1)   
      call shdlod (2)   
      call shdlod (3)   
      call shdlod (4)   
      call shdlod (5)   
        
      outbuf = '0End of /CHANGE_BUS_TYPE'   
      call prtout(1)
      call forbtm   
        
      return
      end   

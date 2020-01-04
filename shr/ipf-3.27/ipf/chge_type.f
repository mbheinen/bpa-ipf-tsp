C    @(#)chge_type.f	20.5 5/27/98
        subroutine chge_type (bigbuf)
        character bigbuf *(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/ordsta.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/tbx.inc'
        include 'ipfinc/tran.inc'

        common /is_batch / is_batch
        common /scratch/ ikk(5,MAXBUS)
  
        integer findex, first, ptr, offset
        character word(100) * 30
	logical found, chgbrn, finished, plist
C       
C        Initialize IKK array: 
C       
C        (1,*)        (not used) 
C        (2,*) = 0 :  bus is not eligible for type change.   
C                1 :  bus is eligible for type change.   
C                     for allocation)   
C        (3,*) = I    (cross index to TBX array) 
C        (4,*)        (not used)
C        (5,*) = J    (LTC index of controlled bus)  
C       
        plist = .true.
        do nb = 1, ntot 
           ikk(1,nb) = 0   
           ikk(2,nb) = 1   
           ikk(3,nb) = 0   
           ikk(4,nb) = 0   
           ikk(5,nb) = 0   
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
C             If NB is already controlled by a different LTC,  
C             flag the opposite terminal as LTC controlled.
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
C             If NB is already controlled by a different LTC,  
C             flag the opposite terminal as LTC controlled.
C       
              if (ikk(5,nb) .ne. 0) then   
                 nb = ltran(9,i)   
              endif
              if (ordltc .eq. 2) nb = opt2inp(nb)
              ikk(5,nb) = i
           endif   
        enddo

        call uscan (bigbuf, word, nwrd, '=',' ,/\\<>()')   
C       
C       Search for LIST = ON
C
        i = 1
        finished = (i .ge. nwrd)
        do while (.not. finished)
           if (word(i)(1:4) .eq. 'LIST') then
c
C             Check for "=" separator.
C
              if (word(i+1) .ne. '=') then
                 last = lastch (word(i))
                 write (errbuf(1), 84) word(i)(1:last)
   84            format('Keyword (',a,') in / CHANGE_BUS_TYPES ',
     &                  'text is not followed with an "=" sign.')
                 call prterx ('W', 1)
                 next = i + 1
              else
                 next = i + 2
              endif
              finished = .true.
              if (word(next) .eq. 'OFF') plist = .false.
              offset = next - i + 1
              do j = next+1, jwrd
                 word(j-offset) = word(j)
              enddo
              nwrd = nwrd - offset
           else
              i = i + 1
              finished = (i .ge. nwrd)
           endif
        enddo
C       
C       Search for AREAS = <area_1,...>
C
        i = 1
        jwrd = nwrd
        finished = .false.
        do while (i .le. jwrd .and. .not. finished)
           if (word(i)(1:4) .eq. 'AREA') then
              nwrd = i - 1
C
C             Check for "=" separator.
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
  340            format('Interchange area (',a,') is not in system')   
                 call prterx ('W', 1)  
  350            continue 
              enddo
              finished = .true.
           else if (word(i)(1:4) .eq. 'ZONE') then 
              nwrd = i - 1 
C       
C             Check for "=" separator. 
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
  380               format('Zone (',a,') is not in system')
                    call prterx ('W', 1)   
                 endif 
              enddo
              finished = .true.
           endif   
           i = i + 1
        enddo
  410   continue   

        chgbrn = .false.

C       / CHANGE_BUS_TYPES,  BQ=B, LTC=OFF, AREAS=<area_1,...>,
C                            BG=BQ,
C                            BG=BE,
C                            BG=B ,
C                            BQ=BF,
C                            BQ=BE,
C                            BT=BE,
C                            BT=B ,
C                            BX=B ,
C                            BX=BF,
C                                         ZONES=<zone_1,...>
C       
C       Write header   
C       
10452   call forbtm
        write (outbuf, 10454)  
10454   format (t53, ' Summary of /CHANGE_BUS_TYPES Conversion ')  
        call shdlod(1) 
        write (outbuf, 411)
  411   format ('0BUS', t18, 'LTC disabled?', t34, 'Zone', 
     1      t40, 'Bus Type', t50, '-- Shunt (MVAR) --', 
     2      t72, '-- Generation (MVAR) --', 
     3      t98, '-- Voltage Original    Final --') 
        call shdlod(2) 
        write (outbuf, 412)
  412   format (t40, 'old new', t48, '   Orig Final Removed',  
     1      t72, ' Orig  Min  Max  Removed',
     2      t98, ' Vact   Vmin  Vmax  Vmin  Vmax')  
        call shdlod(3) 
        outbuf = ' '   
        call shdlod(4) 
        call shdlod(5) 
        call fortop

        do 440 i = 2, nwrd, 3  
            if (word(i) .eq. 'BQ') then 
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 84) word(i)(1:last) 
                  if (is_batch .eq. 0) then
                     call prterx ('W',1)
                  else
                     call prterx ('F',1)
                  endif
               endif
C       
C              BQ --> BE unconditionally.   
C              BQ --> B* unconditionally.   
C              BQ --> B if PGEN, QGEN, or QGEN_limits = 0.  
C              BQ --> BQ otherwise. 
C       
               if (word(i+2) .eq. 'B ' .or. word(i+2) .eq. 'B*' .or.
     &             word(i+2) .eq. 'BE') then   
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
                        else if (word(i+2) .eq. 'BQ') then
                           call chgbty (nb, 7, jtbx, jltc, plist)  
                        else 
                           call chgbty (nb, 2, jtbx, jltc, plist)  
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
                     call prterx ('W',1)
                  else
                     call prterx ('F',1)
                  endif
                  go to 440 
               endif
        
            else if (word(i) .eq. 'BG') then
C       
C              BG --> BQ* unconditionally.  
C              BG --> BE 
C              BG --> BQ if PGEN, QGEN, or QGEN_limits = 0. 
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 84) word(i)(1:last) 
                  if (is_batch .eq. 0) then
                     call prterx ('W',1)
                  else
                     call prterx ('F',1)
                  endif
               endif
        
               if (word(i+2) .eq. 'BQ' .or. word(i+2) .eq. 'BQ*' .or.
     &             word(i+2) .eq. 'BE') then  
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
                        else if (word(i+2) .eq. 'BQ') then
                           call chgbty (nb, 7, jtbx, jltc, plist)  
                        else 
                           call chgbty (nb, 2, jtbx, jltc, plist)  
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
                     call prterx ('W',1)
                  else
                     call prterx ('F',1)
                  endif
                  go to 440 
               endif
        
            else if ((word(i) .eq. 'BT' .or. word(i) .eq. 'BC' .or.
     &                word(i) .eq. 'B ') .and. word(i+2) .eq. 'BE') then
C       
C              B  --> BE unconditionally.   
C              BC --> BE unconditionally.   
C              BT --> BE unconditionally.   
C       
               do nb = 1, ntot   
                  if (ikk(2,nb) .eq. 1 .and. 
     &                (kbsdta(1,nb) .eq. 1 .or.
     &                 kbsdta(1,nb) .eq. 4 .or.
     &                 kbsdta(1,nb) .eq. 10)) then
                     kt = inp2opt(nb)  
                     jtbx = ikk(3,nb)
                     jltc = ikk(5,nb)
                     call chgbty (nb, 2, jtbx, jltc, plist) 
                  endif  
               enddo
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
                     call prterx ('W',1)
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
                           call chgbty (nb, 1, jtbx, jltc, plist)  
                        endif   
                     endif  
                  enddo
               else 
                  write (errbuf(1), 420) word(i), word(i+2) 
                  if (is_batch .eq. 0) then
                     call prterx ('W',1)
                  else
                     call prterx ('F',1)
                  endif
                  go to 440 
               endif
        
            else if (word(i) .eq. 'BX') then
C       
C              BX --> B 
C              BX --> B*
C              BX --> BE
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 84) word(i)(1:last) 
                  if (is_batch .eq. 0) then
                     call prterx ('W',1)
                  else
                     call prterx ('F',1)
                  endif
               endif
        
               if (word(i+2) .eq. 'B ' .or. word(i+2) .eq. 'B*' .or.
     &             word(i+2) .eq. 'BE') then   
                  do nb = 1, ntot   
                     if (ikk(2,nb) .eq. 1 .and. kbsdta(1,nb) .eq. 11)   
     1                   then   
                        jtbx = ikk(3,nb)
                        jltc = ikk(5,nb)
                        if (word(i+2) .eq. 'B ' .or. 
     &                      word(i+2) .eq. 'B*') then
                           call chgbty (nb, 1, jtbx, jltc, plist) 
                        else
                           call chgbty (nb, 2, jtbx, jltc, plist) 
                        endif
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
                     call prterx ('W',1)
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
                     call prterx ('W',1)
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
                           call chgbty (nb, -ktyp, jtbx, jltc, plist)  
                        endif   
                     endif  
                  enddo
               else 
                  write (errbuf(1), 10427) word(i), word(i+2)   
10427             format('Illegal LTC option (', a3,') > (',
     1              a6,') ignored') 
                  if (is_batch .eq. 0) then
                     call prterx ('W',1)
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
                  call prterx ('W',1)
               else
                  call prterx ('F',1)
               endif
               go to 450      
            endif   
  440   continue   
  450   continue        
        
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

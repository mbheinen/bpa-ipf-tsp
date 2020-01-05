C    %W% %G%
C****************************************************************
C
C       File: p_gendrp.f
C       Purpose: IPF shell program to process
C                                                                      
c         / GEN_DROP
C         C ...
C
C       Author: Walt Powell  Date: 19 Dec 1997
C                            Modified: 19 Dec 1997
C       Called by: 
C
C****************************************************************
C
	integer function p_gendrp (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/gendrp.inc'
        include 'ipfinc/ikk.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/optim1.inc'
        include 'ipfinc/ordsta.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/tbx.inc'
        include 'ipfinc/tran.inc'

        common /bldtbl/ bldtbl, fltstr
        logical bldtbl
        integer fltstr, reordr, status

        common /is_batch / is_batch       ! 0 = interactive, 1 = batch

        integer chkerr
        character  zonex(100)*2

        character bus1*8, word(100)*30, capital*30, null*1, 
     &            linefeed*1, text*120
        integer first, last, apdoutbuf, o2, error, find_bus,
     &          findstr, ftn_atoi

        logical opened, finished, found, chgbrn
        external find_bus, kmpgd1, swpgd1, kmpgd2, swpgd2

C       LUN is logical unit number of auxiliary output.             
C       NUMREC is count of records written to auxiliary output file.
C                                                                      
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        p_gendrp = 0        ! default return SUCCESS state
        numerr = 0          ! reinitialize error count
        error = 0

        numdrp = 0
        numgen = 0
        itdpmx = 0
        dropmw = 0.0  
        drptot = 0.0d0
        drptol = 10.0d0
        error = 0 
        chgbrn = .false.  
        o2 = 1
        text = ' '
c
c       Gen_drop requires ostates >= 3
c	
        if (ostates .lt. 2) then
           write (errbuf(1), 140)
  140      format(' No base data in residence')
           call prterx ('W', 1)
           goto 900
        else if (ostates .lt. 3) then
           if (numchg .gt. 0 .or. kspare(1) .eq. 1) then
              if (fltstr .eq. -1) then
                 if (nadd .gt. 0 .or. ndel .gt. 0) then
                    fltstr = 1
                 else
                    fltstr = 0
                 endif
              endif
           else
              fltstr = 0
           endif
           call btable(fltstr, .false.)
           call prtime('BUILD_TABLE')
           call osorto
C
C          Process any EPRI_1964 data.
C
           if (nepbus + nepctl .gt. 0) then
              call lodepr(zonex, numznx)
              call prtime('BUILD_EPRI_1964_TABLES')
           else
              numznx = 0
           endif
C
C          Zones declared in d-c are trasnferred to BLDTIE.
C
           call bldtie(zonex, numznx)
           call prtime('BUILD_AREA_ZONE_TABLES')
           if ( is_batch .eq. 0 ) then
              if (chkerr('E') .eq. 0) ostates = 3
           else
              if (chkerr('F') .eq. 0) ostates = 3
           endif
        endif
        if (ostates .eq. 3) then
           status = reordr()
           call prtime('re_order')
           if (status .ne. 0) go to 900
           call chgorder('inp2opt')             !  EXT2IN
           call prtime('chg_order : inp_to_opt')
           if ( is_batch .eq. 0 ) then
              if (chkerr('E') .eq. 0) ostates = 4
           else
              if (chkerr('F') .eq. 0) ostates = 4
           endif
        else if (ostates .gt. 4) then
           call chgorder('inp2opt')             !  EXT2IN
           call prtime('chg_order : inp_to_opt')
           if ( is_batch .eq. 0 ) then
              if (chkerr('E') .eq. 0) ostates = 4
           else
              if (chkerr('F') .eq. 0) ostates = 4
           endif
        endif
C       
C       Parse /GEN_DROP command:
C
C       /GEN_DROP, AI_CONTROL=CON, MAX_ITER=<nn>, TOL=<nn>,
C                             OFF  
C                             MON  
C       
C                  INITIAL_DROP=<nn>, AREAS=<area1,area2,...>, 
C       
C                  ZONES=<zone1,...>   
C       
C                           Initialize IKK array: 
C     
C              (1,*) = 0 :  (generation is not eligible for allocation)   
C                      1 :  (generation is allocatable)   
C                      2 :  (generation is dropped, therefore ineligible  
C                           for allocation)   
C              (2,*) = 0/1  (bus type is not/is eligible for change)  
C              (3,*) = I    (cross index to TBX array)
C              (4,*) = NTYP (bus type)
C              (5,*) = J    (LTC index of controlled bus) 

        do nb = 1, ntot
          ikk(1,nb) = 1  
          ikk(2,nb) = 1  
          ikk(3,nb) = 0  
          ikk(4,nb) = kbsdta(1,nb)   
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
            if (ordltc .eq. 2) nb = opt2inp(nb)   
            ikk(5,nb) = i   
          else   
            nb = ltran(1,i) 
            if (ordltc .eq. 2) nb = opt2inp(nb)   
            ikk(5,nb) = i   
          endif  
        enddo
C       
C       Check for and "concatenate" continuation records.
C       
c
c       Position in_buffer to next record
c
        first = 1
        last = index (in_buffer, null)
        if (last .eq. 0) last = len (in_buffer)

        next = nxt_term(in_buffer(first+1:)) + first
        text = in_buffer(first:next-1)
        write (outbuf, 10000) text(1:80)   
10000   format (' GEN_DROP text (', a, ')')  
        call prtout (1) 

        finished = .false.
        do while (.not. finished)
          last = lastch (in_buffer(first:next)) + first - 1
          if (in_buffer(last:last) .eq. '-') then   
            do while (last .lt. next)
              in_buffer(last:last) = ' '
              last = last + 1
            enddo
            next = nxt_term(in_buffer(next+1:)) + next
            text = in_buffer(last:next-1)
            write (outbuf, 10000) text(1:80)   
            call prtout (1) 
          else
            finished = .true.
          endif  
        enddo

        call uscan(in_buffer(first:next-1), word, nwrd, '=',
     &             ' ,()<>/\\')   
        do i = 1, nwrd 
          word(i) = capital (word(i)) 
        enddo
        do i = 2, nwrd - 1, 3  
          if (word(i)(1:2) .eq. 'AI') then
C       
C           Check for "=" separator. 
C       
            if (word(i+1) .ne. '=') then 
              last = lastch (word(i))   
              write (errbuf(1), 10010) word(i)(1:last)
10010         format('Keyword (', a, ') in / GEN_DROP ',
     &               'text is not followed with an "=" sign.')
              call prterx ('W', 1)
            endif
            if (word(i+2) .eq. 'OFF') then   
              iopton(17) = 0
            else if (word(i+2) .eq. 'MON') then  
              iopton(17) = 2
            else 
              iopton(17) = 1
            endif
          else if (word(i)(1:3) .eq. 'TOL') then  
C       
C           Check for "=" separator. 
C       
            if (word(i+1) .ne. '=') then 
              last = lastch (word(i))   
              write (errbuf(1), 10010) word(i)(1:last)
              call prterx ('W', 1)  
            endif
            drptol = ftn_atof (word(i+2))
          else if (word(i)(1:3) .eq. 'INI') then  
C       
C           Check for "=" separator. 
C       
            if (word(i+1) .ne. '=') then 
              last = lastch (word(i))   
              write (errbuf(1), 10010) word(i)(1:last)
              call prterx ('W', 1)  
            endif
            drptot = ftn_atof (word(i+2))
            dropmw = drptot  
          else if (word(i)(1:3) .eq. 'MAX') then  
C       
C           Check for "=" separator. 
C       
            if (word(i+1) .ne. '=') then 
              last = lastch (word(i))   
              write (errbuf(1), 10010) word(i)(1:last)
              call prterx ('W', 1)  
            endif
            itdpmx = ftn_atoi (word(i+2))
          else if (word(i)(1:4) .eq. 'AREA') then 
C       
C           Check for "=" separator. 
C       
            if (word(i+1) .ne. '=') then 
              last = lastch (word(i))   
              write (errbuf(1), 10010) word(i)(1:last)
              call prterx ('W', 1)  
            endif
            do nb = 1, ntot  
              ikk(1,nb) = 0
            enddo
            do j = i+2, nwrd 
              found = .false.
              k = 1
              do while (k .le. ntotc .and. .not. found)
                if (arcnam(k) .eq. word(j)) then   
                  do nb = 1, ntot
                    if (jarzn(nb) .eq. k) ikk(1,nb) = 1
                  enddo
                  found = .true.
                else
                  k = k + 1
                endif  
              enddo
              if (.not. found) then
                last = lastch (word(j))   
                write (errbuf(1), 10020) word(j)(1:last)   
10020           format('Interchange area (', a, ') is not in system')   
                call prterx ('W', 1)  
              endif
            enddo
          else if (word(i)(1:4) .eq. 'ZONE') then 
C       
C           Check for "=" separator. 
C       
            if (word(i+1) .ne. '=') then 
              last = lastch (word(i))   
              write (errbuf(1), 10010) word(i)(1:last)
              call prterx ('W', 1)  
            endif
        
            do nb = 1, ntot  
              ikk(1,nb) = 0
            enddo
            do j = i+2, nwrd 
              found = .false.   
              do nb = 1, ntot   
                if (zone(nb) .eq. word(j)) then
                  ikk(1,nb) = 1  
                  found = .true. 
                endif  
              enddo
              if (.not. found) then  
                last = lastch (word(j))
                write (errbuf(1), 10030) word(j)(1:last) 
10030           format('ZONE (', a, ') is not in system')
                call prterx ('W', 1)   
              endif 
            enddo
          else
            last = lastch (word(i))  
            write (errbuf(1), 10040) word(i)(1:last)   
10040       format('0 Undefined keyword (', a, ')')
            write (errbuf(2), 10000) buf(1:80)  
            call prterx ('W',2)  
            error = 1
          endif
        enddo
c
c       First loop - Process GEN_DROP buses
c
        first = next + 1
        next = nxt_term(in_buffer(first+1:)) + first
        if (first .lt. next) then
          text = in_buffer(first:next-1)
          write (outbuf, 10000) text(1:80)   
          call prtout (1) 
        else
          text = '( END )'
        endif
        do while (text(1:1) .eq. '.' .or. text(1:1) .eq. 'B') 
          if (text(1:1) .eq. '.') then
          else 
            read (text, 10050) bus1, base1 
10050       format (bz, t7, a8, f4.0) 
            nb = find_bus (bus1, base1)   
            if (nb .le. 0) then 
              write (errbuf(1), 10060) bus1, base1
10060         format ('GEN_DROP bus (', a8, f6.1, 
     &                ') is not in system.')
              call prterx ('W', 1) 
              error = 1
            else
              call uscan(text(20:), word, nwrd, '=', ' ,()<>/\\')   
              pmax = busdta(7,nb)  
              kt = inp2opt(nb)   
              pgen = (pnetu(kt) + ploadu(kt)) * bmva  
              pmin = 0.0   
              do i = 1, nwrd, 3  
                if (word(i) .eq. 'PMIN') then 
                  pmin = ftn_atof (word(i+2))
                else if (word(i) .eq. 'PMAX') then
                  pmax = ftn_atof (word(i+2))
                  busdta(7,nb) = pmax
                else if (word(i) .eq. 'PGEN') then
                  pgenew = ftn_atof (word(i+2))
                  busdta(8,nb) = busdta(8,nb) - pgen + pgenew
                  pgen = pgenew  
                  pnetu(kt) = dble(pgen / bmva) - ploadu(kt) 
                else if (word(i)(1:3) .eq. 'TOL') then
                  tol = ftn_atof (word(i+2))
                  pmin = pgen - tol
                  pmax = pgen + tol
                endif
              enddo
 
              if (ikk(1,nb) .eq. 0) then
                write (errbuf(1), 10070) bus1, base1
10070           format ('GEN_DROP bus ', a8, f6.1,
     &                  ' does not reside with pickup generators.')
                call prterx ('W', 1)
              endif
              ikk(1,nb) = 2
              if (numdrp .gt. 50) then 
                write (errbuf(1), 10080) 50, bus1, base1   
10080           format ('More than ', i3, 'GEN_DROP "B" records. ',
     &                  'Overflow occurred at (', a8, f6.1, ')')
                call prterx ('E', 1)  
                error = 1 
              else 
                numdrp = numdrp + 1   
                gndpno(numdrp) = nb   
                pgen = pnetu(kt) + ploadu(kt)
                gndpol(numdrp) = dble(pgen * bmva)
                gndpmn(numdrp) = dble(pmin)
                gndpmx(numdrp) = dble(pmax)
C       
C               Flag generator if it is a system or area slack bus.
C       
                found = .false.
                i = 1
                do while (i .le. ntotc .and. .not. found)
                  k = karea(1,i) 
                  if (ordtie .eq. 2) k = opt2inp(k)
                  if (k .eq. nb) then
                    if (iopton(17) .eq. 1) then 
                      gndpty(numdrp) = i   
                    else if (kbsdta(1,nb) .eq. 3) then  
                      gndpty(numdrp) = i   
                    else
                      gndpty(numdrp) = 0   
                    endif   
                    found = .true.
                  else
                    i = i + 1
                  endif  
                enddo
                if (.not. found) then
                  if (kbsdta(1,nb) .eq. 3) then 
                    gndpty(numdrp) = 1 
                  else  
                    gndpty(numdrp) = 0 
                  endif 
                endif
                dropmw = dropmw + pgen * bmva - pmax  
              endif
            endif   
          endif
          first = next + 1
          next = nxt_term(in_buffer(first+1:)) + first
          if (first .lt. next) then
            text = in_buffer(first:next-1)
            write (outbuf, 10000) text(1:80)   
            call prtout (1) 
          else
            text = '( END )'
          endif
        enddo

        if (numdrp .eq. 0 .and. drptot .eq. 0.0) then   
          write (errbuf(1), 10090)  
10090     format('No generation dropped.') 
          call prterx ('E', 1) 
          error = 1
          go to 900
        endif   
c
c       Second loop - Process > EXCLUDE buses
c
        if (first .lt. next .and. text(1:1) .eq. '>' .and.
     &      findstr(text(1:10),'EXCLUDE') .ne. 0) then
          first = next + 1
          next = nxt_term(in_buffer(first+1:)) + first
          if (first .lt. next) then
            text = in_buffer(first:next-1)
            write (outbuf, 10000) text(1:80)   
            call prtout (1) 
          else
            text = '( END )'
          endif
          do while (text(1:1) .eq. '.' .or. text(1:1) .eq. 'B') 
            if (text(1:1) .eq. '.') then
            else 
              read (text, 10050) bus1, base1 
              nb = find_bus (bus1, base1)   
              if (nb .le. 0) then 
                write (errbuf(1), 10100) bus1, base1
10100           format ('EXCLUDE bus (', a8, f6.1, 
     &            ') is not in system.')
                call prterx ('W', 1) 
                error = 1
              else
                found = .false.
                i = 1
                do while (i .le. numdrp .and. .not. found)
                  if (gndpno(i) .eq. nb) then
                    write (errbuf(1), 10110) bus1, base1
10110               format ('EXCLUDE_BUS (', a8, f6.1,
     &                      ') cannot be a dropped generator')
                    call prterx ('W', 1)
                    found = .true.
                  else   
                    i = i + 1
                  endif  
                enddo
                if (.not. found) ikk(1,nb) = 0 
              endif
            endif
            first = next + 1
            next = nxt_term(in_buffer(first+1:)) + first
            if (first .lt. next) then
              text = in_buffer(first:next-1)
              write (outbuf, 10000) text(1:80)   
              call prtout (1) 
            else
              text = '( END )'
            endif
          enddo              
        endif

        if (first .lt. last) then
          write (errbuf(1), 10120) buf(1:80)   
10120     format('Unrecognizable GEN_DROP text (', a, ')')   
          call prterx ('E', 1)   
          error = 1  
        endif 
C       
C       Generate list of allocable generators.
C       
        do nb = 1, ntot   
          kt = inp2opt(nb) 
          if (ikk(1,nb) .eq. 1) then 
            pgen = pnetu(kt) + ploadu(kt)
            if (pgen .gt. 0.005) then   
              pmax = busdta(7,nb)  
              pmin = 0.0   
              if (pgen * bmva - 0.5 .le. pmax) then
                if (pmax / bmva + 0.005 .ge. pgen .and.
     1              pmin / bmva - 0.005 .le. pgen) then
                  if (numgen .gt. MAXGENDRP) then
                    write (errbuf(1), 10130) MAXGENDRP, bus1, base1
10130               format ('More than ', i3, 'reallocatable ',
     &                      'generators. Overflow occurred at (',
     &                       a8, f6.1, ')')
                    call prterx ('E', 1)
                    error = 1   
                  else   
                    numgen = numgen + 1 
                    gennum(numgen) = nb 
                    genpol(numgen) = dble(pgen * bmva)
C       
C                   Flag generator if it is a system or area
C                   slack bus.  
C       
                    do i = 1, ntotc   
                      k = karea(1,i)   
                      if (ordtie .eq. 2) k = opt2inp(k)  
                      if (k .eq. nb) then  
                        if (iopton(17) .eq. 1) then   
                          gentyp(numgen) = i 
                        else if (kbsdta(1,nb) .eq. 3) then
                          gentyp(numgen) = i 
                        else  
                          gentyp(numgen) = 0 
                        endif 
                        go to 110
                      endif
                    enddo
                    if (kbsdta(1,nb) .eq. 3) then   
                      gentyp(numgen) = 1   
                    else
                      gentyp(numgen) = 0   
                    endif   
  110               continue
                  endif
                else
                  write (errbuf(1), 10140) pmin, pmax, bus(nb),
     &              base(nb), pgen * bmva
10140             format ('Limits (', f6.0, ',', f6.0,
     &                    ') should reflect generation on ',
     &                    'allocatable machine ',
     &                     a8, f6.1, ' PGEN = (', f10.1, ')')
                  call prterx ('W', 1)
                  ikk(1,nb) = 0
                endif
              else
              endif
            endif   
          endif  
        enddo
        if ((numdrp .eq. 0 .and. drptot .eq. 0.0) .or. 
     &       numgen .eq. 0) error = 1  
C       
C       Sort generator dropping arrays.   
C       
        if (error .eq. 0) then
           if (numdrp .gt. 1) call qiksrt (1, numdrp, kmpgd1, swpgd1) 
           if (numgen .gt. 1) call qiksrt (1, numgen, kmpgd2, swpgd2) 
        endif 
        
        o2 = index (out_buffer,null)
c
c       Append error messages to buffer
c
  330   j = 1 
        length = 1
        do while (j .le. numerr .and. length .gt. 0)
           length = apdoutbuf(o2, errm(j), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
c
c 	Append summary
c
  900   write (text, 10180) 'p_gendrp.f', p_gendrp, ostates
10180   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
        

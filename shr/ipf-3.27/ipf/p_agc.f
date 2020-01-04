C    @(#)p_agc.f	20.2 1/7/99
C****************************************************************
C
C       File: p_agc.f
C       Purpose: IPF shell program to process
C                                                                      
C       This subroutine processes the following commands
c       
c         / AGC
C         C ...
C
C       Author: Walt Powell  Date: 19 Dec 1997
C                            Modified: 19 Dec 1997
C       Called by: 
C
C****************************************************************
C
	integer function p_agc (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/agc.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/com007.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/ordsta.inc'
        include 'ipfinc/qsdup.inc'

        common /is_batch / is_batch

        character bus1*8, word(100)*30, capital*30, commnt*54, 
     &            type*2, cfmt*7, null*1, linefeed*1, text*120
        integer first, last, apdoutbuf, o2, error, find_bus,
     &          findstr
        logical opened, finished
        external kmpagc, swpagc, find_bus

C       LUN is logical unit number of auxiliary output.             
C       NUMREC is count of records written to auxiliary output file.
C                                                                      
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null

        p_agc = 0        ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        error = 0
c
c       Position in_buffer to next record
c
        first = 1
        last = index (in_buffer, null)
        if (last .eq. 0) last = len (in_buffer)
        o2 = 1
c
c       Align in_buffer past "AGC"
c
c       findstr is a case-insensitive version of index
        first = findstr (in_buffer, 'AGC') + len ('AGC')
        if (in_buffer(first:first) .eq. linefeed .or.
     &      in_buffer(first:first) .eq. null) first = first + 1

        finished = .false.
        do while (first .lt. last .and. .not. finished)
          next = nxt_term(in_buffer(first+1:)) + first
          if (first .lt. next) then
            text = in_buffer(first:next-1)
            write (outbuf,10000) text(1:80) 
10000       format (' AGC text (', a, ')')
            call prtout (1)
            if (text(1:1) .eq. '.') then   
            else if (text(1:1) .eq. 'B') then  
              read (text, 10010, err=180) bus1, base1 
10010         format (bz, t7, a8, f4.0)  
              nb = find_bus (bus1, base1)  
              if (nb .le. 0) then
                write (errbuf(1), 10020) bus1, base1   
10020           format ('/ AGC bus ', a8, f6.1, ' is not in system.')   
                call prterx ('W', 1)
                error = 1   
              else   
                if (numagc .ge. MAXAGC) then
                  write (errbuf(1), 10030) MAXAGC 
10030             format ('More than ', i3, ' / AGC buses.')   
                  if (is_batch .eq. 0) then
                    call prterx ('E',1)
                  else
                    call prterx ('F',1)
                  endif
                  error = 1
                else
                  numagc = numagc + 1  
                  call uscan(text(20:),word,nwrd,'=',' ,()<>/\\')   
                  pmax = busdta(7,nb)  
                  pmin = 0.0   
                  pcttot = 0.0 
                  if (ordvlt .eq. 2) then
                     kt = inp2opt(nb)
                  else
                     kt = nb
                  endif   
                  pgen = (pnetu(kt) +ploadu(kt)) * bmva  
                  if (pgen .le. 0.0) then  
                    write (errbuf(1), 10040) bus1, base1, pgen * bmva   
10040               format ('/ AGC bus ', a8, f6.1,   
     1               ' is not a generator: P_Gen = ', f8.1) 
                    call prterx ('W', 1)  
                  endif
                  kagc(1,numagc) = nb  
                  kagc(2,numagc) = 0   
                  kagc(3,numagc) = 0   
                  agc(5,numagc) = pmax / bmva  
                  agc(6,numagc) = 0.0  
                  agc(7,numagc) = pgen / bmva  
                  agc(8,numagc) = (pnetu(kt) + ploadu(kt)) * bmva
                  agc(8,numagc) = sngl(ploadu(kt))
                  kagc(9,numagc) = 0   
                  kagc(10,numagc) = 0  
                  kagc(11,numagc) = 0  
                  kagc(12,numagc) = ntypu(kt)         
                  pmin = 0.0   
                  do i = 1, nwrd, 3
                    word(i) = capital (word(i))
                    if (word(i)(1:1) .eq. '%') then
                      pct = ftn_atof (word(i+2))
                      agc(4,numagc) = 0.01 * pct
                    else if (word(i) .eq. 'PMAX') then
                      pmax  = ftn_atof (word(i+2))
                      agc(5,numagc) = pmax / bmva
                    else if (word(i) .eq. 'PMIN') then
                      pmin = ftn_atof (word(i+2))
                      agc(6,numagc) = pmin / bmva
                    else if (word(i) .eq. 'PGEN') then
                      pgen = ftn_atof (word(i+2))
                      pnetu(kt) = dble( pgen / bmva) - ploadu(kt) 
                      agc(7,numagc) = pgen / bmva
                    else  
                      last = lastch (word(i))
                      write (errbuf(1), 10050) word(i)(1:last) 
10050                 format ('Unrecognized keyword (', a, 
     &                        ') ignored.')   
                      call prterx ('W',1)
                    endif 
                  enddo
                  if (pmin .gt. pgen .or. pgen .gt. pmax) then 
                    write (errbuf(1), 10060) bus1, base1, pmin, pgen, 
     &                pmax  
10060               format ('/AGC bus ', a8, f6.1,
     &                ' Initial generation violates limits: P_min =',
     &                f7.1, ' P_gen =', f7.1, ' P_max =', f7.1)  
                    call prterx ('W', 1)  
                  endif
                endif   
              endif  
            else if (text(1:1) .eq. '/' .or. text(1:1) .eq. '(') then
              finished = .true.
            else
              write (errbuf(1), 10070) text(1:60)   
10070         format ('Illegal AGC record (', a, ')') 
              if (is_batch .eq. 0) then
                call prterx ('E',1)
              else
                call prterx ('F',1)
              endif
              error = 1  
            endif
          endif
          first = next + 1

          go to 190

  180     continue
          write (errbuf(1), 10080) text(1:60)   
10080     format ('Illegal data in field (', a, ')') 
          if (is_batch .eq. 0) then
            call prterx ('E',1)
          else
            call prterx ('F',1)
          endif
          error = 1  

  190     continue
        enddo
        
  200   buf = '( END ) GETAGC'
        card = buf(1:1)   
        
  210   if (numagc .eq. 0) then   
          write (errbuf(1), 10090 ) 
10090     format ('No generators specified with / AGC')   
          if (is_batch .eq. 0) then
            call prterx ('E',1)
          else
            call prterx ('F',1)
          endif
          error = 1  
          go to 900  
        else if (numagc .gt. 1) then  
          dupsw = .false.
          call qiksrt (1, numagc, kmpagc, swpagc)
          if (dupsw) then
            finished = .false.
            do while (.not. finished)
              finished = .true.
              last = numagc   
              do i = 1, last-1
                if (kagc(1,i) .eq. kagc(1,i+1)) then 
                  nb = kagc(1,i)
                  write (errbuf(1), 10100) bus(nb), base(nb)   
10100             format ('Duplicate AGC generators: ', a8, f7.1,
     1                    '. Second entity deleted.')
                  call prterx ('W', 1)  
                  do j = i+1, last-1
                    do k = 1, 14   
                      kagc(k,j) = kagc(k,j+1)
                    enddo
                  enddo
                  numagc = numagc - 1   
                  finished = .false.
                endif
              enddo
            enddo
          endif  
        endif 
        numslk = 0
        numare = 0
        pcttot = 0.0  
        ptot = 0.0
        do i = 1, numagc
          nb = kagc(1,i)
          pcttot = pcttot + agc(4,i)
          ptot = ptot + agc(5,i)
C
C         Flag generator if it is a system or area slack bus.
C
          do j = 1, ntotc
            k = karea(1,j)  
            if (ordtie .eq. 2) k = opt2inp(k) 
            if (k .eq. nb) then 
              kagc(9,i) = j
              numslk = numslk + 1  
              go to 240
            endif   
          enddo
  240     continue   
          if (numare .eq. 0) then
            numare = jarzn(nb)  
          else if (numare .ne. jarzn(nb)) then   
            write (errbuf(1), 10110) bus(nb), base(nb), 
     &        arcnam(jarzn(nb)) 
10110       format ('/AGC bus ', a8, f6.1,  
     &              ' is in wrong area (', a, ')')   
            call prterx ('W', 1)
          endif
        enddo
C
C       Check for viable percentage scheme: PTOT > 0.
C
        if (ptot .le. 0.1) then
          write (errbuf(1), 10120) ptot
10120     format ('/ AGC scheme does not include any generators: ',
     &            'P_total = ', f8.1)
          if (is_batch .eq. 0) then
            call prterx ('E',1)
          else
            call prterx ('F',1)
          endif
          error = 1
          go to 900
        endif
        if (numslk .eq. 0) then
          write (errbuf(1), 10130)
10130     format ('No area or system slack bus included in ',
     &           '/ AGC control scheme.')
          if (is_batch .eq. 0) then
            call prterx ('E',1)
          else
            call prterx ('F',1)
          endif
          error = 1
        endif
 
        call forbtm
        outbuf = ' Initialization of AGC Generators '
        call rpnlod
        write (outbuf, 10140)
10140   format(t2, 'Type',
     &         t8, 'Generators',  
     &         t26, '- Percentage -',  
     &         t42, '----- Power (MW) ------', 
     &         t78, 'Comments')
        call shdlod(1)
        write (outbuf, 10150)   
10150   format(t26,'Scheduled Used',  
     &         t42,'  P_min  P_base  P_max ') 
        
        call shdlod(2)
        outbuf= ' '   
        call shdlod(3)
        call shdlod(4)
        call shdlod(5)
        call fortop   
          
        do i = 1,numagc   
          k = kagc(1,i)  
        
          percnt = agc(4,i)  
          commnt = ' '   
          pctgen = agc(5,i) / ptot   
          if (abs (pcttot - 1.0) .lt. 0.01) then 
            pctuse = percnt 
            if (abs(pctuse - pctgen) .gt. 0.10) then
              commnt = '% used is not proportional to P_max'   
            endif   
          else if (abs(pcttot) .lt. 0.01) then   
            pctuse = agc(5,i) / ptot
            agc(4,i) = pctuse   
            commnt = '% allocated in proportion to P_max'   
          else   
            pctuse = agc(4,i) / pcttot  
            agc(4,i) = pctuse   
            write (commnt, 10160) agc(4,i) * 100.0
10160       format ('% scaled from ', f8.2) 
          endif  
        
          type = 'B' // bustyp(kagc(12,i))   
          write (outbuf, 10170) type, bus(k), base(k), 100.0 * percnt, 
     &      100.0 * pctuse, agc(6,i) * bmva, agc(7,i) * bmva,   
     &      agc(5,i) * bmva, commnt 
10170     format(t2, a2, t8, a8, f7.1, t24, f6.1, f8.1, t39, 3f8.1,  
     2      t78, a) 
          call prtout(1)   
        enddo
        
        outbuf = ' '  
        call rpnlod   
        call shdlod (1)   
        call shdlod (2)   
        call shdlod (3)   
        call shdlod (4)   
        call shdlod (5)   
        outbuf = '0 End of / AGC '
        call prtout(1)
        call forbtm   
        
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
        write (text, 10180) 'p_agc.f', p_agc, ostates
10180   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
  900   call setercnt (0, ' ')
        return
	end

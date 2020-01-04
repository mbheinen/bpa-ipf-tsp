C    %W% %G%
C****************************************************************
C
C   File: p_cmde2.f
C   Purpose: IPF shell program to process /common_mode_analysis 
C            commands
C
C   Author: Walt Powell  Date: 14 Jule 1998
C                        Modified: 
C   Called by: pf_proc, p_commnd
C
C****************************************************************
C
	integer function p_cmde2 (in_buffer, out_buffer) 

        include 'ipfinc:parametr.inc'

        character in_buffer * (MAXBUFFER)
        character out_buffer * (MAXBUFFER)

        include 'ipfinc:alpha.inc'
        include 'ipfinc:bus.inc'
        include 'ipfinc:blank.inc'
        include 'ipfinc:jobctl.inc'
        include 'ipfinc:lfiles.inc'
        include 'ipfinc:filnam.inc'
        include 'ipfinc:errorsw.inc'
        include 'ipfinc:pfstates.inc'
        include 'ipfinc:prt.inc'
        include 'ipfinc:errmsg.inc'
        include 'ipfinc:errorx.inc'
        include 'ipfinc:cmde_com.inc'
        include 'ipfinc:comm_mode.inc'
        include 'ipfinc:changr.inc'
        include 'ipfinc:delete.inc'
        include 'ipfinc:intrst.inc'

        character null*1, linefeed*1, text*80, tempc*120, word(50)*60, 
     &            bigbuf*512, file(4)*60, temp_buffer*(MAXBUFFER), 
     &            case*10, ljstfy*120
        integer open_file, status, o2, apdoutbuf, p_gtcmde2,
     &          p_change, p_chgbty, p_cnvtld, p_solton, p_chgpar,
     &          p_agc, p_gendrp, firstxstr, error
        logical baseok, finished 
C
C       Syntax: /common_mode_analysis, base_file=<base_file>, -
C                                      common_mode=<common_mode_file>, -
C                                      outage_file=<outage_file>, -
C                                      out_file=<output_file> 

        null = char(0)
        linefeed = char(10)
        p_cmde2 = 0      ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        o2 = 1
        out_buffer(1:1) = null
c
c       Set flag for /alt_case/
c
        flag = 'cmde_init'
C       
C       Check for and concatenate continuation records.
C       
        bigbuf = inrcd
        last = lastch (bigbuf) 
        do while (bigbuf(last:last) .eq. '-') 
          read (inp, fmt='(a)', end=900) tempc
          call space (1)  
          write (outbuf, 10000) tempc(1:80)   
10000     format (' COMMON_MODE_ANALYSIS text (', a, ')')  
          call prtout (1) 
          bigbuf(last:) = tempc
          last = lastch (bigbuf) 
        enddo

        call uscan (bigbuf(1:last), word, nwrd, '=()',  ' ,')   

        do i = 1, 4
          file(i) = ' '
        enddo
        iwrd = 2
        do while (iwrd .le. nwrd)
          if (firstxstr(word(iwrd), 'BASE*') .ne. 0) then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            file(1) = word(iwrd)
          else if (firstxstr(word(iwrd), 'COMMON*') .ne. 0) then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            file(2) = word(iwrd)
            status = open_file (busbrn, file(2), 'F', 'R', iostat)
            if (status .ne. 0) then
              last = lastch (file(2))
              write (errbuf(1), 10030) file(2)(1:last)
10030         format ('Error opening COMMON_MODE_FILE ', a)
              call prterx('E', 1)
              jobreq(1) = 'quit'
              p_cmde2 = 1
              ostates = 0
              go to 900
            endif
          else if (firstxstr(word(iwrd), 'OUTAGE*') .ne. 0) then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            file(3) = word(iwrd)
c
c           Defer processing Outage Simulation file until system changes
c           have been made.
c

          else if (firstxstr(word(iwrd), 'OUT*') .ne. 0) then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            file(4) = word(iwrd)
            status = open_file (wscfil, file(4), 'F', 'W', iostat)
            if (status .ne. 0) then
              last = lastch (file(4))
              write (errbuf(1), 10050) file(4)(1:last)
10050         format ('Error opening OUT_FILE ', a)
              call prterx('E', 1)
              jobreq(1) = 'quit'
              p_cmde2 = 1
              ostates = 0
              go to 900
            endif
          else
            last = lastch (word(iwrd))
            write( errbuf(1), 10060) word(iwrd)(1:last)
10060       format('Unrecognized parameter in /COMMON_MODE_ANALYSIS (',
     &         a, ')')
            call prterx ('E', 1)
            outbuf = errbuf(1)
            call prtout (1)
          endif
          iwrd = iwrd + 1
        enddo

        if (ostates .le. 1 .and. file(1) .eq. ' ') then
          write (errbuf(1), 10070)
10070     format ('No base case data in residence')
          call prterx('E', 1)
          jobreq(1) = 'quit'
          p_cmde2 = 1
          ostates = 0
          go to 900
        else if (ostates .le. 1) then
          obasnm = file(1)  ! Use temporary file name since
C                           ! gtbase returns file in upper case
        endif
        if (file(2) .eq. ' ') then
          write (errbuf(1), 10080)
10080     format ('No COMMON_MODE_FILE opened')
          call prterx('E', 1)
          jobreq(1) = 'quit'
          p_cmde2 = 1
          ostates = 0
          go to 900
        endif
        if (file(3) .eq. ' ') then
          write (errbuf(1), 10090)
10090     format ('No OUTAGE_SIMULATION file opened')
          call prterx('E', 1)
          jobreq(1) = 'quit'
          p_cmde2 = 1
          ostates = 0
          go to 900
        endif
        if (file(4) .eq. ' ') then
          write (errbuf(1), 10100)
10100     format ('No OUTPUT file opened')
          call prterx('E', 1)
          jobreq(1) = 'quit'
          p_cmde2 = 1
          ostates = 0
          go to 900
        endif
c
c       Align COMMON_MODE file to first > mode command
c
        rewind inp
c       Initialize Common Mode counters here to allow the data from multiple
c       COMMON_MODE  commands/files to be appended
c
        num_comm_mode = 0
        num_changes = 0
        num_types = 0
        finished = .false.
        read (busbrn, fmt='(a)', end=900) inrcd
        do while (.not. finished)
          text = ljstfy(inrcd(2:))
          if (inrcd(1:1) .eq. '>' .and. 
     &        firstxstr(text, 'MODE*') .ne. 0) then
            if (num_comm_mode .eq. MAXCOMMODE) then
              write (errbuf(1), 10110) MAXCOMMODE
10110         format (' More than ', i3, ' > COMMON_MODE records')
              write (errbuf(2), 10120) inrcd(1:80)
10120         format (' Overflow at (', a, ')')
              call prterx ('W', 2)
              error = 2
              go to 900
            endif
            num_comm_mode = num_comm_mode + 1
            text = ljstfy(inrcd(2:))
            ix = firstxstr(text, 'MODE') + 5
            do i = 1, num_comm_mode
              if (inrcd(ix:ix+39) .eq. comm_mode(i)(1:40)) then
                write (errbuf(1), 10130) inrcd(1:50)
10130           format (' > > > Duplicate MODE record skipped (',
     &             a, ')  < < <')
                call prterx ('W', 1)
                num_comm_mode = num_comm_mode - 1
                read (busbrn, fmt='(a)', end=900) inrcd
                text = ljstfy(inrcd(2:))
                do while ((inrcd(1:1) .ne. '>' .and.
     &                     firstxstr(text, 'MODE*') .eq. 0) .and.
     &                    (inrcd(1:1) .ne. '(' .and.
     &                     firstxstr(text, 'END') .eq. 0))
                  read (busbrn, fmt='(a)', end=900) inrcd
                  text = ljstfy(inrcd(2:))
                enddo
                go to 100
              endif
            enddo
  100       continue
            buf = inrcd
            comm_mode(num_comm_mode) = text(ix:)

            if (ostates .le. 1 .or. num_comm_mode .gt. 0) then
              crun1(3) = ' '
              call gtbase (obasnm, crun1(3), baseok)
              if (.not. baseok) then                                           
                 last = lastch (file(1))
                 write (errbuf(1), 10020) file(1)(1:last)
10020            format ('Error opening/loading base case file'
     &)
                 call prterx ('E', 1)
                 jobreq(1) = 'quit'
                 p_cmde2 = 1
                 ostates = 0
                 go to 900
              endif
C       
C             Store Q_NET in CAPCOR(2,*)
C       
              do nb = 1, ntot   
                kt = inp2opt(nb) 
                capcor(2,kt) = qnetu(kt)          
              enddo
c
c             Store base case voltages
c                                                                        
              do nbx = 1, ntot_alf  
                nb = alf2inp(nbx)
                kt = inp2opt(nb)  
                vbase(nb) = dsqrt (e(kt)**2 + f(kt)**2)  
              enddo
            endif
c
c           Initialize change archive
c
            numchg = 0
            numchgfl = 0
            numold = 0
            ndelete = 0
c
c           Process individual / commands.
c
            read (busbrn, fmt='(a)', end=900) inrcd
            buf = inrcd
            text = ljstfy(inrcd(2:))
            do while (inrcd(1:1) .ne. '>' .and.
     &                firstxstr(text, 'MODE*') .eq. 0) 
              if (inrcd(1:1) .eq. '/') then
                tempc = inrcd
                rewind inp
                i1 = 1
                last = lastch (inrcd)
                write (inp, '(a)') inrcd(1:last)
                temp_buffer(i1:) = inrcd(1:last)
                temp_buffer(i1+last:) = linefeed
                i1 = i1 + last + 1
                read (busbrn, fmt='(a)', end=110) tempc
                text = ljstfy(tempc(2:))
                do while ((tempc(1:1) .ne. '/') .and.
     &                    (tempc(1:1) .ne. '>' .or.
     &                     firstxstr(text, 'MODE*') .eq. 0))
                  last = lastch (tempc)
                  write (inp, '(a)') tempc(1:last)
                  temp_buffer(i1:) = tempc(1:last)
                  temp_buffer(i1+last:) = linefeed
                  i1 = i1 + last + 1
                  read (busbrn, fmt='(a)', end=110) tempc
                  text = ljstfy(tempc(2:))
                enddo
                goto 120

  110           tempc = '( END )'
  120           text = '( END )'
                last = lastch (text)
                write (inp, '(a)') text(1:last)
                temp_buffer(i1:) = text(1:last)
                temp_buffer(i1+last:) = linefeed
                i1 = i1 + last + 1

                temp_buffer(i1:) = null
                rewind inp
                read (inp, fmt='(a)', end=130) inrcd
                buf = inrcd

                text = ljstfy(inrcd(2:))
                if (firstxstr(text, 'CHANGES*') .ne. 0) then
                  o2 = 1
                  out_buffer(o2:o2) = null
                  status = p_gtcmde2 (inp)
                  if ( num_comm_mode .gt. 0 ) then
                    call cmde2_sum (error, num_comm_mode, wscfil)
                  endif
                  status = p_change (temp_buffer, out_buffer(o2:)) 

                else if (firstxstr(text, 'CHANGEBUS*') .ne. 0) then
                  o2 = 1
                  out_buffer(o2:o2) = null
                  status = p_chgbty (temp_buffer, out_buffer(o2:))

                else if (firstxstr(text, '%LOAD*') .ne. 0) then
                  o2 = 1
                  out_buffer(o2:o2) = null
                  status = p_cnvtld (temp_buffer, out_buffer(o2:))

                else if (firstxstr(text, 'SOLUTION*') .ne. 0) then
                  o2 = 1
                  out_buffer(o2:o2) = null
                  status = p_solton (temp_buffer, out_buffer(o2:)) 

                else if (firstxstr(text, 'CHANGEP*') .ne. 0) then
                  o2 = 1
                  out_buffer(o2:o2) = null
                  status = p_chgpar (temp_buffer, out_buffer(o2:)) 

                else if (firstxstr(text, 'AGC*') .ne. 0) then
                  o2 = 1
                  out_buffer(o2:o2) = null
                  status = p_agc (temp_buffer, out_buffer(o2:)) 

                else if (firstxstr(text, 'GENDROP*') .ne. 0) then
                  o2 = 1
                  out_buffer(o2:o2) = null
                  status = p_gendrp (temp_buffer, out_buffer(o2:)) 

                else 
                  write (errbuf(1), 10140) inrcd(1:50)
10140             format (' /COMMON_MODE_ANALYSIS command not processed 
     &(',
     &            a, ')')
                  call prterx ('W', 1)
                endif
                inrcd = tempc
                buf = inrcd

              else
                write (outbuf, 10150) inrcd(1:50)
10150           format (' /COMMON_MODE_ANALYSIS text skipped (',
     &            a, ')')
                call prtout (1)
                read (busbrn, fmt='(a)', end=130) inrcd
                buf = inrcd
              endif
              text = ljstfy(inrcd(2:))
            enddo

            goto 140

  130       tempc = '( END )'
  140       text = '( END )'
            inrcd = tempc
c
c           Process Outage Simulation Commands
c
            status = open_file (busfil, file(3), 'F', 'R', iostat)
            if (status .ne. 0) then
              last = lastch (file(3))
              write (errbuf(1), 10040) file(3)(1:last)
10040         format ('Error opening OUTAGE_SIMULATION ', a)
              call prterx('E', 1)
              jobreq(1) = 'quit'
              p_cmde2 = 1
              ostates = 0
              go to 900
            endif
            call cmde2_inp (busfil)
            call close_file (busfil)
            call cmde2_ovld (num_comm_mode)

          else
            write (outbuf, 10150) inrcd(1:50)
            call prtout (1)
            read (busbrn, fmt='(a)', end=200) inrcd
            buf = inrcd
          endif

          buf = inrcd

        enddo
  
  200   call close_file (busbrn)

        crun1(3) = ' '
        call gtbase (obasnm, crun1(3), baseok)
C       
C       Store Q_NET in CAPCOR(2,*)
C       
        do nb = 1, ntot   
          kt = inp2opt(nb) 
          capcor(2,kt) = qnetu(kt)          
        enddo

        call cmde2_out (wscfil, file(4))

c       Append error messages to buffer
c
  900   continue
        j = 1 
        length = 1
        o2 = index (out_buffer,null)
        do while (j .le. numerr .and. length .gt. 0)
           length = apdoutbuf(o2, errm(j), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
c
c 	Append summary
c
        write (text, 340) 'p_cmde2.f', p_cmde2, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end

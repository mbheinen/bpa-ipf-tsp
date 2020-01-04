C    @(#)get_orpt22.f	20.7 1/7/99
C****************************************************************
C
C       File: get_orpt22
C       Purpose: Routine to select various sensitivity output reports
C
C                / REPORTS, SELECT LOSS_SENSITIVITY
C                  WHERE LOADING = "<minimum sensitivity>"
C                (END)
C       or
C                / REPORTS, SELECT BUS_SENSITIVITY
C                  WHERE BUS = "<busname>"
C                (END)
C       or
C                / REPORTS, SELECT PHASE_SHIFTER_SENSITIVITY
C                (END)
C       or
C                / REPORTS, SELECT LINE_SENSITIVITY
C                  WHERE LOADING = "<minimum sensitivity>"
C                (END)
C       or
C                / REPORTS, SELECT OWNER_SENSITIVITY
C                  WHERE LOADING = "<minimum sensitivity>"
C                (END)
C
C       Author: Walt Powell  Date: 27 July 1994
C                            Modified: 27 July 1994
C       Called by:
C
C****************************************************************
C
	subroutine get_orpt22 (scrfil)
        integer scrfil

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/blank.inc'
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/dtaiop.inc'
	include 'ipfinc/ecvar.inc'
	include 'ipfinc/bus.inc'
	include 'ipfinc/branch.inc'
	include 'ipfinc/arcntl.inc'
        include 'ipfinc/ordsta.inc'
	include 'ipfinc/ownhash.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/update.inc'
        include 'ipfinc/anlys.inc'
        include 'ipfinc/busanl.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/pageno.inc'
        include 'ipfinc/header.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/jobctl.inc'

        common /ownflg/ ownflg 
        logical ownflg         

        common /shellsort/buffer(1000)
        integer buffer

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null*1, linefeed*1, capital*20, ljstfy*2, bus_name*8,
     &            text*120, query*2, word(20)*20, printf*60, tempc*2
	integer apdoutbuf, o1, o2, findstr, status, busback,
     &          open_file, pagenox, linenox, fichpgx, fichlnx, lstdnlx, 
     &          lstdnfx, find_bus, ptr, br_list(50), brn_ptr(10)
        logical finished, is_factored1, is_factored2, loop1
        external komp_buf, swap_buf

        data is_factored1, is_factored2 / .false., .false. /

        save

        null = char(0)
        linefeed = char(10)
        linespage = 22

        header = ' '
        repnam = ' '
        coment(1) = ' '
        coment(2) = ' '
        outbuf = ' '
        do i = 1, 5
          call shdlod(i)
        enddo

C       Define logical units

        crtsw = 0
        scrfil = lprt
        if (ntotx .eq. 0) then
          write (*, 10020)
10020     format (
     &   ' * Rebuilding solution arrays. This will take a minute.')
  
C         Temporarily divert all lprt output to scratch file. Store
C         pagination variables.
  
          lprtx = lprt
          lprtswx = lprtsw
          lprt = svchfl
          pagenox = pageno
          linenox = lineno
          fichpgx = fichpg
          fichlnx = fichln
          lstdnlx = lstdnl
          lstdnfx = lstdnf
  
          inquire (unit=lprt,name=printf)
          call close_file(lprt)
          status = open_file(lprt, ' ', 'FF', 'W', ios)
          buf = '(end)'
          card = buf(1:1)
          if (ordtbx .ne. 2) then
            ordymx = 2	        ! Set flag to not reorder y-matrix
            call chgorder('inp2opt')
          endif
  
          option(9) = 1.0	! Set minimum phase shift 
c                               ! representation to 1 degree.
          call presln()
          iopton(17) = 1        ! Set area interchange control on
          iopton(18) = 0	! Set flat start off
          kspare(20) = 1        ! Set area interchange constraints on 
c                               ! for sensitivity factorization
          kspare(19) = 2        ! Set LTC's on for sensitivity 
c                               ! factorization
          kspare(24) = 1	! Do not reinitialize XDATA
          text(1:1) = '1'
  
          call opsln1()
          iopton(1) = 0      	! Set number of decoupled iterations 
c                               ! = 0
          call dcstrt(kerr)
          call nrcnvt()
          is_factored1 = .false.
          is_factored2 = .false.
  
C         Now restore lprt, Restore pagination variables.
  
          call close_file(lprt)
          lprt = lprtx
          lprtsw = lprtswx
          pageno = pagenox
          lineno = linenox
          fichpg = fichpgx
          fichln = fichlnx
          lstdnl = lstdnlx
          lstdnf = lstdnfx
          status = open_apnd( lprt, printf)
        endif

        write(*, 9030)
 9030   format(' * Select Area Interchange Options: 0 = Off ', /,
     &         '                                    1 = On ', /,
     &         ' > Enter selection: ', $)
        read (*, '(a)') text
        if (text(1:1) .eq. '1') then
           iopton(17) = 1  ! Set area interchange control on
           kspare(20) = 1  ! Set area interchange constraints on for
C                          ! sensitivity factorization
        else 
           iopton(17) = 0  ! Set area interchange control off
           kspare(20) = 0  ! Set area interchange constraints off for
C                          ! sensitivity factorization
        endif
c
c       Select type of sensitivities
c
   95   loop1 = .true.
        do while (loop1)
           write (*, 98)
   98      format (' * Types of sensitivities > 1) Losses ',
     &        /,   '                            2) Buses ',
     &        /,   '                            3) Phase shifters ',
     &        /,   '                            4) Lines ',
     &        /,   '                            5) Owners ',
     &        /,   '                            Q) Quit ',
     &        /,   ' > Enter selection : ',$)

           read (*, 99) query
   99      format (a)
           query = capital (query)
           query = ljstfy (query)

           if (query .eq. 'Q') go to 900
           iquery = 0
           if (query(2:2) .eq. ' ') then
             tempc = ' ' // query
             query = tempc
           endif
           read (query, '(i2)', err=101) iquery
  101      if (iquery .eq. 1) then
c
c            Loss sensitivities
c
             if (.not. is_factored1) then
               write (*, 10001)
10001          format (' * Factoring Sensitivity Matrices. This will tak
     &e a minute.')
               call senfac()
               is_factored1 = .true.
               is_factored2 = .false.
             endif

             in_buffer(1:1) = null
             i1 = index (in_buffer, null)
             out_buffer(1:1) = null
             length = apdoutbuf(i1,
     &                  '/REPORTS, SELECT LOSS_SENSITIVITIES',
     &                  in_buffer(i1:))
             i1 = i1 + length
             ix = findstr (in_buffer, 'LOSS_SENSITIVITIES')
     &          + lastch ('LOSS_SENSITIVITIES')

             call lossenrpt(in_buffer(ix:), out_buffer, scrfil)
             o1 = 1
             do while (o1 .lt. MAXBUFFER .and.
     &                 out_buffer(o1:o1) .ne. null)
               o2 = nxt_term(out_buffer(o1+1:)) + o1
               if (out_buffer(o1:o1) .eq. '/') then
                 o2 = MAXBUFFER + 1
               else
                 last = min0 (o1+78, o2-1)
                 write (*, '(1x, a)') out_buffer(o1:last)
               endif
               o1 = o2
               if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
             enddo
           else if (iquery .eq. 2) then
C
C            Bus sensitivities: Enter kernal bus
C
             if (.not. is_factored1) then
               write (*, 10001)
               call senfac()
               is_factored1 = .true.
               is_factored2 = .false.
             endif

             null = char(0)
             linefeed = char(10)

             finished = .false.
             do while (.not. finished)
               last = 1
               write (*,210)
  210          format (' > Enter busname, KV, buses-back : ',$)
               read (*,212) text
  212          format (a)
               bus_name = ' '
               base_kv = 0.0
               busback = 0
               if (text .eq. ' ') go to 228
c
C              Parse TEXT into busname, kv, and buses-back
c
               call parse_bus (text, word, nwrd)
               do i = nwrd+1, 3
                 word(i) = ' '
               enddo
               bus_name = capital(word(1))
               if (index(word(2), '.') .eq. 0) then
                 last = lastch(word(2))
                 word(2) = word(2)(1:last) // '.0'
               endif
               read (word(2), fmt='(f6.0)', err = 220) base_kv
               read (word(3), fmt='(i1)', err = 220) busback

               if (bus_name .eq. ' ') go to 900
               nb = find_bus (bus_name,base_kv)

               if (nb .gt. 0) then
                 ilevel = 0
                 first = 1
                 last = 1
                 buffer(last) = nb
                 next = last
                 do while (ilevel .le. busback .and. next .ge. last)
                   if (first .lt. last) 
     &               call shellsrt (first, last, komp_buf, swap_buf)
                   do iv = first, last
                     nb = buffer(iv)
                     call bcdbus (nb, text)

                     in_buffer(1:1) = null
                     i1 = index (in_buffer, null)
                     length = apdoutbuf(i1,
     &                 '/REPORTS, SELECT BUS_SENSITIVITIES WHERE BUS = '
     &                 // '"' // text(7:18) // '"' // linefeed
     &                 // '(END)',
     &               in_buffer(i1:))
                     out_buffer(1:1) = null
                     ix = findstr (in_buffer, 'BUS_SENSITIVITIES')
     &                 + lastch ('BUS_SENSITIVITIES')
                     call bussenrpt(in_buffer(ix:), out_buffer,
     &                              scrfil)
                     o1 = 1
                     do while (o1 .lt. MAXBUFFER .and.
     &                         out_buffer(o1:o1) .ne. null)
                       o2 = nxt_term(out_buffer(o1+1:)) + o1
                       if (out_buffer(o1:o1) .eq. '/') then
                         o2 = MAXBUFFER + 1
                       else
                         last = min0 (o1+78, o2-1)
                         write (*, '(1x, a)') out_buffer(o1:last)
                       endif
                       o1 = o2
                       if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
                     enddo

                     ptr = kbsdta(16,nb)
                     do while (ptr .gt. 0)
                       do j = 1, next
                         if (buffer(j) .eq. ky(ptr)) go to 242
                       enddo
                       next = next + 1
                       buffer(next) = ky(ptr)
  242                  continue
                       ptr = brnch_nxt(ptr)
                     enddo
                   enddo
                   ilevel = ilevel + 1
                   first = last + 1
                   last = next
                 enddo
               else
                 call ermisbus (bus_name, base_kv, text)
               endif
               go to 226

  220          write (*, 224)
  224          format (' *** Illegal data entry ***')

  226          continue
             enddo
  228        continue

           else if (iquery .eq. 3) then
c
c            Phase shifter sensitivities
c
             if (.not. is_factored1) then
               write (*, 10001)
               call senfac()
               is_factored1 = .true.
               is_factored2 = .false.
             endif

             in_buffer(1:1) = null
             i1 = index (in_buffer, null)
             out_buffer(1:1) = null
             length = apdoutbuf(i1,
     &                  '/REPORTS, SELECT PHASE_SHIFTER_SENSITIVITIES',
     &                  in_buffer(i1:))
             i1 = i1 + length
             ix = findstr (in_buffer, 'PHASE_SHIFTER_SENSITIVITIES')
     &          + lastch ('PHASE_SHIFTER_SENSITIVITIES')
             call phssenrpt(in_buffer(ix:), out_buffer, scrfil)
             o1 = 1
             do while (o1 .lt. MAXBUFFER .and.
     &                 out_buffer(o1:o1) .ne. null)
               o2 = nxt_term(out_buffer(o1+1:)) + o1
               if (out_buffer(o1:o1) .eq. '/') then
                 o2 = MAXBUFFER + 1
               else
                 last = min0 (o1+78, o2-1)
                 write (*, '(1x, a)') out_buffer(o1:last)
               endif
               o1 = o2
               if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
             enddo

           else if (iquery .eq. 4) then
c
c            Line Flow sensitivities
c            Build ownership array
c
             if (numown .eq. 0) status = buildzbo (status)
             if (.not. is_factored2) then
c
c              Modify system for fixed/LTC phase shifters
c
               write (*, 10001)
               call sen_bph2 (0)
               call sen_fac2 ()
               is_factored1 = .false.
               is_factored2 = .true.
             endif

             finished = .false.
             do while (.not. finished)
               last = 1
               write (*,230)
  230          format (' > Enter terminal 1 busname, Base KV : ',$)
               read (*, fmt='(a)') text
               bus_name = ' '
               base_kv = 0.0
               busback = 0
               if (text .eq. ' ') go to 310
c
C              Parse TEXT into busname, kv, and buses-back
c
               call parse_bus (text, word, nwrd)
               do i = nwrd+1, 3
                 word(i) = ' '
               enddo
               bus_name = capital(word(1))
               if (index(word(2), '.') .eq. 0) then
                 last = lastch(word(2))
                 word(2) = word(2)(1:last) // '.0'
               endif
               read (word(2), fmt='(f6.0)', err = 280) base_kv

               if (bus_name .eq. ' ') go to 310
               nb = find_bus (bus_name,base_kv)
               if (nb .gt. 0) then
c
c                Select line
c
                 write (*, 240)
  240            format (' * Select line number : ')

                 num_brn = 0
                 ptr = kbsdta(16,nb)
                 do while (ptr .gt. 0 .and. num_brn .lt. 50)
                   num_brn = num_brn + 1
                   br_list(num_brn) = ptr
                   call bcdbrn (ptr, text)
                   write (*, 250) num_brn, text(1:33)
  250              format (' * ', i2, 2x, a)
                   ptr = brnch_nxt(ptr)
                 enddo

                 write (*, 260)
  260            format (' > Enter selection : ',$)

                 read (*, fmt='(a)') query
                 query = capital (query)
                 query = ljstfy (query)

                 if (query .eq. 'Q') go to 310
                 iquery = 0
                 if (query(2:2) .eq. ' ') then
                   tempc = ' ' // query
                   query = tempc
                 endif
                 read (query, fmt='(i2)', err=270) iquery
  270            if (iquery .eq. 0 .or. iquery .gt. num_brn) go to 310

                 ptr = br_list(iquery)
                 brn_ptr(1) = ptr 
                 loop1 = (ptr .gt. 0)
                 do while (loop1)
                   threshold = -1.0
                   do while (threshold .lt. 0.0)
                      write (*, 272)
  272                 format (' > Enter Threshold (percent) ', a, ': ')
                      read (*, '(a)') text
                      last = lastch (text)
                      if (text .eq. ' ' .or. ichar(text(1:1)) .eq. 0) 
     &                  then
                        threshold = -1.0
                        go to 274
                     else
                        threshold = ftn_atof (text)
                     endif
                   enddo
  274              continue
                   if (threshold .lt. 0.0) go to 310
C
C                  Compute dLine/dPi, dLine/dQi sensitivities with the 
C                  following steps (all within module sen_dldx2):
C
C                  1. Compute the objective function dLine/dX
C                  2. Compute the LaGrange multipliers
C
                   call sen_dldx2 (1, brn_ptr, pij, qij)
c
c                  Print out line sensitivity results
c
                   call sen_out2 (1, brn_ptr, pij, qij, threshold)
                 enddo
               endif
               go to 300

  280          write (*, 290)
  290          format (' * Illegal data in field - reenter quantity')

  300          continue
         
             enddo
  310        continue
             inquire (unit=scrfil, name=text)
             last = lastch (text)
             write (*, 320) text(1:last)
  320        format (' * Sensitivity output written to file ', a)
c
c            Restore modified system for fixed/LTC phase shifters
c
             call sen_bph2 (1)

           else if (iquery .eq. 5) then
c
c            Line-Ownership sensitivities
c
             in_buffer(1:1) = null
             i1 = index (in_buffer, null)
             out_buffer(1:1) = null
             length = apdoutbuf(i1,
     &                  '/REPORTS, SELECT OWNER_SENSITIVITIES',
     &                  in_buffer(i1:))
             i1 = i1 + length
             ix = findstr (in_buffer, 'PHASE_SHIFTER_SENSITIVITIES')
     &          + lastch ('PHASE_SHIFTER_SENSITIVITIES')
             call ownsenrpt(in_buffer(ix:), out_buffer, scrfil)
             o1 = 1
             do while (o1 .lt. MAXBUFFER .and.
     &                 out_buffer(o1:o1) .ne. null)
               o2 = nxt_term(out_buffer(o1+1:)) + o1
               if (out_buffer(o1:o1) .eq. '/') then
                 o2 = MAXBUFFER + 1
               else
                 last = min0 (o1+78, o2-1)
                 write (*, '(1x, a)') out_buffer(o1:last)
               endif
               o1 = o2
               if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
             enddo
           else
             write (*, 118)
  118        format (' * Illegal value - reenter selection :', $)
  119        continue
           endif
        enddo
  900   return
        end

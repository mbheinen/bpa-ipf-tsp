C    @(#)p_chgpar.f	20.8 1/7/99
C****************************************************************
C
C   File: p_chgpar.f
C   Purpose: IPF shell program to process /CHANGE_PARAMETERS commands
C
C   Author: Walt Powell  Date: 27 August 1996
C                        Modified: 
C   Called by: 
C
C****************************************************************
C
	integer function p_chgpar( in_buffer, out_buffer ) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/chgprm.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/itrhis.inc'
        include 'ipfinc/update.inc'
        include 'ipfinc/usranl.inc'
        include 'ipfinc/comdrx.inc'
        include 'ipfinc/gendrp.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/pageno.inc'

        common /ownflg/ ownflg
        logical ownflg

        common /sol_temp/ idchgx, kswsln, itlow

        character null*1, linefeed*1, stext*80, prmfil*60, 
     &            tempc*60, word(50)*60, capital*60, bigbuf*512, 
     &            ljstfy*512, code*4, basekvc*4, xpoint*2, ypoint*2
        integer   apdoutbuf, o2, status, open_file, firstxstr, chkerr,
     &            ckzoneup, error, dict(4), pagenox, linenox,
     &            fichpgx, fichlnx, lstdnlx, lstdnfx

        logical initialized, finished
        external nrpqv

        data initialized, prmfil / .false. , ' ' /

        save 

        max_buf = len( out_buffer ) - len( stext ) - 10
        if (max_buf .lt. 0) max_buf = len( out_buffer )
        numerr = 0       ! reinitialize error count
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = 1
        p_chgpar = 0
        error = 0
        lprtx = 0
        
        call space (1) 
        write (outbuf, 70) buf(1:80)  
   70   format (' CHANGE_PAR text (', a, ')')  
        call prtout (1)
        
        bigbuf = buf   
        last = lastch (bigbuf) 
        do while (bigbuf(last:last) .eq. '-')
          read (inp, fmt='(a)', end=80) buf
          call space (1)  
          write (outbuf, 70) buf(1:80)
          call prtout (1) 
          if (buf(1:1) .ne. '.') bigbuf(last:) = ' ' // ljstfy (buf) 
          last = lastch (bigbuf) 
          buf = bigbuf
        enddo
   80   continue
        
        call uscan (in_buffer(1:last), word, nwrd, '=' ,
     &               ', ' // linefeed // null )

c       Align "iwrd" after "FILE ="

        do i = 1, 4
          dict(i) = 0
        enddo

        iwrd = 1
        do while (iwrd .le. nwrd)
          word(iwrd) = capital(word(iwrd))
          if (word(iwrd)(1:5) .eq. 'FILE '  .and.
     &        word(iwrd+1) .eq. '=') then
            dict(1) = 1
            iwrd = iwrd + 2
            prmfil = word(iwrd)
            if (prmfil .ne. '*' .and. prmfil .ne. 'NULL' .and.
     &          prmfil .ne. 'null') then
              last = lastch (prmfil)
              call close_file(23)
              status = open_file (23, prmfil, 'F', 'W', iostat)
            endif
            numprm = 0
            iwrd = iwrd + 1
          else if (word(iwrd)(1:3) .eq. 'BUS'  .and.
     &        word(iwrd+1) .eq. '=') then
            dict(2) = 1
            iwrd = iwrd + 2
          else if (word(iwrd)(1:5) .eq. '%LOAD'  .and.
     &        word(iwrd+1) .eq. '=') then
            dict(3) = 1
            iwrd = iwrd + 2
          else if (word(iwrd)(1:5) .eq. '%GEN'  .and.
     &        word(iwrd+1) .eq. '=') then
            dict(4) = 1
            iwrd = iwrd + 2
          else
            iwrd = iwrd + 1
          endif
        enddo

        if (prmfil .eq. ' ') then
          inquire (unit=inp, name=prmfil)
          i = index (prmfil, ']')
          j = index (prmfil(i+1:), '.') + i
          if (j .eq. 0) then
            j = lastch (prmfil) + 1
            prmfil(j:j) = '.'
          endif
          tempc = prmfil
C
          inquire (unit=inp, name=prmfil)
          i = index (prmfil, ']')
          j = index (prmfil(i+1:), '.') + i
          if (j .eq. 0) then
            j = lastch (prmfil) + 1
            prmfil(j:j) = '.'
          endif

          k = index (prmfil, ']')
          if (prmfil(k+1:k+3) .eq. 'FOR' .or.
     &        prmfil(k+1:k+3) .eq. 'for') then
            tempc = nbasnm
            if (tempc .eq. ' ') tempc = obasnm
            if (tempc .eq. ' ') tempc = prmfil
            prmfil = tempc
          endif
          tempc = prmfil
          i = index (prmfil, ']')
          j = index (prmfil(i+1:), '.') + i
          if (j .eq. 0) then
            j = lastch (prmfil) + 1
            prmfil(j:j) = '.'
          endif
          prmfil = tempc(1:j) // 'QVPT'
          call close_file(23)
          status = open_file (23, prmfil, 'F', 'W', iostat)
          numprm = 0
        endif

        if (.not. initialized) then

          ownflg = .false.        ! Set flag to force updating OWNLOS
          update(1) = 1           ! Set flag to force updating ZSUM
          ickzone = ckzoneup(status)   ! Determine whether it is 
C                                   ! necessary to update ZSUM after
C                                   ! each solution
C
C         Open OUTPUT files.  If none are explicitly defined,
C         create default names.
C
          j = lastch (prmfil)
          do while (j .gt. 1 .and. prmfil(j:j) .ne. '.')
            j = j - 1
          enddo
          do iuser = 1, numusr
            if (usrfil(iuser) .eq. ' ') then
              write (usrfil(iuser), 90) prmfil(1:j),
     1              'USER_REPORT_', iuser
   90         format (a, a, i2.2)
            endif
            lun(iuser) = 26
            call close_file (lun(iuser))
            status = open_file (lun(iuser), usrfil(iuser), 'F', 'W', 
     &                          ios)
            if (status .eq. 0) go to 130

            last = lastch(usrfil(iuser))
            write (errbuf(1), 120) ios, usrfil(iuser)(1:last),
     1             lun(iuser)
  120       format (' Error ', i3, ' opening file ', a,
     1              ' on logical unit ', i2)
            call prterx ('W', 1)

  130       close(unit=lun(iuser))
            numupx(iuser) = 0
          enddo
          initialized = .true.
        endif
c
c       Skip perturbation if no BUS or %LOAD_CHANGE or %GEN_CHANGE
c       is specified
c
        if (dict(2) .eq. 0 .and. dict(3) .eq. 0 .and. dict(4) .eq. 0) 
     &    go to 900
c
c       Temporarily divert all lprt output to scratch file. Store
c       pagination variables. Note that the output file is left open.
c
        lprtx = lprt
        lprtswx = lprtsw
        lprt = svchfl
        pagenox = pageno 
        linenox = lineno 
        fichpgx = fichpg
        fichlnx = fichln 
        lstdnlx = lstdnl 
        lstdnfx = lstdnf

        call close_file (lprt)
        status = open_file (lprt, ' ', 'FF', 'W', ios)

        if (ostates .lt. 4) then
          write (errbuf(1), 140)
  140     format(' Unsolved base data in residence')
          call prterx ('W', 1)
          p_chgpar = 1
          goto 900
        else if (ostates .eq. 5) then
          call chgorder('inp2opt')             !  EXT2IN
          call prtime('chg_order : inp_to_opt')
C
          finished = .false.
          do while (.not. finished)

            call getprm (bigbuf, error)
C
C           We start at iteration 3 because bus switching is permitted.
C           The bus history logic has a maximum iteration limit of 50,
C           which could easily be violated with several parameter studie
C
            ittot = 3
            call nrsoln (ittot, iopton(3), idchgx, kswsln, itlow)
            if (itrhis(2) .eq. 0) itrhis(2) = itrhis(1) + ittot
            if (lskp .ne. 3 .and. numdrp .gt. 0) then
              itrtot = 1
              if (numdrp .gt. 0) then
                drop2 = 1.0e10
                itx = 1
                do while (itx .le. 4 .and. drop2 .ge. 10.0)
                  call comdrp ('NR Iteration', 1, num1, drptmp,
     1                          num2, drop2, status, nrpqv, itrx)
                  ittot = 3
                  call nrsoln (ittot, iopton(3), idchgx, kswsln,
     1                         itlow)
                  if (itrhis(2) .eq. 0) then
                    itrhis(2) = itrhis(1) + ittot
                  endif
                  itx = itx + 1
                enddo
              endif
            else
              ittot = 3
              call nrsoln (ittot, iopton(3), idchgx, kswsln, itlow)
              if (itrhis(2) .eq. 0) itrhis(2) = itrhis(1) + ittot
            endif
            kerr=chkerr('F')
            if (lskp .eq. 3 .or. kerr .gt. 0) then
              write (errbuf(1), 150)
  150         format ('The following / CHANGE_PARAMETERS ',
     &                'commands aborted by failed solution.')
              call prterx ('W', 1)
C
C             Restore solution
C
              call retsln
              error = 1
              p_chgpar = 1
C
C             Skip current and subsequent / CHANGE_PARAMETER records.
C
              do while (card .eq. '/' .and.
     &                  firstxstr (buf(2:), 'CHANGE_PAR') .ne. 0)
                write (errbuf(1), 160) buf(1:80)
  160           format (' COMMAND IGNORED (', a, ')')
                call prterx ('W', 1)

                read (inp, fmt='(a)', end=170) buf
                card = buf(1:1)
                last = lastch (buf)
                do while (buf(last:last) .eq. '-') 
                  write (errbuf(1), 160) buf(1:80)
                  call prterx ('W', 1)
                  read (inp, fmt='(a)', end=170) buf
                  card = buf(1:1)
                  last = lastch (buf)
                enddo
C
C               Examine next input record
C
                do while (card .eq. '.')
                  read (inp, fmt='(a)', end=170) buf
                  card = buf(1:1)
                enddo
              enddo
              go to 180
 
  170         buf = '(STOP) SOLTON'
              card = buf(1:1)
 
  180         call ctlpow
              finished = .true.
            else
              call prtprm (xvalue, yvalue, error)

              if (typprm(1)(1:2) .eq. '%P' .or. 
     &            typprm(1)(1:2) .eq. '%Q') then
                if (index ('X ', typprm(1)(3:3)) .ne. 0) then   
                  xpoint = typprm(1)(1:2)  
                  ypoint = typprm(2)(1:1)  
                else
                  ypoint = typprm(1)(1:2)  
                  xpoint = typprm(2)(1:1)  
                endif   
              else if (typprm(2)(1:1) .eq. 'Q') then 
                if (index ('X ', typprm(2)(2:2)) .ne. 0) then   
                  xpoint = typprm(2)(1:1)  
                  ypoint = typprm(1)(1:1)  
                else
                  ypoint = typprm(2)(1:1)  
                  xpoint = typprm(1)(1:1)  
                endif   
              else   
                if (index ('X ', typprm(2)(2:2)) .ne. 0) then   
                  xpoint = typprm(2)(1:1)  
                  ypoint = typprm(1)(1:1)  
                else
                  ypoint = typprm(2)(1:1)  
                  xpoint = typprm(1)(1:1)  
                endif   
              endif
              basekvc = code (base(nb_prm), 4, 0)
              write (stext, 182) bus(nb_prm), basekvc, xpoint, xvalue, 
     &           ypoint, yvalue
  182         format ('B', t6, a8, a4, 1x, a, ' = ', f17.10, 1x, a, 
     &           ' = ', f17.10)
              length = apdoutbuf( o2, stext, out_buffer(o2:) )
              o2 = o2 + length

              call savsln
C
C             Write user-specified report if requested.
C
              if (numusr .gt. 0) then
                call postsl
                update(1) = ickzone
                call updzon()
                call prtusr (lun, numupx)
              endif
            endif

            if (firstxstr (buf,'CHANGE_PAR') .eq. 0) finished = .true.
          enddo
          call chgorder('opt2inp')             !  EXT2IN
C
C         Summarize CHANGE_PARAMETER file
C
          if (numprm .gt. 0) then
            inquire (unit=23, name=prmfil)
            l = lastch (prmfil)
            write (outbuf, 190) numprm, prmfil(1:l)
  190       format ('0 ', i4, ' points written to file ', a)
            call prtout (1)
          endif
C
        else
c
c         Solution rejected - advance control cards
c
          if (buf(1:1) .eq. '/') then
            read (inp, fmt='(a)', end=210) buf   
            do while (buf(1:1) .ne. '/')
              read (inp, fmt='(a)', end=210) buf   
            enddo
            go to 220
  210       buf = '( end ) solution' 
  220       continue
          endif
        endif
c
c       Append error messages to buffer
c
 900    j = 1 
        length = 1
        do while ( j .le. numerr  .and.  length .gt. 0 )
          length = apdoutbuf( o2, errm(j), out_buffer(o2:) )
          o2 = o2 + length
          j = j + 1
        enddo
c
c 	Append summary
c
        if ( o2 .gt. max_buf ) then
          o2 = max_buf
          do while ( o2 .gt. 1  .and.
     &               out_buffer(o2:o2) .ne. linefeed )
            o2 = o2 - 1
          enddo
          out_buffer(o2:o2) = null
        endif

        write (stext, 910) 'p_chgpar.f', p_chgpar, ostates
  910   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf( o2, stext, out_buffer(o2:) )
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt( 0, ' ' )

        ownflg = .false. ! Flag OWNLOS as being obsolete
        update(1) = 1    ! Flag ZSUM as being obsolete
c
c       Restore all pagination variables.
c
        if (lprtx .ne. 0) then
          call close_file (lprt)
          lprt = lprtx
          lprtsw = lprtswx
          pageno = pagenox
          lineno = linenox
          fichpg = fichpgx
          fichln = fichlnx
          lstdnl = lstdnlx
          lstdnf = lstdnfx
        endif

        return
	end

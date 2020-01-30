C    @(#)get_orpt17.f	20.8 1/7/99
C****************************************************************
C
C       File: get_orpt17.f
C       Purpose: Routine to generate User Analysis output 
C                reports using IPF command
C
C                / REPORTS, SELECT USER_ANALYSIS
C                  FILE = <filename>,
C                  OUTPUT_FILE = <filename>
C                (END)
C
C       Author: Walt Powell  Date: 27 July 1994
C                            Modified: 27 July 1994
C       Called by:
C
C****************************************************************
C
	subroutine get_orpt17 (scrfil)
        integer scrfil

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/blank.inc'
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/dtaiop.inc'
	include 'ipfinc/ecvar.inc'
	include 'ipfinc/bus.inc'
	include 'ipfinc/arcntl.inc'
        include 'ipfinc/ordsta.inc'
	include 'ipfinc/ownhash.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/update.inc'
        include 'ipfinc/anlys.inc'
        include 'ipfinc/busanl.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/pageno.inc'
        include 'ipfinc/prt.inc'

        common /ownflg/ ownflg 
        logical ownflg         

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null*1, linefeed*1, capital*1, text*10, 
     &            filename*60, query*1, temp*10
	integer apdoutbuf, o1, o2, findstr, status, usranlrpt,
     &          tempfile, open_file, pagenox, linenox, fichpgx, 
     &          fichlnx, lstdnlx, lstdnfx
        logical finished, is_loaded, no_more

        null = char(0)
        linefeed = char(10)
        linespage = 22
        call getenvir  ('NO_MORE', temp)
        no_more = (temp(1:7) .eq. 'NO_MORE')
C
C	Get input file name
C
        in_buffer(1:1) = null
        i1 = index (in_buffer, null)
        out_buffer(1:1) = null
        length = apdoutbuf(i1, '/REPORTS, SELECT USER_ANALYSIS',
     &                     in_buffer(i1:))
        i1 = i1 + length

        if (ntotx .eq. 0) then 
           write (*, 70)
   70      format (' * Rebuilding solution arrays. This will take a minu
     &te.')
c
c          Temporarily divert all lprt output to scratch file. Store
c          pagination variables.
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
           buf = '(end)'
           card = buf(1:1)
           if (ordtbx .ne. 2) then
              ordymx = 2     ! Set flag to not reorder y-matrix
              call chgorder ('inp2opt')
           endif
           call presln ()
           iopton(18) = 0    ! Set flat start off
           kspare(24) = 1    ! Do not reinitialize XDATA
           call opsln1 ()
           iopton(1) = 0     ! Set number of decoupled iterations = 0
           call dcstrt (kerr)
           call nrcnvt ()
c
c          Now restore lprt
c
           call close_file (lprt)
c
c          Temporarily divert all lprt output to scratch file. Store
c          pagination variables.
c
           lprt = lprtx
           lprtsw = lprtswx
           pageno = pagenox
           lineno = linenox
           fichpg = fichpgx
           fichln = fichlnx
           lstdnl = lstdnlx
           lstdnf = lstdnfx
        endif

        do while (.true.)
           is_loaded = .false.
           do while (.not. is_loaded)
              write(*, 80)
   80         format(' > Enter USER_ANALYSIS file name or <RETURN> to qu
     &it: ')
	      read (*, 90) filename
   90	      format (a)

              if (filename .eq. ' ' .or. filename(1:1) .eq. null) 
     &           go to 900
              if (.not. is_loaded) then
                 tempfile = 20           
                 status = open_file (tempfile, filename, 'F', 'R', ierr)
                 if (status .ne. 0) then
                    last = lastch(filename)
                    write (*, 94) filename(1:last)
   94               format(' File ', a, ' cannot be opened ')
                 else
                    is_loaded = .true.
                    call close_file (tempfile)
c
c                   Append "FILE = " to in_buffer
c
                    i1 = findstr (in_buffer, 'USER_ANALYSIS')
                    if (i1 .gt. 0) then
                       i1 = i1 + len ('USER_ANALYSIS')
                    else
                       i1 = findstr (in_buffer, linefeed // '(END)')
                    endif
                    write(*, 96)
   96               format (' > Append Case Comments (Y or N?) > ')
	            read (*, 90) query
                    if (index ('Yy', query) .ne. 0) then
c
c                      Append "APPEND_COMMENTS" to in_buffer
c
                       length = apdoutbuf(i1, ' APPEND_COMMENTS',
     &                                    in_buffer(i1:))
                       i1 = i1 + length
                    endif
                    if (i1 .eq. 0) i1 = 1
                    in_buffer(i1:i1) = null
                    last = lastch(filename)
                    length = apdoutbuf(i1, 
     &                  ' FILE = ' // filename(1:last),
     &                  in_buffer(i1:))
                    i1 = i1 + length
                 endif
              endif
           enddo
           in_buffer(i1:i1) = null
           out_buffer(1:1) = null
           status = usranlrpt(in_buffer, out_buffer, scrfil)
           o1 = 1
           numlin = 1
           finished = .false.
           do while (.not. finished)
              do while (o1 .lt. MAXBUFFER .and. 
     &                  out_buffer(o1:o1) .ne. null)
                 o2 = nxt_term(out_buffer(o1+1:)) + o1
                 if (out_buffer(o1:o1) .eq. '/') then
                    o2 = MAXBUFFER + 1
                 else 
                    if (out_buffer(o1:o1+6) .eq. '*[MORE]' .and.
     &                 .not. no_more) then
                       in_buffer(1:1) = null
                       i1 = index (in_buffer, null)
                       length = apdoutbuf (i1, 
     &                   '/REPORTS, SELECT USER_ANALYSIS CONTINUE',
     &                   in_buffer(i1:))
                       i1 = i1 + length
                       out_buffer(1:1) = null
                       status = usranlrpt(in_buffer, out_buffer, scrfil)
                       o1 = 1
                       o2 = nxt_term(out_buffer(o1+1:)) + o1
                    endif
                    if (no_more) then
                       finished = .true.
                       go to 130
                    else if (numlin .ge. linespage+1) then
                       numlin = 0
                       write(*, 100)
  100                  format (' --More-- ')
                       read (*, 120) text(1:1)
  120                  format (a)
                       text(1:1) = capital (text(1:1))
                       if (text(1:1) .eq. 'Q') then
                          finished = .true.
                          go to 130
                       endif
                    endif
                    numlin = numlin + 1
                    write (*, 110) out_buffer(o1:o2-1)
  110               format (1x, a)
                 endif
                 o1 = o2 
                 if (out_buffer(o1:o1) .eq. linefeed) 
     &              o1 = o1 + 1
              enddo
  130         finished = .true.
           enddo
        enddo
  900   return
        end

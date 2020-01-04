C    @(#)p_svfile.f	20.8 8/20/98
C****************************************************************
C
C   File: p_svfile.f
C   Purpose: IPF shell program to process the following set of commands
c
c      /SAVE_FILE, TYPE = NEW_BASE, FILE = <file_name>
c      /SAVE_FILE, TYPE = NETWORK_DATA, FILE = <file_name>
c      /SAVE_FILE, TYPE = CHANGES, FILE = <file_name>
c      /SAVE_FILE, TYPE = WSCC_BINARY_STABILITY, FILE = <file_name>
c      /SAVE_FILE, TYPE = WSCC_ASCII_STABILITY, FILE = <file_name>
C
C   Author: Walt Powell  Date: 14 January 1993
C
C   Called by: 
C
C****************************************************************
C
	integer function p_svfile (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'

        character null * 1, linefeed * 1, capital * 80, temp_buf * 512,
     &            text * 80, word(30) * 80, filename * 80,
     &            size * 10, ratings * 10, dialects * 10, type * 10,
     &            sections * 12
        integer o2, savewscc, savenetd, savechgs, scrfil, error,
     &          open_file, apdoutbuf
        external savewscc, savenetd, savechgs

        p_svfile = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
c
c	Parse until in_buffer LINEFEED // C  (for trailing comment text)
c
        i1 = 1
        i2 = index (in_buffer, linefeed // 'C')
        i3 = index (in_buffer, null)
        if (i2 .eq. 0) i2 = len (in_buffer)
        if (i3 .eq. 0) i3 = len (in_buffer)
        i2 = min0 (i2, i3)

        temp_buf = in_buffer(i1:i2)
        call uscan (temp_buf, word, numwrd, '=',
     &              ', ' // linefeed // null )
c
c       Capitalize all word() except <filename>
c
c        iflag = 0
c        do i = 1, numwrd
c           if (iflag .ne. 1 .and. word(i) .ne. '=') then
c              word(i) = capital(word(i))
c           endif
c           if (iflag .eq. 0 .and. word(i) .eq. 'FILE') then
c              iflag = 1
c           else if (iflag .eq. 1 .and. word(i) .ne. '=') then
c              iflag = 2
c           endif
c        enddo
        i = 1
        do while ( i .le. numwrd )
           if ( word(i) .ne. '=' ) word(i) = capital(word(i))
           if ( word(i) .eq. 'FILE' ) i = i + 2
           i = i + 1
        end do

	iwrd = 2
        do while (iwrd .le. numwrd)
           if (word(iwrd)(1:4) .eq. 'TYPE') then
              iwrd = iwrd + 1
              if (word(iwrd) .eq. '=') iwrd = iwrd + 1
              if (word(iwrd)(1:8) .eq. 'NEW_BASE') then
c
c                Process /SAVE_FILES, TYPE = NEW_BASE, 
c                                     FILE = <filename>,
c                                     CASE = <casename>
C                        C    <case comments>
C                        C    <case comments>
C                        C    <case comments>
c
                 if (ostates .lt. 5) then
                    p_svfile = 1
                    write (errbuf(1), 148)
  148               format(' Unsolved base data in residence')
                    call prterx ('W', 1)
                 else
                    call ctlpow
                    buf = inrcd
                    if ( kspare(14) .eq. 1 ) then 
c
c                      DATAO file opened
c
                       call wrdtao
                       call prtime('WRITE_NEWBASE')
                       if ( ostates .le. 6 ) then
                          ostates = 6
                       else
                          ostates = 8
                       endif
                    else
                       p_svfile = 1
                    endif
                    call close_file (datao)
                    call loadarcv
                    iwrd = numwrd + 1
                 endif

              else if (word(iwrd)(1:11) .eq. 'WSCC_BINARY' .or. 
     &                 word(iwrd)(1:10) .eq. 'WSCC_ASCII') then
c
C                Process / SAVE_FILE, TYPE = WSCC_BINARY_STABILITY,
c                                     FILE = <filename>
C                or
c
C                Process / SAVE_FILE, TYPE = WSCC_ASCII_STABILITY,
c                                     FILE = <filename>
c
                 type = word(iwrd)
                 if (ostates .lt. 5) then
                    p_svfile = 1
                    write (errbuf(1), 148)
                    call prterx ('W', 1)
                 else
                    filename = ' '
                    iwrd = iwrd + 1
                    do while (iwrd .le. numwrd)
                       if (word(iwrd)(1:4) .eq. 'FILE') then
                          iwrd = iwrd + 1
                          if (word(iwrd) .eq. '=') iwrd = iwrd + 1
                          filename = word(iwrd)
                       endif
                       iwrd = iwrd + 1
                    enddo

                    scrfil = wscfil
                    call close_file (scrfil)
                    if (type(1:10) .eq. 'WSCC_ASCII') then
                       p_svfile = open_file ( scrfil, filename, 'F',
     &                                        'W', iostat )
                       if (p_svfile .eq. 0) then
                          p_svfile = savewscc ( scrfil, 'ASCII' )
                       else 
                          go to 160
                       endif
                    else
                       p_svfile = open_file ( scrfil, filename, 'U',
     &                                        'W', iostat )
                       if (p_svfile .eq. 0) then
                          p_svfile = savewscc ( scrfil, 'BINARY' )
                       else 
                          go to 160
                       endif
                    endif
                    if (p_svfile .gt. 0) then
                       errbuf(1) =
     &                        ' Errors or incompatible data in case.'
                       errbuf(2) = ' WSCC Stability file is invalid!'
                       errbuf(3) = ' '
                       call prterx ('W', 3)
                    endif
                    call close_file (scrfil)
                    call prtime('WRITE_WSCC')
                    call loadarcv
c***                buf = inrcd
                    buf = '(END)'
                    iwrd = numwrd + 1
                 endif

              else if (word(iwrd)(1:12) .eq. 'NETWORK_DATA') then
c
c                Process /SAVE_FILES, TYPE = NETWORK_DATA, 
c                                     FILE = <filename>,
c                                     DIALECTS = BPA,
c                                               WSCC,
c                                               WSCC1,
c                                               PTI
c                                     SIZE = 120,
C                                            80
c                                     RATINGS = EXTENDED,
c                                               MINIMUM,
C                                               NOMINAL,
c                                     SECTIONS = <null>
c                                               PSEUDOBUSES,
C                                               NOMINAL,
c                                     IPS_REF  = <null>,
c                                                <filename>,
c                                     ZONE_REN = <null>,
c                                                <filename>,
c
                 filename = ' '
                 dialects = 'BPA'
                 size = '120'
                 ratings = 'EXTENDED'
                 sections = ' '

                 iwrd = iwrd + 1
                 do while (iwrd .le. numwrd)
                    if (word(iwrd)(1:4) .eq. 'FILE') then
                       iwrd = iwrd + 1
                       if (word(iwrd) .eq. '=') iwrd = iwrd + 1
                       filename = word(iwrd)
                    else if (word(iwrd)(1:7) .eq. 'DIALECT') then
                       iwrd = iwrd + 1
                       if (word(iwrd) .eq. '=') iwrd = iwrd + 1
                       dialects = word(iwrd)
                    else if (word(iwrd)(1:4) .eq. 'SIZE') then
                       iwrd = iwrd + 1
                       if (word(iwrd) .eq. '=') iwrd = iwrd + 1
                       size = word(iwrd)
                    else if (word(iwrd)(1:6) .eq. 'RATING') then
                       iwrd = iwrd + 1
                       if (word(iwrd) .eq. '=') iwrd = iwrd + 1
                       ratings = word(iwrd)
                    else if (word(iwrd)(1:6) .eq. 'RATING') then
                       iwrd = iwrd + 1
                       if (word(iwrd) .eq. '=') iwrd = iwrd + 1
                       sections = word(iwrd)
                    endif
                    iwrd = iwrd + 1
                 enddo

                 scrfil = svchfl
                 newnetfil = filename
                 p_svfile = open_file (scrfil, filename, 'F', 'W', 
     &                                 iostat)
                 if (p_svfile .eq. 0) then
                    p_svfile = savenetd (scrfil, filename, dialects, 
     &                                   size, ratings, sections, 0,
     &                                   ' ', 0, ' ', 0, ' ')
                 else 
                    go to 160
                 endif
                 call close_file (scrfil)
                 call prtime('SAVE_NETWORK_DATA')
c***             buf = inrcd
                 buf = '(END)'
                 call loadarcv
                 iwrd = numwrd + 1

              else if (word(iwrd)(1:7) .eq. 'CHANGES') then
c
c                Process /SAVE_FILES, TYPE = CHANGES
c
                 filename = ' '
                 iwrd = iwrd + 1
                 do while (iwrd .le. numwrd)
                    if (word(iwrd)(1:4) .eq. 'FILE') then
                       iwrd = iwrd + 1
                       if (word(iwrd) .eq. '=') iwrd = iwrd + 1
                       filename = word(iwrd)
                    endif
                    iwrd = iwrd + 1
                 enddo

                 scrfil = svchfl
                 newchgfil = filename
                 p_svfile = open_file (scrfil, filename, 'F', 'W', 
     &                                 iostat)
                 if (p_svfile .eq. 0) then
                    p_svfile = savechgs (scrfil, filename)
                 else 
                    go to 160
                 endif
                 call close_file(scrfil)
                 call prtime('SAVE_CHANGES')
c***             buf = inrcd
                 buf = '(END)'
                 call loadarcv
                 iwrd = numwrd + 1

              else
                 last = lastch (word(iwrd))
                 write (errbuf(1), 150) word(iwrd)(1:last)
  150            format(' Unrecognized /SAVE_FILE, TYPE = (', a, ')')
                 call prterx ('W', 1)
                 iwrd = iwrd + 1
              endif
              iwrd = numwrd + 1
           endif
           iwrd = iwrd + 1
        enddo
        go to 200

  160   last = lastch (filename)
        if (p_svfile .eq. 1) then
           write (errbuf(1), 170) filename(1:last)
  170      format(' file ', a, ' has write protection--request ignored')
           call prterx ('W',1)
           p_svfile = 1
        else if (p_svfile .eq. 2) then
           write (errbuf(1), 180) filename(1:last)
  180      format(' file ', a, ' could not be opened--request iognored')
           call prterx ('W',1)
           p_svfile = 1
        else
           write (errbuf(1), 190) filename(1:last), iostat
  190      format(' failure on opening file ',a,' error code =', i2)
           call prterx ('W',1)
           p_svfile = 1
        endif
        buf = '(END)'

  200   continue

        inrcd = buf
        o2 = index (out_buffer,null)
c
c       Append error messages to buffer
c
        j = 1
        length = 1
        o2 = index (out_buffer,null)
        do while (j .le. numerr .and. length .gt. 0)
           length = apdoutbuf(o2, errm(j), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo

        write (text, 340) 'p_svfile.f', p_svfile, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end

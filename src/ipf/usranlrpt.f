C    @(#)usranlrpt.f	20.10 1/7/99
C****************************************************************
C
C   File: usranlrpt.f
C
C   Purpose: Routine to obtain a user analysis report.
C
C   Author: Walt Powell  Date: 15 July 1994
C   Called by: p_report.f
C
C****************************************************************
C
        integer function usranlrpt (in_buffer, out_buffer, scrfil)
        integer scrfil

        include 'ipfinc/parametr.inc'

        character in_buffer * (MAXBUFFER)
        character out_buffer * (MAXBUFFER)

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/pageno.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/anlys.inc'
        include 'ipfinc/usranl.inc'
        include 'ipfinc/busanl.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/zbo.inc'
        include 'ipfinc/coment.inc'

	character capital * 80, null * 1, linefeed * 1, header(1) * 80, 
     &            text * 80, filename * 60, word(20) * 60, 
     &            ofilename * 60,
     &            symnam(1000) * 12, subdef * 132, defchr(100) * 40
        logical found, finished, repeat, comments
        integer apdoutbuf, findex, tempfile, status, findstr,
     &          symind(1000), ndefch(1000), first, open_file, o2,
     &          scrfilx
        real symval(1000)

        save

        data ofilename / ' ' /

        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        maxbuf_out = len( out_buffer ) - 400

        usranlrpt = 0
c
c       Check for re-entry and continue
c
        last = index( in_buffer, null )
        ix = last
        if (findstr (in_buffer(1:ix), 'CONTINUE') .ne. 0) then
           repeat = .true.
           scrfilx = 0
           lastloop = loop
           go to 170
        endif

        repeat = .false.
        scrfilx = scrfil
        lastloop = 0

        call updzon()

        ix = 1
        i1 = 1
        i2 = index (in_buffer, null)
        call uscan (in_buffer(i1:i2), word, nwrd, '=', ', ' // linefeed)
        comments = .false.
        do iwrd = 1, nwrd
           if (capital(word(iwrd)) .eq. 'APPEND_COMMENTS') 
     &        comments = .true.
        enddo
        iwrd = 1
        do while (iwrd .lt. nwrd .and. 
     &            (capital(word(iwrd)) .ne. 'FILE'))
           iwrd = iwrd + 1
        enddo
        if (iwrd .ge. nwrd .and. 
     &     (ofilename .eq. ' ' .or. ofilename(1:1) .eq. null)) then
           write (errbuf(1), 90)
   90      format(' No User Analysis file loaded ')
           call prterx ('W', 1)
           usranlrpt = 1
           filename = ' '
           go to 900
        else if (capital(word(iwrd)) .ne. 'FILE') then
           filename = ofilename
           go to 110
        endif
        iwrd = iwrd + 1
        if (word(iwrd) .eq. '=') iwrd = iwrd + 1
        filename = word(iwrd)
        tempfile = 20           
        status = open_file (tempfile, filename, 'F', 'R', ierr)
        if (status .ne. 0) then
           write (errbuf(1), 92)
   92      format(' User Analysis file cannot be opened ')
           write (errbuf(2), 94) filename
   94      format(' File ', a)
           call prterx ('W', 2)
           usranlrpt = 1
           go to 900
        endif

  110   o2 = index (out_buffer,null)
C
C	Read DEFINE/COMMENT file
C
	call read_user (tempfile)
C
C       Get symbol definations
C
        write (*, 138)
  138   format (' * Computing USER_ANALYSIS tokens. This will take a min
     &ute.')

        call getdef (numdef(numusr), usrdef, numsym, symnam, symval, 
     &               symind, nchr, ndefch, defchr)

  140   if (numdef(numusr) .gt. 0) then

c          Set up the page header

           if (scrfilx. gt. 0) then


              outbuf = 'User Analysis Report'
              call rpnlod
    
              write (outbuf, 142) chase1(1), chase1(34), chase1(35)
  142         format('Case: ', a10, ' Project: ', 2a10)
              call hedlod

              outbuf = ' '
              call shdlod(1)
              call shdlod(2)
              call shdlod(3)
              call shdlod(4)
              call shdlod(5)
              call comlod(1)
              call comlod(2)
              call forbtm()
              call fortop()

              if (comments) then
                 do i = 1, ncom
                    outbuf = com(i)
                    call prtout (1)
                 enddo
              endif

           endif

        endif

  170   ix = lastloop
        if (ix .eq. 0) ix = 1
        loop = ix

        finished = .false.
        do while (ix .le. numtxt(numusr) .and. .not. finished)
C
C          Substitute symbols in comment text
C
           if (usrtxt(ix)(1:1) .eq. 'C') then
              outbuf = subdef (usrtxt(ix)(2:), numsym, symnam, symval, 
     1                         symind, nchr, ndefch, defchr)
           else
              outbuf = usrtxt(ix)
           endif
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:80), out_buffer(o2:))
              o2 = o2 + length
              loop = ix
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
           ix = ix + 1
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 190) linefeed, null
  190      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end

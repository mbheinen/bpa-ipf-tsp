C    @(#)p_subdef.f	20.7 1/4/99
C****************************************************************
C
C       File: p_subdef.f
C       Purpose: This program process the following command
c       
c         / GET_DATA, TYPE = SUB_DEFINE, SOURCE = BASE
c                                                 ALTERNATEBASE
c       or
c         / GET_DATA, TYPE = SUB_DEFINE, CONTINUE
C
c       Input: 
c          in_buffer - the above command line
c          (LUNUSR)  - previously loaded >DEFINE... and C... records
c       Output: 
c          out_buffer - all C... records with run-time substitutions
C
C       Author: Walt Powell  Date: 21 July 1992 
C                           Modified: 21 July 1992
C       Called by: 
C
C****************************************************************
C
	integer function p_subdef (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/usranl.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/update.inc'

        common /ownflg/ ownflg
        logical ownflg

        character symnam(1000) * 12, subdef * 132, defchr(100) * 40,
     &            null * 1, linefeed * 1, 
     &            text * 80, source * 10, word(10) * 20, capital * 20
        real symval(1000)
        integer symind(1000), ndefch(1000), first, apdoutbuf, o2,
     &          findstr
        logical repeat, finished
C
        save

        null = char(0)
        linefeed = char(10)
        p_subdef = 0  ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        out_buffer(1:1) = null
        maxbuf_out = len( out_buffer ) - 400
c
c       Check for re-entry and continue
c
        last = index( in_buffer, null )
        ix = last
        if (findstr (in_buffer(1:ix), 'CONTINUE') .ne. 0) then
           repeat = .true.
           lastloop = loop
           go to 170
        endif

        repeat = .false.
        lastloop = 0

        first = 1
        last = index (in_buffer, null)
        if (last .eq. 0) last = len (in_buffer)

        next = nxt_term (in_buffer(first+1:)) + first
        text = in_buffer(1:next-1)

        source = 'BASE' ! Default SOURCE = BASE
        call uscan (text, word, nwrd, '=', ', ' // linefeed)
        iwrd = 1
        do while (iwrd .lt. nwrd)
           if (capital(word(iwrd)) .eq. 'SOURCE') then
              iwrd = iwrd + 1
              if (word(iwrd) .eq. '=') iwrd = iwrd + 1
              if (capital(word(iwrd)(1:3)) .eq. 'ALT') source = 'ALT'
              iwrd = nwrd
           endif
           iwrd = iwrd + 1
        enddo

        rewind lunusr
        call updzon()
        
        rewind lunusr
        ndef = numdef(1)
        do first  = 1, ndef, 200
           last = min0 (first+199,ndef)
           read (lunusr) (usrdef(i),i=first,last)
        enddo
        ntex = numtxt(1)
        do first = 1, ntex, 200
           last = min0 (first+199,ntex)
           read (lunusr) (usrtxt(i),i=first,last)
        enddo

        if (source .eq. 'BASE') then
           call getdef (numdef(1), usrdef, numsym, symnam, symval,
     1                  symind, nchr, ndefch, defchr)
        else
           call ogetdef (numdef(1), usrdef, numsym, symnam, symval,
     1                   symind, nchr, ndefch, defchr)
        endif 

  170   ir = lastloop
        if (ir .eq. 0) ir = 1
        loop = ir

        o2 = index (out_buffer,null)

        finished = .false.
        do while (ir .le. numtxt(1) .and. .not. finished)
C                                                                      *
C          Process only "C" records.                                   *
C                                                                      *
           if (usrtxt(ir)(1:1) .eq. 'C' .or.
     &         usrtxt(ir)(1:1) .eq. 'c') then
C                                                                      *
C             Substitute any symbols for their numerical value.        *
C                                                                      *
              text = subdef (usrtxt(ir)(2:), numsym, symnam, symval,
     1                       symind, nchr, ndefch, defchr)
c             write (*, 100) text(1:80)
  100         format (' p_subdef: ', a)
              if (o2 .lt. maxbuf_out) then
                length = apdoutbuf(o2, text, out_buffer(o2:))
                o2 = o2 + length
                loop = ir
              else if (repeat) then
                finished = .true.
              endif
           endif
           ir = ir + 1
        enddo

c***    remember maxbuf_out is really 400 less than the real buffer size

        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 190) linefeed, null
  190      format (a, '*[EOM]', a)
           o2 = o2 + 8
        endif
c
c       Append error messages to buffer
c
        j = 1 
        length = 1
        do while (j .le. numerr .and. length .gt. 0 .and. 
     &            o2 .lt. maxbuf_out)
           length = apdoutbuf(o2, errm(j), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
c
c 	Append summary
c
        write (text, 340) 'p_subdef.f', p_subdef, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
  900   call setercnt (0, ' ')
        return
	end

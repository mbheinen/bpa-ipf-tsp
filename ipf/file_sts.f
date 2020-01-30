C    @(#)file_sts.f	20.3 11/11/97
C****************************************************************
C
C   File: file_sts.f
C   Purpose: Routine to process /GET_DATA, TYPE=FILE_WRT commands
C            Returned value: 
C
C            status = 0 : file is writable or does not exsit
C                     1 : file exists and is not writable
C
C   Author: Walt Powell  Date: 10 Nov 1992
C   Called by: p_gtdata.f
C   Re-written by Jay Coleman  30-SEP-93
C
C****************************************************************
        integer function file_sts (in_buffer, out_buffer)
        character in_buffer *(*)
        character out_buffer *(*)

        include 'ipfinc/prt.inc'
        include 'ipfinc/miscfile.inc'

        integer index, lastch

        character word(20)*80, capital*80, null*1, linefeed*1,
     &            filename*80, formatted*10, form*1
        logical yes, found
        integer o2, iwrd, nwrd, last, status

        null = char(0)
        linefeed = char(10)
        file_sts = 0            ! Initialize to "does not exist"
        out_buffer(1:1) = null
        yes = .false.
        last = index (in_buffer, null)
        if (last .eq. 0) last = 120

        call uscan (in_buffer(1:last), word, nwrd, '=' ,
     &              ', ' // linefeed // null )

c       Align "iwrd" after "FILE ="

        iwrd = 1
        do while (iwrd .le. nwrd)
           word(iwrd) = capital(word(iwrd))
           if (word(iwrd)(1:5) .eq. 'FILE '  .and.
     &         word(iwrd+1) .eq. '='               ) then
              iwrd = iwrd + 2
              filename = word(iwrd)
              last = lastch (filename)
              inquire (file = filename(1:last), exist = yes, 
     &                 formatted = formatted)
              if (formatted .eq. 'FORMATTED') then
                 form = 'F'
              else 
                 form = 'U'
              endif
              if (yes) then
                 call close_file(lunscr1)
                 status = open_file (lunscr1, filename(1:last), form, 
     &                               'U', iostat)
                 if (iostat .eq. 0) then
                    file_sts = 0   ! File exists and is writable
                 else
                    file_sts = 1   ! File exists and is not writable
                 endif
                 call close_file(lunscr1)
                 return
              endif
              return
           endif
           iwrd = iwrd + 1
        enddo

        write (errbuf(1), 100)
  100   format (' Command parsing error, missing "FILE =" keyword ')
        call prterx ('W', 1)
        return 
        end

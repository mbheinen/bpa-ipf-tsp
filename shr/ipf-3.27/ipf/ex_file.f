C    @(#)ex_file.f	20.3 2/13/96
C****************************************************************
C
C   File: ex_file.f
C   Purpose: Routine to inquire of a file's status:
C            Returned value: status = 0 : file exists 
C                                     1 : file does not exist
C
C   Author: Walt Powell  Date: 10 Nov 1992
C   Called by: p_gtdata.f
C   Re-written by Jay Coleman  30-SEP-93
C
C****************************************************************
        integer function ex_file (in_buffer, out_buffer)
        character in_buffer *(*)
        character out_buffer *(*)

        include 'ipfinc/prt.inc'

        integer index, lastch

        character word(20) * 80, capital * 80,
     &            null * 1, linefeed * 1,
     &            filename * 80
        logical yes, found
        integer o2, iwrd, nwrd, last

        null = char(0)
        linefeed = char(10)
c*** initialize to "file not found"
        ex_file = 1
        out_buffer(1:1) = null
        yes = .false.
        last = index (in_buffer, null)
        if (last .eq. 0) last = 120

        call uscan (in_buffer(1:last), word, nwrd, '=' ,
     &              ', ' // linefeed // null )

c***  Align "iwrd" after "FILE ="

        iwrd = 1
        do while (iwrd .le. nwrd)
           word(iwrd) = capital(word(iwrd))
           if (word(iwrd)(1:5) .eq. 'FILE '  .and.
     &         word(iwrd+1) .eq. '='               ) then
              iwrd = iwrd + 2
              filename = word(iwrd)
              last = lastch (filename)
              inquire (file = filename(1:last), exist = yes)
              if (yes) then
                 ex_file = 0
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

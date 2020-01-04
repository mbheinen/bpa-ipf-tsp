C    @(#)prnt_fltr.f	20.3 2/13/96
C****************************************************************
C
C       File: prnt_fltr.f
C       Purpose: Routine to print out current filters
C
C       Invoked by:
C              / REPORTS, SELECT OWNER_INT_DETAIL
C              / REPORTS, SELECT OWNER_INT_BRIEF
C                  WHERE AREAS = ...
C                (END)
C          
C       Author: Walt Powell  Date: 17 Feb 1995
C                            Modified: 17 Feb 1995
C       Called by:
C
C****************************************************************
	integer function prnt_fltr (in_buffer)

        character in_buffer * (*)

        include 'ipfinc/parametr.inc'
  
        include 'ipfinc/blank.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/lfiles.inc'

        character capital*40, word(50) * 40, linefeed * 1, null * 1
        integer status
        
        null = char(0)
        linefeed = char(10)
        prnt_fltr = 0
c
c       Parse filters
c
        last = index (in_buffer, null) - 1
        if (last .le. 0) last = len (in_buffer)
        call uscan (in_buffer(1:last), word, numwrd, '/=' ,
     &              ', ' // linefeed // null )

c	Remove any "..." bracketing word, convert to upper case

        do iwrd = 1, numwrd
           do while (index (word(iwrd), '"') .ne. 0)
              i = index (word(iwrd), '"')
              word(iwrd)(i:) = word(iwrd)(i+1:)
           enddo
           word(iwrd) = capital(word(iwrd))
        enddo

	iwrd = 1
        outbuf = ' '
        do while (iwrd .le. numwrd)
           if (word(iwrd)(1:4) .eq. 'AREA') then
              if (outbuf .ne. ' ') call prtout(1)
              outbuf = ' AREA FILTER: '
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 last1 = lastch (outbuf)
                 last2 = lastch (word(i))
                 if (outbuf(last1:last1) .eq. ':') then
                    outbuf(last1+1:) = ' ' // word(i)
                 else if (last1 + last2 .lt. 132) then
                    outbuf(last1+1:) = ', ' // word(i)
                 else
                    call prtout (1)
                    outbuf = ' '
                    outbuf(15:) = word(i)
                 endif
                 i = i + 1
              enddo
              iwrd = i
           else if (word(iwrd)(1:4) .eq. 'ZONE') then
              if (outbuf .ne. ' ') call prtout(1)
              outbuf = ' ZONE FILTER: '
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 last1 = lastch (outbuf)
                 last2 = lastch (word(i))
                 if (outbuf(last1:last1) .eq. ':') then
                    outbuf(last1+1:) = ' ' // word(i)
                 else if (last1 + last2 .lt. 132) then
                    outbuf(last1+1:) = ', ' // word(i)
                 else
                    call prtout (1)
                    outbuf = ' '
                    outbuf(15:) = word(i)
                 endif
                 i = i + 1
              enddo
              iwrd = i
           else if (word(iwrd)(1:5) .eq. 'OWNER') then
              if (outbuf .ne. ' ') call prtout(1)
              outbuf = ' OWNER FILTER: '
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1

              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 last1 = lastch (outbuf)
                 last2 = lastch (word(i))
                 if (outbuf(last1:last1) .eq. ':') then
                    outbuf(last1+1:) = ' ' // word(i)
                 else if (last1 + last2 .lt. 132) then
                    outbuf(last1+1:) = ', ' // word(i)
                 else
                    call prtout (1)
                    outbuf = ' '
                    outbuf(16:) = word(i)
                 endif
                 i = i + 1
              enddo
              iwrd = i
           else if (word(iwrd)(1:4) .eq. 'BASE') then
              if (outbuf .ne. ' ') call prtout(1)
              outbuf = ' BASE FILTER: '
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 last1 = lastch (outbuf)
                 last2 = lastch (word(i))
                 if (outbuf(last1:last1) .eq. ':') then
                    outbuf(last1+1:) = ' ' // word(i)
                 else if (last1 + last2 .lt. 132) then
                    outbuf(last1+1:) = ', ' // word(i)
                 else
                    call prtout (1)
                    outbuf = ' '
                    outbuf(15:) = word(i)
                 endif
                 i = i + 1
              enddo
              iwrd = i

           else if (word(iwrd)(1:4) .eq. 'TYPE') then
              if (outbuf .ne. ' ') call prtout(1)
              outbuf = ' TYPE FILTER: '
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 last1 = lastch (outbuf)
                 last2 = lastch (word(i))
                 if (outbuf(last1:last1) .eq. ':') then
                    outbuf(last1+1:) = ' ' // word(i)
                 else if (last1 + last2 .lt. 132) then
                    outbuf(last1+1:) = ', ' // word(i)
                 else
                    call prtout (1)
                    outbuf = ' '
                    outbuf(15:) = word(i)
                 endif
                 i = i + 1
              enddo
              iwrd = i
           else
              iwrd = iwrd + 1
           endif
        enddo
        if (outbuf .ne. ' ') call prtout(1)

        return
        end

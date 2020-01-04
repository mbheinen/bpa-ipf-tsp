C    %W% %G%
      subroutine nxtwrd (string,khere,kwbeg,kwend)
      character string *(*)
c
c     Determines the starting and ending character positions in
c     string of the first blank delimited word following the 
c     current position.  If khere == 0, then the next word can begin
c     at position 1.  If there is no next word after khere, then 
c     kwbeg will be set to 0.
c
c     begin     begin     begin     begin     begin     begin
c     Set end to current pos to allow repeated calls after failure

      kwend = kcur
      maxend = len (string)
      kcur = khere
      if (kcur .lt. 1) kcur = 1
      kwbeg = 0

c     No more words if we're already at the end of string

      if (kcur .ge. maxend) then 
        kwbeg = 0
        return
      endif

c     Determine start of next word

      if (khere .lt. 1 .and. (string(1:1) .ne. ' ')) then 

c       will accept new word start in column 1

        kwbeg = 1
      elseif (string(kcur:kcur) .eq. ' ') then

c       khere was at space between words.  Find first blank.

        la = kcur + 1
        do while (string(la:la) .eq. ' ')

c         check if rest of line is blank

          if (la .ge. maxend) then
            kwbeg = 0
            return
          endif
          la = la + 1
        enddo
        kwbeg = la
      else

c       khere was in the middle of a word.  Look for first blank.

        la = kcur + 1
        do while (string(la:la) .ne. ' ')

c         check if word extended to end of string

          if (la .ge. maxend) then 
            kwbeg = 0
            return
          endif
          la = la + 1
        enddo
        kblnk = la

c       At blank after previous word.  Now find start of next word.

        la = kblnk + 1
        do while (string(la:la) .eq. ' ')

c         check if rest of line is blank

          if (la .ge. maxend) then 
            kwbeg = 0
            return
          endif
          la = la + 1
        enddo
        kwbeg = la
      endif

c     Have start of word.  Now find its end.

      la = kwbeg + 1
      last = len(string)
      do while (la .le. last .and. (string(la:la) .ne. ' '))
        if (la .ge. maxend) then
          kwend = maxend
          return
        endif
        la = la + 1
      enddo
      kwend = la - 1 

      return
      end

C    @(#)codex.f	20.3 2/13/96
c**********************************************************************
c*** character function CODEX -- used for base kv to ascii conversion
c***
c*** This function has a sister function called "convert" in
c*** the "gui" library (convert.c) that should be kept
c*** functionally the same as this routine
c***
c*** The fortran function CODE is similar to this function, but does not
c*** force a decimal point.  It is used for conversions other than
c*** base kv conversion.  It has a sister function in the "gui"
c*** library called "reformat" (reformat.c)
c***
c**********************************************************************

      character * (*) function codex (a,m,n)
 
      include 'ipfinc/prt.inc'

      character e * 21, fmt * 10
 
      integer free
C ***
C *** CHECK VALIDIY OF FORMAT SPECIFICATIONS
C ***
      if (m .le. 0 .or. m .gt. 10 .or. n .gt. m .or. n .lt. 0) then
         write (errbuf(1),1000) m, n
 1000    format(' Improper encoding format (programming error)',
     &          ' :FORMAT:','f',i2,'.',i1)
         call prterx ('E',1)
         codex = '**********'
         return
      endif
C ***
C *** SKIP UNDER/OVERFLOW TESTS FOR ZERO'S
C ***
      if (a .eq. 0) then
         codex = ' '
         return
      endif
C ***
C *** ENCODE WORD IN MAXIMUM F21.10 (TEN DIGITS ON EITHER
C *** SIDE OF DECIMAL POINT.
C ***
      write (e,1100) a
 1100 format(f21.10)
C ***
C *** COMPRESS LEADING BLANKS
C ***
      last = 21
 1200 last = last - 1
      if (e(1:1) .eq. ' ') then
         e(1:) = e(2:)
         go to 1200
      endif
C ***
C *** ELIMINATE LEADING "0"
C ***
      if (e(1:1) .eq. '-' .and. e(2:2) .eq. '0') then
         e(2:) = e(3:)
      else if (e(1:1) .eq. '0') then
         e(1:) = e(2:)
      endif
C ***
C *** LOCATE DECIMAL POINT
C ***
      id = index (e,'.') - 1
C ***
C *** COMPUTE AVAILABLE CHARACTERS FOR MANTISSA
C ***
      free = m - n - id
C ***
C *** Two conditions exist for encoding "E":
C ***
C *** 1. Leading blanks appear in encoded word.  Accuracy of
c ***    encoding can be improved by shifting decimal point
c ***    left.
C ***
      if (free .gt. 1) then
         if (abs (a) .ge. 1.0) then
            write (fmt,1300) m+1, m-id-1
 1300       format ('(F',i2,'.',i2,')')
            write (e,fmt,err=1302) a
            goto 1304
 1302       if (m-id-2 .lt. 0) then
               write (fmt,1300) m, 0
            else
               write (fmt,1300) m, m-id-2
            endif
            write (e,fmt) a
 1304       continue
            if (e(1:1) .eq. ' ') then
               e = e(2:)
            endif
         else
            write (fmt,1300) m+2, m-id-1
            write (e,fmt,err=1306) a
            goto 1308
 1306       if (m-id-2 .lt. 0) then
               write (fmt,1300) m+1, 0
            else
               write (fmt,1300) m+1, m-id-2
            endif
            write (e,fmt) a
 1308       continue
            if (e(1:1) .eq. ' ') then
               e = e(2:)
            endif
C ***
C ***       Eliminate leading "0"
C ***
            if (e(1:1) .eq. '-' .and. e(2:2) .eq. '0') then
               e(2:) = e(3:)
            else if (e(1:1) .eq. '0') then
               e(1:) = e(2:)
            endif
         endif
C ***
C *** 2. Overflow occurs in present format.  Check if shifting
c ***    the decimal point to the right suffices.
C ***
      else if (free .lt. 0) then
         if (m-1 .lt. id) then
            write (errbuf(1),1400) a, m, n
 1400       format(' DATA OVERFLOW ',e12.5,' FORMAT: ','F',i2,
     1       '.',i1)
            call prterx ('W',1)
            codex = '**********'
            return
         else
            write (fmt,1300) m+1, m-id-1
            write (e,fmt) a
            if (e(1:1) .eq. ' ') then
               e = e(2:)
            endif
         endif
      endif
      codex = e
      return
      end

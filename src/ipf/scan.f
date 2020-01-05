C    @(#)scan.f	20.6 2/13/96
        subroutine scan (ain, aout, nwrd)
        character    aout(*)*(*)
        character    ain*(*)
 
C       This routine will scan a FREE FIELD CHARACTER STRING BUFFER
C       "AIN", find the words (up to the length of "AOUT" ,
C       truncating beyond that) and put them in array "AOUT".
 
C       AOUT is an array of character strings.  Its diminsions are
C       determined by the calling program and it is important to
C       have it dimensioned big enough because this routine ignors
C       the maximum no of words in AOUT.
 
C       ALL WORDS FOUND WILL BE RETURNED.  THE NUMBER OF WORDS
C       FOUND AND BUILT IN "AOUT" IS RETURNED IN "NWRD".
 
C       EACH WORD RETURNED IN "AOUT" IS LEFT JUSTIFIED.
 
C          WORD DELIMITORS: COMMA ",", BLANK " ", EQUAL SIGN "="
c
c***********************************************************************
c*** Modified for DOS and UNIX: "\" and "/" are directory path characters
c***            SLASH  BACKSLASH  "/"  "\"
c***            LEFT AND RIGHT SQUARE BRACKET  "["  "]"
c***            LEFT AND RIGHT PREN  "("  ")"
c***            GREATER THAN AND LESS THAN  ">"  "<"
c***
c***  Now the above characters are stripped off of the beginning and end,
c***  but are allowed otherwise.
c***********************************************************************
c
C       The following will be ignored and the word squeezed together:
C                UNDERSCORE "_"
C 
C       A blank character can be inserted in the word and not be
C       interpreted as a blank by using the "#".  SCAN will insert
C       the blank in the same position as the word is built.
 
c*************************************************************************
c****** SCAN will stop at a right pren ")","]", "\", "<"  ****************
c******   or end of string.  *********************************************
c***
c***  Now only stops at end of string -- comments no longer supported
c***      on the same line as a command, must use "." comments.
c*************************************************************************

        character * 1   back_slash
 
C                       CLEAR THE OUTPUT ARRAY BEFORE STARTING
 
        nwrd=1
        nchr=0
        lenout = len(aout(1))
        aout(nwrd) = ' '

c*** modified for DOS and UNIX ***
c*************************************************************
c***  mods for UNIX and DOS file path and name compatibility
c***  special handling for  "\" (DOS)  and  "/" (UNIX)
c
c some compilers use '\' as a "C" style escape character
c
        back_slash = char(92)

        last = len( ain )
        do while ( last .gt. 1  .and. 
     &             index( ' )<]\\', ain(last:last) ) .ne. 0 )
           last = last - 1
        end do
        ifirst = 1
        do while ( ifirst .lt. last  .and. 
     &             index( ' (>[/', ain(ifirst:ifirst) ) .ne. 0 )
           ifirst = ifirst + 1
        end do
c*************************************************************

        do 100 i = ifirst, last
 
C*** modified for DOS and UNIX ***
c*************************************************************
c***       ityp = index (' ,=/[(>_])\\<',ain(i:i))
C                         123456789012
           ityp = index (' ,=_',ain(i:i))
c*************************************************************
 
C          FIRST CHECK FOR BRACKETS THAT ARE FOR DIRECTORIES INSTEAD
C          OF COMMANDS
 
C               ADD CHARACTER TO WORD replacing # with 'blank'
 
c*** modified for DOS and UNIX ***
c*************************************************************
c***       if ( ityp .eq. 0
c*** &          .or. (ityp .eq. 5 .and. i .ne. 1)
c*** &          .or. (ityp .eq. 9 .and. ain(1:1) .ne. '[' ) ) then
           if ( ityp .eq. 0 ) then
c*************************************************************
 
              nchr=nchr+1
              if ( nchr .le. lenout ) then
                 if ( ain(i:i) .eq. '#' ) then
                    aout(nwrd)(nchr:nchr) = ' '
                 else
                    aout(nwrd)(nchr:nchr) = ain(i:i)
                 endif
              endif
 
C                       END OF WORD.  FIND NEXT WORD
 
C*** modified for DOS and UNIX ***
c*************************************************************
           else if ( ityp .eq. 4 ) then
c             do nothing (compress out "_")
c***       else if ( ityp .le. 4 ) then
           else if ( ityp .lt. 4 ) then
c*************************************************************
              if ( nchr .ne. 0 ) then
                 nwrd=nwrd+1
                 aout(nwrd) = ' '
                 nchr=0
              endif
 
C                                       End of scan area...
c 
c*** modified for DOS and UNIX ***
c*************************************************************
c***       else if ( ityp .gt. 9 .or.
c*** 1               ( ityp .eq. 9 .and. ain(1:1) .eq. '[' ) ) then
c***          go to 150
c*************************************************************
           endif
 
 100    continue
 
 150    if (nchr .eq. 0) nwrd = nwrd - 1
        return
        end

C    @(#)lastch.f	20.5 1/15/98
        function lastch( text )
        character text *(*)
C
C       FIND the last non_blank character in the RECORD.
C       
        if ( len( text ) .eq. 0 ) then
           lastch = 0       ! null record
        else if ( ichar( text(1:1) ) .eq. 0 ) then
           lastch = 0       ! "C" style null string
        else if ( text .eq. ' ' ) then
           lastch = 1
        else
           l = len( text )
           do i = l,2,-1
           if ( text(i:i) .ne. ' ' ) go to 110
           enddo
           i = 1
110        lastch = i
        endif
        return
        end


        function lastch0 ( text )
        character text *(*)
C
C       FIND the last non_blank character in the RECORD.
C       
        character * 1 null

        null = char(0)
        if ( len(text) .eq. 0 ) then
           lastch0 = 0       ! null record
        else if ( text(1:1) .eq. null ) then
           lastch0 = 0       ! "C" style null string
        else if ( text .eq. ' ' ) then
           lastch0 = 1
        else
           lc = index( text, null ) - 1
           if ( lc .lt. 1 ) then
             lc = len (text)
           else if (text(lc:lc) .eq. char(10)) then
             lc = lc - 1
           endif
           do i = lc, 2, -1
             if ( text(i:i) .ne. ' ' ) go to 110
           enddo
           i = 1
110        lastch0 = i
        endif
        return
        end


        function lastchln( text )
        character *(*) text

        character * 4  ctst

        lc = len( text )
        lastchln = 0
        if ( lc .eq. 0 ) return   ! null record
        if ( text .eq. ' ' ) then
           lastchln = 1
           return
        endif
c***           null        LF           FF          CR
        ctst = char(0) // char(10) // char(12) // char(13)
        i = 1
        do while ( i .lt. lc+1 )
           if ( index( ctst, text(i:i) ) .ne. 0 ) then
              lc = i - 1
              goto 100
           endif
           i = i + 1
        enddo
  100   continue
        do while ( lc .gt. 1  .and.  text(lc:lc) .eq. ' ' )
           lc = lc - 1
        enddo
        lastchln = lc
        return
        end

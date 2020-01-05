C    @(#)getid.f	20.1 11/11/97
C****************************************************************
C
C     File: getid.f
C
C     Purpose: Routine to obtain a unique 1-character id from a 
C              string
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
	character*1 function getid (id)
        character *(*) id

        character tempc*10, capital*10, chr*1
        integer ftn_atoi

        if (id(1:1) .ne. ' ') then
          tempc = capital(id)
        else
          tempc = capital(id(2:))
        endif
        last = lastch (tempc)

        if (last .le. 1) then
          getid = tempc
        else 
          chr = 'A'
          num = 0
          do i = 1, last
            ichr = ichar (tempc(i:i))
            if (ichr .ge. 65 .and. ichr .le. 90) then
              chr = tempc(i:i)
            else
              num = num + ftn_atoi (tempc(i:i))
            endif
          enddo
          getid = char (ichar(chr) + num)
        endif
        return
        end

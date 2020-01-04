C    @(#)firstxstr.f	20.3 2/13/96
	integer function firstxstr (str1, str2)
        character str1*(*), str2*(*)
c
c       This function is a FORTRAN version of a case-insensitive
c       "index" function with a wild card "*" provision in str2)
c
        character capital*1, null*1, temp1*40, temp2*40, cnvtwrd*40
        logical finished

        null = char(0)

        temp1 = cnvtwrd(str1)      ! temp1 converted to u.c. w/o "_"
        len1 = index (temp1, null) - 1
        if (len1 .lt. 0) len1 = lastch(temp1)

        temp2 = cnvtwrd(str2)      ! temp2 converted to u.c. w/o "_"
        len2 = index (temp2, null) - 1
        if (len2 .lt. 0) len2 = lastch(temp2)

        firstxstr = 1       ! default as string matched
        i = 1
        j = 1
        finished = .false.
        do while (j .le. len2 .and. .not. finished)
           if (temp2(j:j) .eq. '*') then
              finished = .true.
           else if (i .gt. len1) then
              finished = .true.
              firstxstr = 0
           else if (temp1(i:i) .ne. temp2(j:j)) then
              finished = .true.
              firstxstr = 0   
           else
              i = i + 1
              j = j + 1
           endif
        enddo
        return
        end

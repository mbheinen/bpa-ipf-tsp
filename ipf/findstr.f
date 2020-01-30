C    @(#)findstr.f	20.3 2/13/96
	integer function findstr (str1, str2)
        character str1*(*), str2*(*)
c
c       This function is a FORTRAN version of a case-insensitive
c       "index" function.
c
        character capital*1, null*1

        null = char(0)

        len1 = index (str1, null) - 1
        if (len1 .lt. 0) len1 = len(str1)
        len2 = index (str2, null) - 1
        if (len2 .lt. 0) len2 = len(str2)

        findstr = 0
        do i = 1, len1-len2+1
           do j = 1, len2
              k = i + j - 1
              if (capital(str1(k:k)) .ne. capital(str2(j:j))) go to 100
           enddo
           findstr = i
           go to 110
  100      continue
        enddo
  110   continue
        return
        end

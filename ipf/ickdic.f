C    @(#)ickdic.f	20.6 7/18/96
       function ickdic(word,dict,ld)
 
C       This FUNCTION checks for the location of a STRING in "WORD"
C       in a dictionary array "DICT" of LD entries.
 
C       The number of characters matched is determined by the minimum of
C                a) the non-blank characters in "WORD"
C                b) the characters in "DICT" to the left of any "*"
C                c) 10 characters.
 
 
C       The position of "WORD" in "DICT" is returned when it is found,
C       ELSE a zero is return when it is not found.
 
 
        character  word*(*), dict*10, tmpwrd*10
        dimension dict(ld)
 
        character*10 capital
        tmpwrd = word
        tmpwrd = capital (tmpwrd)
C
C       Eliminate any underscores in first word
C
        nchr = lastch(tmpwrd)
        do while (index(tmpwrd, '_') .ne. 0)
           i = index (tmpwrd, '_')
           tmpwrd(i:) = tmpwrd(i+1:)
           nchr = nchr - 1
        enddo
C
        do i = 1,ld
           nc1 = index(dict(i),'*') - 1
           nc2 = nchr
           if (nc1 .le. 0) then
              nc1 = lastch (dict(i))
           else
              nc2 = min0(nc1, nc2)
           endif
           if (dict(i)(1:nc1) .eq. tmpwrd(1:nc2)) goto 200
        enddo

C       Not found

        ickdic = 0
c       write(*,111) tmpwrd
c111    format(' *** INFO ***  "', a, '"  not found in dictionary')
        goto 300

C       Found

 200    ickdic = i
 300    return
        end

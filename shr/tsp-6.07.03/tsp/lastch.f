C    %W% %G%
        function lastch (text)
        character text *(*)
C
C       FIND the last non_blank character in the RECORD.
C
        if (len(text) .eq. 0) then
           lastch = 0       ! null record
        else if (ichar(text(1:1)) .eq. 0) then
        else
           l = len (text)
           do 100 i = l,2,-1
           if (text(i:i) .ne. ' ') go to 110
  100      continue
           i = 1
110        lastch = i
        endif
        return
        end

C    @(#)comprs.f	20.3 2/13/96
        character * (*) function comprs (buffer)
C
C       squeeze out blanks in character string BUFFER.
C
        character buffer * (*)
        comprs = buffer
        last = lastch (comprs)
        do 100 i = 1, len (comprs)
   90      if (i .lt. last) then
              if (comprs(i:i) .eq. ' ') then
                 comprs(i:) = comprs(i+1:)
                 last = last - 1
                 go to 90
              endif
           endif
  100   continue
        return
        end

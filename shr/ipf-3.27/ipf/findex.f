C    @(#)findex.f	20.3 2/13/96
        integer function findex (text,string)
C
C       This function searches the first non-blank string in TEXT
C       for STRING.
C
        character text *(*), string *(*)
 
        findex = index (text,string)
        if (findex .gt. 1) then
           if (text(1:findex-1) .ne. ' ') findex = 0
        endif
        return
        end

C    @(#)setercnt.f	20.3 2/13/96
        subroutine setercnt(errnum,val)
C
C       SETERCNT will set the error count array to 0
 
        include 'ipfinc/errorx.inc'
C
        character *1 val
        integer errnum
C
        if ( errnum .eq. 0 ) then
           do 10 i = 1,5
              errcnt(i) = 0
 10        continue
        endif
C
        return
        end

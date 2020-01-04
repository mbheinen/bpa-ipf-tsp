C    @(#)chkerr.f	20.3 2/13/96
        integer function chkerr(etype)
C
C       This function returns the count of how many errors
C       of type "ETYPE" have occured.
C
        character*(*) etype
 
      include 'ipfinc/errorx.inc'
C
        ind=index('IWEFA',etype)
        if(ind.eq.0) then
           chkerr=0
        else
           chkerr=errcnt(ind)
        endif
        return
        end

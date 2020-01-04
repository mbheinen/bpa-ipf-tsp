C    %W% %G%
       subroutine copyd (frm,to,ndbls)
C        -  This subroutine copies 8-byte dbl-prec or cmplx array data 
C
       real*8 frm(ndbls), to(ndbls)
       do 23 i = 1, ndbls
         to(i) = frm(i)
 23    continue
       return
       end

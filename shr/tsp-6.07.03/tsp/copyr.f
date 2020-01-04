C    %W% %G%
       subroutine copyr (frm,to,nreals)
C        -  This subroutine copies 4-byte real or integer array data 
C
       real*4 frm(nreals), to(nreals)
       do 23 i = 1, nreals
         to(i) = frm(i)
 23    continue
       return
       end

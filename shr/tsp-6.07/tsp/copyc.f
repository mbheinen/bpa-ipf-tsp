C    %W% %G%
       subroutine copyc (frm,to,nchars)
C        -  This subroutine copies character array data 1 by 1
C
       character*1 frm(nchars), to(nchars)
       do 23 i = 1, nchars
         to(i) = frm(i)
 23    continue
       return
       end


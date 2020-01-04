C    %W% %G%
       subroutine copy(n,a,b)
C
C      THIS SUBROUTINE COPIES FROM ARRAY A TO ARRAY B
C
       dimension a(n),b(n)
       do 10 i = 1, n/4
       b(i) = a(i)
 10    continue
       return
       end

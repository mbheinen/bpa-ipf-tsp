C    %W% %G%
        subroutine redecq(bm,k,n)
C       -  Old CDC ext core simul routine 
C       -  Sets  BM(I) <-- ECQ(K+I)
C       -    FOR I = 1 to N 
C       -
        common /ecqa/ ecq(80000)
        dimension bm(*)
C
        do 406 i = 1,n
           bm (i) = ecq(k+i) 
 406    continue 
        return
        end

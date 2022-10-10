C    %W% %G%
        subroutine redecs(bm,k,n)

C       CDC extended core simulation routine 
C       Sets BM(I) <-- ECNN(K+I) 
C       for I = 1 to N   if N < MAXECN 
C       
C       Otherwise sets BM(J) <-- ECQ(K+1-MAXECN+J)
C       for J = 1 to N
C       ECQ is the overflow parking lot for ECNN
C       -
        include 'tspinc/params.inc' 
        include 'tspinc/space0.inc' 

        parameter (MAXECQ = 200000)                                 !DEM
        common /ecqa/ ecq(MAXECQ)
        dimension bm(*)
C       -  
        if (n .eq. 0) then
        else if (k .lt. MAXECN) then

C          Retrieve from ECN if address <= MAXECN

           navail = min0 (n, MAXECN-k)
           do i = 1, navail
             bm(i) = ecnn(k+i)
           end do

C          Retrieve overflow from ECQ if address > MAXECN

           nreq = k+1-MAXECN
           do i = navail+1, n
             bm(i) = ecq(i+nreq)
           end do
        else
           nreq = k+1-MAXECN
           do i = 1, n
             bm(i) = ecq(i+nreq)
           end do
        endif                                                           
        return                                                          
        end                                                             

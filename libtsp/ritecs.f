C    %W% %G%
        subroutine ritecs(bm,k,n)   
        dimension bm(*)

C       CDC extended core simulation routine 
C       Sets ECNN(K+I) <-- BM(I)
C       for I = 1 to N   if N < MAXECN 

C       otherwise sets ECQ(K+1-MAXECN+J) <-- BM(J)
C       for J = 1 to N

C       Thus ECQ is the overflow parking lot for ECNN
C       -
        include 'tspinc/params.inc' 
        include 'tspinc/space0.inc' 
        include 'tspinc/prt.inc' 

        parameter (MAXECQ = 200000)                             !DEM
        common /ecqa/ ecq(MAXECQ)
C         
        if (n .eq. 0) then
        else if (k .lt. MAXECN) then

C          Write to ECN if address <= MAXECN

           navail = min0 (n, MAXECN-k)
           do i = 1, navail
             ecnn(k+i) = bm(i)
           end do

C          Write to overflow from ECQ if address > MAXECN

           nreq = k+1-MAXECN
           do i = navail+1, n
             ecq(i+nreq) = bm(i) 
           end do
        else if (k + n .ge. MAXECN + MAXECQ) then
           nreq = k + n - MAXECN - 1
           write (errbuf(1),225) MAXECQ, nreq
  225      format (' RITECQ - Too much ECQ storage requested.',
     1             ' Max: ', i6, ';  Req: ', i6)
           call prterr ('E',1) 
           call erexit 
        else
           nreq = k+1-MAXECN
           do i = 1, n
             ecq(i+nreq) = bm(i) 
           end do
        endif
        return
        end

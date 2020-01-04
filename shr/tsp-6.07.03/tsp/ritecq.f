C    %W% %G%
        subroutine ritecq(bm,k,n)
C       -  Old CDC ext core simul routine 
C       -  Sets  ECQ(K+I) <-- BM(I)
C       -    FOR I = 1 to N 
C       -
      include 'tspinc/prt.inc' 
C       -
        common /ecqa/ ecq(80000)
        dimension bm(*)
C
        nreq = k + n
        if (nreq .gt. 80000 ) then
           write (errbuf(1),225) 80000,nreq 
  225         format (' RITECQ - Too much ECQ storage requested.',
     1          '  Max: ',i6,';  Req: ',i6)
           call prterr ('E',1) 
           call erexit 
           endif
C       -
        do 406 i = 1,n
           ecq(k+i) = bm (i)
 406    continue 
        return
        end

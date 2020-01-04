C    %W% %G%
        subroutine ritecc (bm, k, n)                            
        character*(*) bm(n)                                     
C                                                                       
C       This routine simulates ECS (Extended Core Storage, a CYBER
C       vestige). It transfers character data from BM(1):BM(N) to 
c       ECCS(K):ECCS(K+N).  It is a companion to REDECC.            
C                                                                       
        integer MAXECS
        parameter (MAXECS = 64000)
        character*10 eccs(MAXECS)

        do i=1,n                                                
           eccs(k+i)=bm(i)                                          
        enddo
        return                                                          

        entry redecc(bm,k,n)                            
C                                                                       
C       This routine simulates ECS (Extended Core Storage, a CYBER
C       vestige). It transfers character data from ECCS(K):ECCS(K+N) to 
c       BM(1):BM(N).  It is a companion to RITECC.            
C                                                                       
        do i=1,n                                                
           bm(i)=eccs(k+i)                                           
        enddo
        return                                                          
        end                                                             

C    %W% %G%
      integer function inam (alpha,kb)                                           
      character*8 alpha                                               
C 
c     This function locates bus ALPHA, BASKV(KB) using a binary search
c     in arrays EXNAMEC and BASEKV(KB). 
c
c     Returns:
c        N > 0 is the index to EXNAMEC().
c        N = 0 is an error condition.
c
c     Called from subroutines INPUT1,INPUT2,INPUT3,SHDINP, and ZNOINP.                                                    
C 
      include 'tspinc/params.inc' 
      include 'tspinc/comn34.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/reread.inc' 
C 
C     Check for blank                                                
C 
      inam = 0
      if (alpha .eq. ' ') then
         errflag = 1
      else
         i1 = 1                                                            
         i2 = ntot                                                         
         do while (i1 .le. i2 .and. inam .eq. 0)
            jnam = (i1 + i2)/2                                                
            komp = kompr (exnamc(jnam), alpha, kdum)
            if (komp .eq. 0) komp = ixnamn(jnam) - kb
            if (komp .eq. 0) then
               inam = jnam                                                       
            else if (komp .lt. 0) then
               i1 = jnam + 1
            else
               i2 = jnam - 1                                                     
            endif
         enddo
         if (inam .eq. 0) then
            if (kb .ne. 0) then                                                 
               base = basekv(kb)                                                
            else                                                              
               base = 0.0                                                     
            endif                                                             
            write (errbuf(1), 100) buffer                                       
  100       format('0', a)                                                  
            write (errbuf(2), 110) alpha, base                                
  110       format(' This record ignored because bus (', a8, f7.1, 
     &             ') is not in POWERFLOW hase case data.')             
            if (errflag .eq. 0) then
               call prterr ('W', 2)                                               
            else
               call prterr ('E', 2)                                               
               iabort = 1                                                          
            endif
            errflag = 1
         endif                                                              
      endif
      return                                                            
      end                                                               

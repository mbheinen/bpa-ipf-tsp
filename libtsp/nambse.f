C    %W% %G%
       function nambse(base)                                         

C       THIS FUNCTION IS USED TO COMPARE THE BASE KVA RATING            
C       FOR A PARTICULAR BUS TO THOSE UNIQUE BASE KVA RATINGS           
C       IN THE BASEKV(63) TABLE.  THE ENTRIES IN BASEKV ARE             
C       IN ASCENDING ORDER, HENCE A BINARY SEARCH IS USED.              
C       IF AGREEMENT IS MADE WITH A BASE KVA RATING, AN INTEGER         
C       IS RETURNED INDICATING THE POSITION IN THE BASEKV               
C       TABLE.  IF NO AGREEMENT CAN BE MADE AN INTEGER VALUE OF         
C       ZERO IS RETURNED, AN ERROR MESSAGE PRINTED, AND AN              
C       ABORT FLAG SET.                                                 
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/reread.inc' 
c   
c     -     Begin     Begin     Begin     Begin     Begin     Begin
C     CHECK FOR BLANK BASE                                              
      if(base.gt.0.0)go to 5                                            
      if(sign(1.0,base).lt.0.0)go to 45                                 
C * * *                                                                 
C * * * DO A BINARY SEARCH AMONG NUMERICALLY ORDERED BASES              
C * * *                                                                 
    5 i1=0                                                              
      i2 = ibxyz                                                        
      if( i2 .ne. 1) go to 10                                           
      i2 = 2                                                            
   10 nambse = (i1 + i2)/2                                          
      if(nambse .eq. 0) go to 20                                    
      if(basekv(nambse) - base) 20, 80, 30                          
   20 i1 = nambse + 1                                               
      go to 40                                                          
   30 i2 = nambse - 1                                               
   40 if(i2.ge.i1)go to 10                                              
   45 write (errbuf(1),51) base                                         
   51 format('NAMBSE - The card below was ignored because the base KV ',
     1  ' (', f6.1, ') was not used in the base power flow.')           !dem
      write (errbuf(2),50) buffer                                       
   50 format(1h0, a  )                                                  
c  50 FORMAT('0', A )                                                  
c  51 FORMAT(' THE CARD WAS IGNORED BECAUSE THE BASE (',               
c    1          F7.1,') WAS NOT USED IN THE POWER FLOW PROGRAM.')       
      write (errbuf(1),50) buffer                                       
      call prterr ('E',2)                                               !dem
      iabort=1                                                          
   70 nambse = 0                                                    
   80 return                                                            
      end                                                               

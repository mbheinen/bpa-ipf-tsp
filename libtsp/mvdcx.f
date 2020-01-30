C    %W% %G%
       subroutine mvdcx(ibus,iopt,iadr)                                 
C * * *                                                                 
C * * * THIS SUBROUTINE STORES THE REQUIRED DC OUTPUT QUANTITIES        
C * * * FOR THE SPECIAL PLOTS OUTPUT.  IT IS CALLED BY NDCOUT.          
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/comn56.inc'                                              !dem
      include 'tspinc/mvn.inc' 
*END  MVN                                                               
      include 'tspinc/link56.inc' 
      include 'tspinc/room.inc' 
      do 200 n = 1, 2*mvkt                                              
      if(mvgen(n) .ne. ibus) go to 200                                  
      if(mvcde(n) .ne. 'D') go to 200                                   
      if(mvopt(n) .ne. iopt) go to 200                                  
      mvadr(n) = madr1                                                  
      do 100 l = 1,icount+2                                             
  100 vspce(madr1+l) = worksp(iadr+l)                                   
      madr1 = madr1+icount+2                                            
  200 continue                                                          
      return                                                            
      end                                                               

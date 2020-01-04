C    %W% %G%
      subroutine rsint                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE INITIALIZES THE DATA TABLES FOR THE             
C * * * UNDERFREQUENCY CF-1 LOAD SHEDDDING RELAY.  IT IS CALLED         
C * * * BY INITL2.                                                      
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/rscom.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/busnum.inc' 
      include 'tspinc/relays.inc' 
      do 1000 i=1,nufreq                                                
      nbus = irsbno(i)                                                  
      nbus = indx2n(nbus)                                               
      irsbno(i) = nbus                                                  
      eold = eyr(nbus)                                                  
      fold = eyi(nbus)                                                  
      rsvint(i) = sqrt(eold*eold + fold*fold )                          
      rseyro(i) = eold                                                  
      rseyio(i) = fold                                                  
      rsx1o(i) = rsfeq1(i)                                              
      rsx1n(i) = rsfeq1(i)                                              
      rsfeqo(i) = 0.0                                                   
 1000 continue                                                          
      return                                                            
      end                                                               

C    %W% %G%
      subroutine gdifmd                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE MODIFIES THE EXISTING REQUESTED OUTPUT          
C * * * TABLES TO INCLUDE OPTIONS REQUESTED ON THE 'GD',                
C * * * GENERATOR DIFFERENCE CARDS. IT IS CALLED BY NOUT1.              
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/gdif.inc' 
      include 'tspinc/igend.inc' 
*END  IGEND                                                             
      include 'tspinc/mvnout.inc' 
*END  MVNOUT                                                            
      include 'tspinc/out512.inc' 
*END  OUT512                                                            
C * * *                                                                 
C * * * PROCESSING GENERATOR OUTPUT REQUESTS FROM MV CARD               
C * * *                                                                 
      do 1000 n = 1, idifkt                                             
      k1 = k1gdif(n)                                                    
      k2 = k2gdif(n)                                                    
      ktemp = k1                                                        
 100  if(igdat .eq. 1)go to 900                                         
      do 800 l = 1,igdat-1                                              
      if(igend(1,l) .ne. ktemp) go to 800                               
      if(igdifc(1,n) .ne. 0)then                                        
         iopt1 = igend(2,l)                                             
         if(iopt1 .le. 3) iopt1 = 3                                     
         if(iopt1 .eq. 4 .or. iopt1 .eq. 5) iopt1 = iopt1 +2            
         igend(2,l) = iopt1                                             
      endif                                                             
      if(igdifc(2,n) .ne. 0)then                                        
         iopt1 = igend(3,l)                                             
         if(iopt1 .le. 3) iopt1 = 3                                     
         if(iopt1 .eq. 4 .or. iopt1 .eq. 5) iopt1 = iopt1 +2            
         igend(3,l) = iopt1                                             
      endif                                                             
      if(igdifc(3,n) .ne. 0)then                                        
         iopt1 = igend(8,l)                                             
         if(iopt1 .le. 3) iopt1 = 3                                     
         if(iopt1 .eq. 4 .or. iopt1 .eq. 5) iopt1 = iopt1 +2            
         igend(8,l) = iopt1                                             
      endif                                                             
      go to 950                                                         
  800 continue                                                          
  900 igswt = igswt + 1                                                 
      igend(1,igdat) = ktemp                                            
      if(igdifc(1,idifkt) .ne. 0)igend(2,igdat) = 3                     
      if(igdifc(2,idifkt) .ne. 0)igend(3,igdat) = 3                     
      if(igdifc(3,idifkt) .ne. 0)igend(8,igdat) = 3                     
      igdat = igdat + 1                                                 
      igdatt = igdatt + 1                                               
  950 if(ktemp .eq. k1)then                                             
         ktemp = k2                                                     
         go to 100                                                      
      endif                                                             
 1000 continue                                                          
      return                                                            
      end                                                               

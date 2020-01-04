C    %W% %G%
      subroutine mvmod                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE MODIFIES THE EXISTING REQUESTED OUTPUT          
C * * * TABLES TO INCLUDE OPTIONS REQUESTED ON THE MV CARDS.            
C * * * IT IS CALLED BY NOUT1.                                          
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/indx2n.inc' 
      include 'tspinc/out512.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/igend.inc' 
      include 'tspinc/ibusd.inc' 
      include 'tspinc/ilind.inc' 
      include 'tspinc/idctbl.inc' 
      include 'tspinc/dc.inc' 
      include 'tspinc/mvn.inc' 
      include 'tspinc/mvnout.inc' 
*END  MVNOUT                                                            
C * * *                                                                 
C * * * PROCESSING GENERATOR OUTPUT REQUESTS FROM MV CARD               
C * * *                                                                 
      if(mvgkt .eq. 0) go to 1100                                       
      do 1000 n = 1, 2*mvkt                                             
      if(mvcde(n) .ne. 'G')go to 1000                                   
      k1 = mvgen(n)                                                     
      if(igdat .eq. 1)go to 900                                         
      do 800 l = 1,igdat-1                                              
      if(igend(1,l) .ne. k1) go to 800                                  
      iopt = mvopt(n) + 1                                               
      iopt1 = igend(iopt,l)                                             
      if(iopt1 .le. 3) iopt1 = 3                                        
      if(iopt1 .eq. 4 .or. iopt1 .eq. 5) iopt1 = iopt1 +2               
      if(iopt1 .le. 10)iopt1 = iopt1 + 10                               
      igend(iopt,l) = iopt1                                             
      go to 1000                                                        
  800 continue                                                          
  900 igswt = igswt + 1                                                 
      igend(1,igdat) = k1                                               
      iopt = mvopt(n) + 1                                               
      igend(iopt,igdat) = 13                                            
      igdat = igdat + 1                                                 
      igdatt = igdatt + 1                                               
 1000 continue                                                          
C * * *                                                                 
C * * * PROCESSING BUS OUTPUT REQUESTS FROM MV CARD                     
C * * *                                                                 
 1100 if(mvbkt .eq. 0) go to 2100                                       
      do 2000 n = 1, 2*mvkt                                             
      if(mvcde(n) .ne. 'B')go to 2000                                   
      k1 = mvgen(n)                                                     
      if(ibdat .eq. 1)go to 1900                                        
      do 1800 l = 1,ibdat-1                                             
      if(ibusd(1,l) .ne. k1) go to 1800                                 
      iopt = mvopt(n)+1                                                 
      iopt1 = ibusd(iopt,l)                                             
      if(iopt1 .le. 1) iopt1 = iopt1 + 2                                
      if(iopt1 .eq. 4 .or. iopt1 .eq. 5) iopt1 = iopt1 +2               
      if(iopt1 .le. 10) iopt1 = iopt1 + 10                              
      ibusd(iopt,l) = iopt1                                             
      go to 2000                                                        
 1800 continue                                                          
 1900 ibswt = ibswt + 1                                                 
      ibusd(1,ibdat) = k1                                               
      iopt = mvopt(n) + 1                                               
      ibusd(iopt,ibdat) = 12                                            
      ibdat = ibdat + 1                                                 
      ibdatt = ibdatt + 1                                               
 2000 continue                                                          
C * * *                                                                 
C * * * PROCESSING LINE OUTPUT REQUESTS FROM MV CARD                    
C * * *                                                                 
 2100 if(mvlkt .eq. 0) go to 3100                                       
      do 3000 n= 1,2*mvkt                                               
      if(mvcde(n) .ne. 'L')go to 3000                                   
      k1 = mvgen(n)                                                     
      k2 = mvgen2(n)                                                    
      if(ildat .eq. 1) go to 2900                                       
      do 2800 l=1,ildat-1                                               
      if(k1 .ne. ilindn(1,1,l))go to 2800                               
      if(k2 .ne. ilindn(1,2,l)) go to 2800                              
      if(mvid(n) .ne. ilindc(1,l))go to 2800                            
      iopt = mvopt(n)                                                   
      iy = 1                                                            
      if(iopt .gt. 4) iy = 2                                            
      if(iopt .eq. 3 .or. iopt .eq. 4 .or. iopt .eq. 8)ix = 7           
      if(iopt .eq. 2 .or. iopt .eq. 6 .or. iopt .eq. 7)ix = 6           
      if(iopt .eq. 1 .or. iopt .eq. 5)ix = 5                            
      iopt1 = ilindn(ix,iy,l)                                           
      if(iopt1 .le. 1)iopt1 = iopt1 + 2                                 
      if (iopt1 .eq. 4 .or. iopt1 .eq. 5) iopt1 = iopt1 + 2             !dem
c     IF(IOPT1 .EQ. 4 .OR. IOPT1 .EQ. 5)IOPT1 = IOP1 + 2                
      ilindn(ix,iy,l) = iopt1                                           
      mvgen(n) = l                                                      
      go to 3000                                                        
 2800 continue                                                          
 2900 ilswt = ilswt +1                                                  
      iopt = mvopt(n)                                                   
      iy = 1                                                            
      if(iopt .gt. 4) iy = 2                                            
      if(iopt .eq. 3 .or. iopt .eq. 4 .or. iopt .eq. 8)ix = 7           
      if(iopt .eq. 2 .or. iopt .eq. 6 .or. iopt .eq. 7)ix = 6           
      if(iopt .eq. 1 .or. iopt .eq. 5)ix = 5                            
      ilindn(ix,iy,ildat) = 2                                           
      mvgen(n) = ildat                                                  
      ilindn(1,1,ildat) = k1                                            
      ilindn(1,2,ildat) = k2                                            
      ilindc(1,ildat) = mvid(n)                                         
      ilindc(2,ildat) = mvnm11(n)                                       
      ilindc(3,ildat) = mvnm12(n)                                       
      ildat = ildat+1                                                   
      ildatt = ildatt + 1                                               
 3000 continue                                                          
 3100 if(mvdkt .eq. 0)return                                            
      do 3500 n = 1,2*mvkt                                              
      if(mvcde(n) .ne. 'D')go to 3500                                   
      k1 = mvgen(n)                                                     
      if(iddat .eq. 1) go to 3300                                       
      do 3200 l = 1,iddat-1                                             
      if(k1 .ne. idctln(2,1,l)) go to 3150                              
      iopt = mvopt(n)                                                   
      if(iopt .le. 5)then                                               
         ix = 2*iopt - 1                                                
         iy = 3                                                         
      endif                                                             
      if(iopt .gt. 5)then                                               
         ix = 2*iopt - 9                                                
         iy = 2                                                         
      endif                                                             
      iopt1 = idctln(ix,iy,l)                                           
      if(iopt1 .le. 1)iopt1 = iopt1 + 2                                 
      if (iopt1 .eq. 4 .or. iopt1 .eq. 5) iopt1 = iopt1 + 2             !dem
c     IF(IOPT1 .EQ. 4 .OR. IOPT1 .EQ. 5)IOPT1 = IOP1 + 2                
      idctln(ix,iy,l) = iopt1+10                                        
      mvgen(n) = indx2n(mvgen(n))                                       
      go to 3500                                                        
 3150 if(k1 .ne. idctln(2,2,l)) go to 3200                              
      iopt = mvopt(n)                                                   
      if(iopt .le. 5)then                                               
         ix = 2*iopt                                                    
         iy = 3                                                         
      endif                                                             
      if(iopt .gt. 5)then                                               
         ix = 2*iopt - 8                                                
         iy = 2                                                         
      endif                                                             
      iopt1 = idctln(ix,iy,l)                                           
      if(iopt1 .le. 1)iopt1 = iopt1 + 2                                 
      if (iopt1 .eq. 4 .or. iopt1 .eq. 5) iopt1 = iopt1 + 2             !dem
c     IF(IOPT1 .EQ. 4 .OR. IOPT1 .EQ. 5)IOPT1 = IOP1 + 2                
      idctln(ix,iy,l) = iopt1+10                                        
      mvgen(n) = indx2n(mvgen(n))                                       
      go to 3500                                                        
 3200 continue                                                          
 3300 iopt = mvopt(n)                                                   
      if(iopt .le. 5)then                                               
         ix = 2*iopt-1                                                  
         iy = 3                                                         
      endif                                                             
      if(iopt .gt. 5)then                                               
         ix = 2*iopt-9                                                  
         iy = 2                                                         
      endif                                                             
      idctln(2,1,iddat) = k1                                            
      mvgen(n) = indx2n(mvgen(n))                                       
      idctln(ix,iy,iddat) = 13                                          
      idctlc(1,iddat) = mvnm11(n)                                       
      idctln(1,1,iddat) = nambse(pkv11(n))                              
      idswt = 1                                                         
      iddat = iddat+1                                                   
 3500 continue                                                          
      return                                                            
      end                                                               

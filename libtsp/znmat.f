C    %W% %G%
      subroutine znmat(itrr)                                            
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES THE EQUIVALENT ADMITTANCE            
C * * * FOR THE ZINC OXIDE ARRESTOR MODEL                               
C * * *                                                                 
      include 'tspinc/znox.inc' 
      include 'tspinc/znox2.inc' 
      include 'tspinc/vy1.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/ecsind.inc' 
      character*8 ibus,jbus                                             
      eii = eyi(iznbus(itrr))                                           
      eri = eyr(iznbus(itrr))                                           
      eij = eyi(jznbus(itrr))                                           
      erj = eyr(jznbus(itrr))                                           
      jznsw = 1                                                         
      if(zng14(itrr) .eq. 0.0 .and. znb14(itrr) .eq. 0.0)jznsw = 2      
      if(zng23(itrr) .eq. 0.0 .and. znb23(itrr) .eq. 0.0)jznsw = 3      
C * * *                                                                 
C * * * ZERO ALL ENTRIES IN TEMPORARY Y MATRIX                          
C * * *                                                                 
      do 120 i = 1,10                                                   
      do 100 j = 1,10                                                   
      gofd(i,j) = 0.0                                                   
 100  bofd(i,j) = 0.0                                                   
 120  continue                                                          
      do 130 i = 1,4                                                    
      eg(i) = 0.0                                                       
 130  fg(i) = 0.0                                                       
C * * *                                                                 
C * * *  INITALIZE UPPER Y MATRIX                                       
C * * *  JZNSW = 1 MEANS LINE SECTIONS TO THE LEFT AND RIGHT OF         
C * * *  THE VARIABLE Y                                                 
C * * *                                                                 
      if(jznsw .eq. 1) then                                             
         gofd(1,1) = zng12(itrr)+zng14(itrr)+zng11l(itrr)+zng44r(itrr)  
         bofd(1,1) = znb12(itrr)+znb14(itrr)+znb11l(itrr)+znb44r(itrr)  
         gofd(2,2) = zng12(itrr)+zng23(itrr)+zng11r(itrr)+zng33l(itrr)  
         bofd(2,2) = znb12(itrr)+znb23(itrr)+znb11r(itrr)+znb33l(itrr)  
         gofd(3,3) = zng23(itrr)+zng33r(itrr)                           
         bofd(3,3) = znb23(itrr)+znb33r(itrr)                           
         gofd(4,4) = zng14(itrr) + zng44l(itrr)                         
         bofd(4,4) = znb14(itrr) + znb44l(itrr)                         
         gofd(1,2) = -zng12(itrr)                                       
         bofd(1,2) = -znb12(itrr)                                       
         gofd(1,4) = -zng14(itrr)                                       
         bofd(1,4) = -znb14(itrr)                                       
         gofd(2,3) = -zng23(itrr)                                       
         bofd(2,3) = -znb23(itrr)                                       
         eg(4) = eri                                                    
         fg(4) = eii                                                    
         eg(3) = erj                                                    
         fg(3) = eij                                                    
         nbus = 4                                                       
      endif                                                             
C * * *                                                                 
C * * *  JZNSW = 2 MEANS NO LINE SECTION TO THE LEFT OF THE VARIABLE Y  
C * * *                                                                 
      if(jznsw .eq. 2)then                                              
         gofd(1,1)=zng12(itrr)+zng23(itrr)+zng11r(itrr)+zng33l(itrr)    
         bofd(1,1)=znb12(itrr)+znb23(itrr)+znb11r(itrr)+znb33l(itrr)    
         gofd(1,2) = -zng23(itrr)                                       
         bofd(1,2) = -znb23(itrr)                                       
         gofd(1,3) = -zng12(itrr)                                       
         bofd(1,3) = -znb12(itrr)                                       
         gofd(2,2) = zng23(itrr) + zng33r(itrr)                         
         bofd(2,2) = znb23(itrr) +  znb33r(itrr)                        
         gofd(3,3) = zng12(itrr) + zng11l(itrr)                         
         bofd(3,3) = znb12(itrr) + znb11l(itrr)                         
         eg(3) = eri                                                    
         fg(3) = eii                                                    
         eg(2) = erj                                                    
         fg(2) = eij                                                    
         nbus = 3                                                       
      endif                                                             
C * * *                                                                 
C * * *  JZNSW = 3 MEANS NO LINE SECTION TO THE RIGHT OF THE VARIABLE Y 
C * * *                                                                 
      if(jznsw .eq. 3)then                                              
         gofd(1,1)=zng14(itrr)+zng12(itrr)+zng44r(itrr)+zng11l(itrr)    
         bofd(1,1)=znb14(itrr)+znb12(itrr)+znb44r(itrr)+znb11l(itrr)    
         gofd(1,2) = -zng12(itrr)                                       
         bofd(1,2) = -znb12(itrr)                                       
         gofd(1,3) = -zng14(itrr)                                       
         bofd(1,3) = -znb14(itrr)                                       
         gofd(2,2) = zng12(itrr) +  zng11r(itrr)                        
         bofd(2,2) = znb12(itrr) +  znb11r(itrr)                        
         gofd(3,3) = zng14(itrr) + zng44l(itrr)                         
         bofd(3,3) = znb14(itrr) + znb44l(itrr)                         
         eg(3) = eri                                                    
         fg(3) = eii                                                    
         eg(2) = erj                                                    
         fg(2) = eij                                                    
         nbus = 3                                                       
      endif                                                             
 400  nbusm2 = nbus-2                                                   
      nbusm1 = nbus-1                                                   
C * * *                                                                 
C * * * FACTORIZE Y MATRIX                                              
C * * *                                                                 
      i = 0                                                             
  420 i = i + 1                                                         
      if(i .gt. nbus) go to 520                                         
      if(i.eq.1) go to 480                                              
      im = i - 1                                                        
      do 460 j = 1, im                                                  
      if(i .gt. nbusm2 .and. j .gt. nbusm2) go to 420                   
      if(gofd(j,i).eq.0.0 .and. bofd(j,i) .eq.0.0) go to 460            
C                                                                       
C     CHECK FOR ZERO DIAGONAL ELEMENT                                   
C                                                                       
      if(gofd(j,j) .eq. 0.0 .and. bofd(j,j) .eq. 0.0)then               
         rijc = 0.0                                                     
         xijc = 0.0                                                     
      else                                                              
         rijc = gofd(j,i)*gofd(j,j) - bofd(j,i)*bofd(j,j)               
         xijc = gofd(j,i)*bofd(j,j) + bofd(j,i)*gofd(j,j)               
      endif                                                             
      do 440 k=j+1,nbus                                                 
      gofd(i,k) = gofd(i,k) - rijc*gofd(j,k) + xijc*bofd(j,k)           
  440 bofd(i,k) = bofd(i,k) - rijc*bofd(j,k) - xijc*gofd(j,k)           
  460 continue                                                          
C                                                                       
C     CHECK FOR ZERO DIAGONAL ELEMENT 'BUS WITH ALL LINES AND TERMINALS 
C     DISCONNECTED WOULD CAUSE THIS CONDITION'                          
C                                                                       
 480  if(i .gt. nbusm2) go to 420                                       
      if(gofd(i,i).eq. 0.0 .and. bofd(i,i) .eq. 0.0)then                
         rrdd(i) = 0.0                                                  
         rxdd(i) = 0.0                                                  
      else                                                              
         recip=1.0/(gofd(i,i)*gofd(i,i) + bofd(i,i)*bofd(i,i))          
         rrdd(i) = gofd(i,i)*recip                                      
         rxdd(i) = -bofd(i,i)*recip                                     
      endif                                                             
      i1 = i+1                                                          
      do 500 l=i1,nbus                                                  
      g  = gofd(i,l)* rrdd(i) - bofd(i,l)*rxdd(i)                       
      b  = bofd(i,l)*rrdd(i)  + gofd(i,l)*rxdd(i)                       
      gofd(i,l) = g                                                     
 500  bofd(i,l) = b                                                     
      go to 420                                                         
C                                                                       
C BACKSUBSTITUTION                                                      
C                                                                       
 520  do 540 i = 1,nbusm2                                               
      nbmi = nbusm2 - i +1                                              
      k = nbmi +1                                                       
      do 540 j=k,nbus                                                   
      eg(nbmi)= eg(nbmi) - gofd(nbmi,j)*eg(j)                           
     1          +bofd(nbmi,j)*fg(j)                                     
  540 fg(nbmi)= fg(nbmi) - gofd(nbmi,j)*fg(j)                           
     1          -bofd(nbmi,j)*eg(j)                                     
C      WRITE(6,550)EG(1),FG(1),EG(2),FG(2),EG(3),FG(3)                  
 550  format(1h0,5x,' EI AND FI',6(2x,f10.6))                           
      if(jznsw .eq. 2)then                                              
         eg(2) = eg(1)                                                  
         fg(2) = fg(1)                                                  
         eg(1) = eg(3)                                                  
         fg(1) = fg(3)                                                  
      endif                                                             
C * * *                                                                 
C * * * UPDATE TOTAL LINE EQUIVALENT TABLES                             
C * * *                                                                 
C * * *                                                                 
C * * * UPDATE TOTAL LINE EQUIVALENT TABLES                             
C * * *                                                                 
      zngij(itrr) = -gofd(nbusm1,nbus)                                  
      znbij(itrr) = -bofd(nbusm1,nbus)                                  
      zngjj(itrr) = gofd(nbusm1,nbusm1) - zngij(itrr)                   
      znbjj(itrr) = bofd(nbusm1,nbusm1) - znbij(itrr)                   
      zngii(itrr) = gofd(nbus,nbus) - zngij(itrr)                       
      znbii(itrr) = bofd(nbus,nbus) - znbij(itrr)                       
      return                                                            
      end                                                               

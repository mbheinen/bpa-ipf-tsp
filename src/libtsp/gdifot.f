C    %W% %G%
      subroutine gdifot                                                 
C * * *                                                                 
C * * *    THIS SUBROUTINE CALCULATES AND OUTPUTS THE                   
C * * *    GENERATOR DIFFERENCES QUANTITIES. IT IS CALLED               
C * * *    BY NOUT2.                                                    
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/mvn.inc' 
      include 'tspinc/gdif.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      dimension vmax(3),vmin(3)                                         
      do 200 l= 1,idifkt                                                
      do 50 m = 1,3                                                     
      vmax(m) = 0.0                                                     
  50  vmin(m) = 0.0                                                     
      do 100 n = 1, icount                                              
      if(gdifan(n,l) .gt. vmax(1))vmax(1) = gdifan(n,l)                 
      if(gdifan(n,l) .lt. vmin(1))vmin(1) = gdifan(n,l)                 
      if(gdifsp(n,l) .gt. vmax(2))vmax(2) = gdifsp(n,l)                 
      if(gdifsp(n,l) .lt. vmin(2))vmin(2) = gdifsp(n,l)                 
      if(gdifpw(n,l) .gt. vmax(3))vmax(3) = gdifpw(n,l)                 
  100 if(gdifpw(n,l) .lt. vmin(3))vmin(3) = gdifpw(n,l)                 
      gdifan(icount+1,l) = vmax(1)                                      
      gdifan(icount+2,l) = vmin(1)                                      
      gdifsp(icount+1,l) = vmax(2)                                      
      gdifsp(icount+2,l) = vmin(2)                                      
      gdifpw(icount+1,l) = vmax(3)                                      
      gdifpw(icount+2,l) = vmin(3)                                      
  200 continue                                                          
      if(mvkt .eq. 0)go to 2000                                         
      do 1000 n = 1, 2*mvkt                                             
      if(mvcde(n) .ne. 'Y') go to 1000                                  
      do 980 l = 1,idifkt                                               
      if(mvgen(n) .ne. k1gdif(l)) go to 980                             
      mvgen2(n) = k2gdif(l)                                             
      if(mvopt(n) .eq. 1) then                                          
         mvadr(n) = madr1                                               
         do 920 m = 1,icoun2                                            
  920    vspce(madr1+m) = gdifan(m,l)                                   
         madr1 = madr1+icoun2                                           
      endif                                                             
      if(mvopt(n) .eq. 2) then                                          
         mvadr(n) = madr1                                               
         do 940 m = 1,icoun2                                            
  940    vspce(madr1+m) = gdifsp(m,l)                                   
         madr1 = madr1+icoun2                                           
      endif                                                             
      if(mvopt(n) .eq. 7) then                                          
         mvadr(n) = madr1                                               
         do 960 m = 1,icoun2                                            
  960    vspce(madr1+m) = gdifpw(m,l)                                   
         madr1 = madr1+icoun2                                           
      endif                                                             
  980 continue                                                          
 1000 continue                                                          
 2000 return                                                            
      end                                                               

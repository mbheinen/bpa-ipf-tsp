C    %W% %G%
      subroutine pieqiv (brnch,gk1,bk1,gk2,bk2,g12,b12,g21,b21)         
C ***                                                                   
C *** This subroutine computes the 2-port Y-matrix from the PF          
C *** branch addressed as BRNCH. The pi-equivalents are extracted       
C *** from the Y-matrix.                                                
C ***                                                                   
C *** Note the unusual method in which integer equivalents are          
C *** obtained from BRNCH: (RK1,K1), (RK2,K2), (RLIST,LIST), and        
C *** (RLTYPE,LTYPE)                                                    
C *** PIEQIV IS CALLED BY BRNCHZ.                                       
      include 'tspinc/params.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/link56.inc' 
      dimension brnch(18)                                               
      complex y(2,2), cx                                                
      equivalence (rk1,k1), (rk2,k2), (rltype,ltype), (rlist,list)      
      rltype = brnch(1)                                                 
      rk1 = brnch(2)                                                    
      rk2 = brnch(12)                                                   
      rlist = brnch(17)                                                 
      go to (1700,1700,1000,1700,1000,1000,1700,1000)ltype              
C ***                                                                   
C *** Compute transfer impedance                                        
C ***                                                                   
 1000 r = brnch(5)                                                      
      x = brnch(6)                                                      
      z = r * *2 + x * *2                                               
      go to (1700,1900,1200,1900,1400,1500,1900,1300) ltype             
C ***                                                                   
C *** " L " branch                                                      
C ***                                                                   
 1200 y(1,2) = -cmplx (r/z, -x/z)                                       
      y(2,1) = y(1,2)                                                   
      y(1,1) = cmplx (brnch(7),brnch(8)) - y(1,2)                       
      y(2,2) = y(1,1)                                                   
      go to 1800                                                        
C ***                                                                   
C *** " E " branch                                                      
C ***                                                                   
 1300 y(1,2)= -cmplx (r/z, -x/z)                                        
      y(2,1) = y(1,2)                                                   
      y(1,1) = cmplx(brnch(7),brnch(8)) - y(1,2)                        
      y(2,2) = cmplx(brnch(9),brnch(10)) - y(2,1)                       
      go to 1800                                                        
C ***                                                                   
C *** " T " branch                                                      
C ***                                                                   
 1400 kb1 = ixnamn(k1)                                                  
      kb2 = ixnamn(k2)                                                  
      tk = brnch(9) / basekv(kb1)                                       
      tm = brnch(10) / basekv(kb2)                                      
      tkm = 1.0 / (tk*tm)                                               
      y(1,1) = cmplx(0.5*tkm*brnch(7),-0.5*tkm*abs(brnch(8)))           
      y(2,2) = y(1,1)                                                   
      y(1,2) = -cmplx(r/(z*tk*tm),-x/(z*tk*tm))                         
      y(2,1) = y(1,2)                                                   
      y(1,1) = y(1,1) + cmplx (r/(z*tk*tk),-x/(z*tk*tk))                
      y(2,2) = y(2,2) + cmplx (r/(z*tm*tm), -x/(z*tm*tm))               
      go to 1800                                                        
C ***                                                                   
C *** " TP " branch                                                     
C ***                                                                   
 1500 kb1 = ixnamn(k1)                                                  
      kb2 = ixnamn(k2)                                                  
      tk = brnch(9)                                                     
      if (list .eq. 0) tm = brnch(10) / basekv(kb1)                     
      if (list .eq. 1) tm = brnch(10) / basekv(kb2)                     
      if (tm .eq. 0.0) tm = 1.0                                         
      angle = 0.0174532925 * tk                                         
      y(1,2) = cmplx(0.5*brnch(7),-0.5*abs(brnch(8)))                   
      y(1,1) = y(1,2) + cmplx(r/z,-x/z)                                 
      y(2,2) = y(1,2) + cmplx(r/z,-x/z) / cmplx(tm**2,0.0)              
      y(1,2) = -cmplx(cos(angle),sin(angle)) *                          
     1   cmplx(r/(z*tm),-x/(z*tm))                                      
      y(2,1) = -cmplx(cos(angle),-sin(angle)) *                         
     1   cmplx(r/(z*tm),-x/(z*tm))                                      
      if (list .ne. 0) then                                             
         cx = y(2,2)                                                    
         y(2,2) = y(1,1)                                                
         y(1,1) = cx                                                    
      endif                                                             
      go to 1800                                                        
C ***                                                                   
C *** " L "  EQUIVALENT BRANCH                                          
C ***                                                                   
 1700 gk1 = brnch(4) + brnch(6)                                         
      bk1 = brnch(5) + brnch(7)                                         
      g12 = -brnch(6)                                                   
      b12 = -brnch(7)                                                   
      g21 = -brnch(8)                                                   
      b21 = -brnch(9)                                                   
      gk2 = brnch(10) + brnch(8)                                        
      bk2 = brnch(11) + brnch(9)                                        
      go to 1900                                                        
 1800 continue                                                          
      gk1 = real (y(1,1)+y(1,2))                                        
      bk1 = aimag (y(1,1)+y(1,2))                                       
      gk2 = real (y(2,2)+y(2,1))                                        
      bk2 = aimag (y(2,2)+y(2,1))                                       
      g12 = -real (y(1,2))                                              
      b12 = -aimag (y(1,2))                                             
      g21 = -real (y(2,1))                                              
      b21 = -aimag (y(2,1))                                             
 1900 return                                                            
      end                                                               
